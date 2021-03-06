import java.io.BufferedWriter
import java.io.ByteArrayInputStream
import java.io.FileInputStream
import java.io.FileWriter
import java.io.InputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.net.InetAddress
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.CountDownLatch
import java.util.concurrent.CyclicBarrier
import java.util.concurrent.Executors
import java.util.concurrent.LinkedBlockingQueue


import scala.annotation.tailrec
import scala.concurrent._
//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.control.Exception._

import scala.reflect.runtime.{universe => ru}

def execTime[A](v: => A) {
    val st = System.currentTimeMillis
    v
    println(f"exec time = ${System.currentTimeMillis - st}%d")
}

trait BinaryStreamParser {
    val RECORD_SIZE: Int

    def bin2hex(bs: Array[Byte]): String = bs.map(b => f"$b%02X").mkString
    def bin2Int(bs: Array[Byte]): Int = {
        val byteBuffer = ByteBuffer.allocate(4)
        byteBuffer.position(2)
        byteBuffer.put(bs).rewind
        byteBuffer.getInt
    }
    def bin2Long(bs: Array[Byte]): Long = {
        val byteBuffer = ByteBuffer.allocate(8)
        byteBuffer.position(4)
        byteBuffer.put(bs).rewind
        byteBuffer.getLong
    }

    type BPFType = String with Int with Long with InetAddress

    /* BytesParseFunction */
    case class BPF[A >: BPFType : ru.TypeTag](n: Int, func: A => String) {
//        def tpe: ru.Type = ru.typeOf[A]

        val tpe_String = ru.typeOf[String] 
        val tpe_Int    = ru.typeOf[Int] 
        val tpe_Long   = ru.typeOf[Long] 
        val tpe_InetAddress = ru.typeOf[InetAddress] 

        def arg(bs: Array[Byte]): A = ru.typeOf[A] match {
            case t if t =:= tpe_String      => bin2hex(bs).asInstanceOf[A]
            case t if t =:= tpe_Int         => bin2Int(bs).asInstanceOf[A]
            case t if t =:= tpe_Long        => bin2Long(bs).asInstanceOf[A]
            case t if t =:= tpe_InetAddress => InetAddress.getByAddress(bs).asInstanceOf[A]
        }
    }

    val bpfList: List[BPF[_]]

    def parseBinaryStream(inStream: InputStream, pWriter: PrintWriter) {
        ultimately{
            inStream.close
        } {
            trait Ret { val n: Long }
            object Ret {
                def unapply(arg: Any):Option[Long] = arg match {
                    case ret: Ret => Some(ret.n)
                    case _ => None
                }
            }

            var countDownLatch =  new CountDownLatch(1)

            /* ------------------------------------------------------------ */

            val parseExecutionContext: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(16))

            def startBinaryParserFuture(previousFuture: Future[String], cnt: Long, buff: Array[Byte]): Future[String] = {
                implicit val execContext = parseExecutionContext
                val parseFuture = previousFuture.flatMap(s =>
                    Future {
                        @tailrec
                        def parseBinary(srcBytes: Array[Byte], dstStr: String)(bpfl: List[BPF[_]]): String
                        = {
                            bpfl match {
                                case Nil => dstStr
                                case BPF(n, _) :: _ if(srcBytes.length < n) => dstStr
                                case (bpf @ BPF(n, func)) :: lt => parseBinary(srcBytes.drop(n), dstStr + func(bpf.arg(srcBytes.take(n))))(lt)
                            }
                        }
//                        println(f"★1:cnt=$cnt")
                        parseBinary(buff, "")(bpfList)
                    }
                )
                parseFuture.onSuccess {
                    case s => {
//                        println(f"★2:cnt=$cnt%d")
                        pWriter.println(s)
//                        println(f"★3:cnt=$cnt%d")
                    }
                }
                parseFuture
            }

            def startParseStopFuture(previousFuture: Future[String], cnt: Long): Future[String] = {
                implicit val execContext = parseExecutionContext
                val parseStopFuture = previousFuture.flatMap(s => Future {""}(parseExecutionContext))
                parseStopFuture.onSuccess {
                    case _ => countDownLatch.countDown
                }
                parseStopFuture
            }

            /* ------------------------------------------------------------ */

            def parseLoop(lineCount: Long, previousFuture: Future[String]) {
                val inBuff: Array[Byte] = new Array(RECORD_SIZE)
                var ret = inStream.read(inBuff)
                if(ret < 0) {
                    println("end of file")
                    startParseStopFuture(previousFuture, lineCount)
                } else if(ret < RECORD_SIZE) {
                    println(f"□2:lineCoount=$lineCount")
                    println("No enough bytes in last line.")
                    println(s"remain : " +
                            inBuff.take(ret).map(_.toByte).grouped(2).map(a => f"${a(0)}%02X${a(1)}%02X").mkString(","))
                    startParseStopFuture(previousFuture, lineCount)
                } else {
                    val parseFuture = startBinaryParserFuture(previousFuture, lineCount, inBuff)
                    parseLoop(lineCount+1, parseFuture)
                }
            }

            parseLoop(1, Future{""}(parseExecutionContext))

            countDownLatch.await
            pWriter.flush
        }
    }

    def parseFromFile(fileName: String) {
        val inStream = new FileInputStream(fileName) 
        val pWriter = new PrintWriter(new BufferedWriter(new FileWriter(fileName.split('.')(0) + "_out.txt")), true)
        ultimately{
            pWriter.close
        } {
            parseBinaryStream(inStream, pWriter)
        }
    }

    def parseFromString(hexStr: String) {
        val inStream = new ByteArrayInputStream(hexStr.grouped(2).map(Integer.parseInt(_,16).toByte).toArray) 
        val pWriter = new PrintWriter(new OutputStreamWriter(System.out), true)
        parseBinaryStream(inStream, pWriter)
    }
}

/*
 * 29010203ABCDABCDABCDEFABACA80164
 * 29 01:02:03 ABCD 43981 2882400171 /192.168.1.100
 *
 */
object BinaryStreamParser01 extends BinaryStreamParser {
    val RECORD_SIZE = 16

    val bpfList = List(
        BPF[String](1, _ + " "),                /* date */
        BPF[String](1, _ + ":"),                /* hour */
        BPF[String](1, _ + ":"),                /* minute */
        BPF[String](1, _ + " "),                /* second */
        BPF[String](2, _ + " "),                /* status */
        BPF[Int]   (2, i => f"$i%5d" + " "),    /* millisecond */
        BPF[Long]  (4, l => f"$l%6d" + " "),    /* nanosecond */
        BPF[InetAddress](4, _.toString)         /* IP Address */
    )
}

/*
 * 031134131d0202230001f17500d0ec003ddcfc61982885e708004500041c16f9400080119597c0a8648bc0a86464efdb2ee00408a67b0000583a9485
 * 03 11:34:13 1D02   547 127349 00D0EC003DDCFC61982885E708004500041C16F9400080119597C0A8648BC0A86464EFDB2EE00408A67B 00 00 583A9485
 *
 */

object BinaryStreamParser02 extends BinaryStreamParser {
    val RECORD_SIZE = 60

    val bpfList = List(
            BPF[String]( 1, _ + " "),               /* date */
            BPF[String]( 1, _ + ":"),               /* hour */
            BPF[String]( 1, _ + ":"),               /* minute */
            BPF[String]( 1, _ + " "),               /* second */
            BPF[String]( 2, _ + " "),               /* status */
            BPF[Int]   ( 2, i => f"$i%5d" + " "),   /* millisecond */
            BPF[Long]  ( 4, l => f"$l%6d" + " "),   /* nanosecond */
            BPF[String](42, _ + " "),               /* packet dump */
            BPF[String]( 1, _ + " "),               /* port_on */
            BPF[String]( 1, _ + " "),               /* dummy */
            BPF[String]( 4, _ + "")                 /* fcs(CRC) */
    )
}

/* vim: set foldmethod=marker: */
