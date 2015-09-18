import java.io.BufferedWriter
import java.io.ByteArrayInputStream
import java.io.FileInputStream
import java.io.FileWriter
import java.io.InputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.net.InetAddress
import java.util.concurrent.CountDownLatch
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.Executors
import java.util.concurrent.LinkedBlockingQueue


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
    val recordSize: Int

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
        def tpe: ru.Type = ru.typeOf[A]
        def arg(bs: Array[Byte]): A = ru.typeOf[A] match {
            case t if t =:= ru.typeOf[String]      => bin2hex(bs).asInstanceOf[A]
            case t if t =:= ru.typeOf[Int]         => bin2Int(bs).asInstanceOf[A]
            case t if t =:= ru.typeOf[Long]        => bin2Long(bs).asInstanceOf[A]
            case t if t =:= ru.typeOf[InetAddress] => InetAddress.getByAddress(bs).asInstanceOf[A]
        }
    }

    val bpfList: List[BPF[_]]

    def parseBinaryStream(inStream: InputStream, pWriter: PrintWriter) {
        ultimately{
            inStream.close
        } {
            trait Ret { val n: Int }
            object Ret {
                def unapply(arg: Any):Option[Int] = arg match {
                    case ret: Ret => Some(ret.n)
                    case _ => None
                }
            }
            case class ResultString(n: Int, s: String) extends Ret
            case class EndOfList(n: Int) extends Ret

            var countDownLatch: CountDownLatch = null
            def initCountDownLatch() = countDownLatch = new CountDownLatch(1)

            val EXEC_UNIT = 1000
            val outCollectQueue = new ArrayBlockingQueue[Ret](EXEC_UNIT)
            val outQueue = new LinkedBlockingQueue[Option[Array[String]]]

            /* ------------------------------------------------------------ */

            val outExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
            val outFuture = Future {
                var loopFlag = true
                while(loopFlag) {
                    outQueue.take match {
                        case Some(sArray) => sArray.foreach(pWriter.println(_))
                        case None => loopFlag = false
                    }
                }
            }(outExecutionContext)

            val outCollectFuture = Future {
                var count = 0
                var sArray = new Array[String](EXEC_UNIT)
                var loopFlag = true
                while(loopFlag) {
                    outCollectQueue.take match {
                        case ResultString(n, s) => {
                            sArray(n-1) = s
                            count += 1
                            if(count == EXEC_UNIT) {
                                outQueue.put(Some(sArray))
                                count = 0
                                sArray = new Array(EXEC_UNIT)
                                countDownLatch.countDown
                            }
                        }
                        case EndOfList(n) => {
                            if(count != 0) {
                                outQueue.put(Some(sArray.filter(_ != null)))
                            }
                            outQueue.put(None)
                            loopFlag = false
                        }
                        case ret => throw new IllegalStateException(s"unexpected Ret : $ret")
                    }
                }
            }(outExecutionContext)

            /* ------------------------------------------------------------ */

            val parseExecutionContext: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool)
            def startBinaryParserFuture(cnt: Int, buff: Array[Byte]) {
                implicit val execContext = parseExecutionContext
                Future {
                    def parseBinary(srcBytes: Array[Byte], dstStr: String)(bpfl: List[BPF[_]]): String
                    = {
                        bpfl match {
                            case Nil => dstStr
                            case BPF(n, _) :: _ if(srcBytes.length < n) => dstStr
                            case (bpf @ BPF(n, func)) :: lt => parseBinary(srcBytes.drop(n), dstStr + func(bpf.arg(srcBytes.take(n))))(lt)
                        }
                    }

                    parseBinary(buff, "")(bpfList)
                } onSuccess {
                    case s => outCollectQueue.put(ResultString(cnt,s))
                }
            }

            def startParseStopFuture(cnt: Int) = {
                implicit val execContext = parseExecutionContext
                Future {}(parseExecutionContext) onSuccess {
                    case _ => outCollectQueue.put(EndOfList(cnt))
                }
            }

            /* ------------------------------------------------------------ */

            initCountDownLatch()
            var lineCount = 0
            val inBuff: Array[Byte] = new Array(recordSize)
            var loopFlag = true
            while(loopFlag) {
                var ret = inStream.read(inBuff)
                lineCount += 1
                if(ret < 0) {
                    println("end of file")
                    startParseStopFuture(lineCount)
                    loopFlag = false
                } else if(ret < recordSize) {
                    println("No enough bytes in last line.")
                    println(s"remain : " +
                            inBuff.take(ret).map(_.toByte).grouped(2).map(a => f"${a(0)}%02X${a(1)}%02X").mkString(","))
                    startParseStopFuture(lineCount)
                    loopFlag = false
                } else {
                    startBinaryParserFuture(lineCount, Array(inBuff: _*))
                    if((lineCount % EXEC_UNIT) == 0) {
                        countDownLatch.await
                        lineCount = 0
                        initCountDownLatch()
                    }
                }
            }

            Await.ready(outFuture, Duration.Inf)
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
    val recordSize = 16

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
    val recordSize = 60

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

