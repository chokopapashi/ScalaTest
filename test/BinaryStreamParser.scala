
import java.io.File
import java.io.FileInputStream
import java.io.FileWriter
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.net.InetAddress

import scala.io.Source
import scala.util.control.Exception._

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

    type FL = List[Tuple2[Int,Array[Byte] => String]]

    def parseBinaryStreamConcrete(fileName: String)(functionList: FL) {
        val inStream = new FileInputStream(fileName) 
        val pWriter = new PrintWriter(new FileWriter(fileName.split('.')(0) + "_out.txt"))
        ultimately{
            inStream.close
            pWriter.flush
            pWriter.close
        } {
            val inBuff: Array[Byte] = new Array(recordSize)
            var loopFlag = true
            while(loopFlag) {
                var ret = inStream.read(inBuff)
                if(ret < 0) {
                    println("end of file")
                    loopFlag = false
                } else if(ret < recordSize) {
                    println("No enough bytes in last line.")
                    println(s"remain : " +
                            inBuff.take(ret).map(_.toByte).grouped(2).map(a => f"${a(0)}%02X${a(1)}%02X").mkString(","))
                    loopFlag = false
                } else {
                    def parseHexString(srcBytes: Array[Byte], dstStr: String)(fl: FL): String
                    = {
                        fl match {
                            case Nil => dstStr
                            case (n, _) :: _ if(srcBytes.length < n) => dstStr
                            case (n, func) :: lt => parseHexString(srcBytes.drop(n), dstStr + func(srcBytes.take(n)))(lt)
                        }
                    }

                    val formatedLine = parseHexString(inBuff, "")(functionList)
                    pWriter.println(formatedLine)
                }
            }
        }
    }

    def parseBinaryStream(fileName: String)
}

/*
 * 29010203ABCDABCDABCDEFABACA80164
 * 29 01:02:03 ABCD 43981.2882400171 192.168.1.100
 *
 */
object BinaryStreamParser01 extends BinaryStreamParser {
    val recordSize = 16

    def parseBinaryStream(fileName: String) {
        parseBinaryStreamConcrete(fileName)(List(
            (1, bs => bin2hex(bs) + " "),           /* date */
            (1, bs => bin2hex(bs) + ":"),           /* hour */
            (1, bs => bin2hex(bs) + ":"),           /* minute */
            (1, bs => bin2hex(bs) + " "),           /* second */
            (2, bs => bin2hex(bs) + " "),           /* status */
            (2, bs => f"${bin2Int(bs)}%5d" + " "),  /* millisecond */
            (4, bs => f"${bin2Long(bs)}%6d" + " "), /* nanosecond */
            (4, bs => InetAddress.getByAddress(bs).toString)
                                                    /* IP Address */
        ))
    }
}

/*
 * 031134131d0202230001f17500d0ec003ddcfc61982885e708004500041c16f9400080119597c0a8648bc0a86464efdb2ee00408a67b0000583a9485
 * 03 11:34:13 1D02   547 127349 00D0EC003DDCFC61982885E708004500041C16F9400080119597C0A8648BC0A86464EFDB2EE00408A67B 00 00 583A9485
 *
 */

object BinaryStreamParser02 extends BinaryStreamParser {
    val recordSize = 60

    def parseBinaryStream(fileName: String) {
        parseBinaryStreamConcrete(fileName)(List(
            (1,  bs => bin2hex(bs) + " "),          /* date */
            (1,  bs => bin2hex(bs) + ":"),          /* hour */
            (1,  bs => bin2hex(bs) + ":"),          /* minute */
            (1,  bs => bin2hex(bs) + " "),          /* second */
            (2,  bs => bin2hex(bs) + " "),          /* status */
            (2,  bs => f"${bin2Int(bs)}%5d" + " "), /* millisecond */
            (4,  bs => f"${bin2Long(bs)}%6d" + " "),/* nanosecond */
            (42, bs => bin2hex(bs) + " "),          /* packet dump */
            (1,  bs => bin2hex(bs) + " "),          /* port_on */
            (1,  bs => bin2hex(bs) + " "),          /* dummy */
            (4,  bs => bin2hex(bs))                 /* fcs(CRC) */
        ))
    }
}

