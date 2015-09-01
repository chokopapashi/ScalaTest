
import java.io.File
import java.io.FileInputStream
import java.io.FileWriter
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.net.InetAddress

import scala.io.Source
import scala.util.control.Exception._

/*
 * 29010203ABCDABCDABCDEFABACA80164
 * 29 01:02:03 ABCD 43981.2882400171 192.168.1.100
 *
 */

object BinaryStreamParser {
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

    def parseBinaryStream(fileName: String) {
        val inStream = new FileInputStream(fileName) 
        val pWriter = new PrintWriter(new FileWriter(fileName.split('.')(0) + "_out.txt"))
        ultimately{
            inStream.close
            pWriter.flush
            pWriter.close
        } {
            val inBuff: Array[Byte] = new Array(16)
            var loopFlag = true
            while(loopFlag) {
                var ret = inStream.read(inBuff)
                if(ret < 0) {
                    println("end of file")
                    loopFlag = false
                } else if(ret < 16) {
                    println("No enough bytes in last line.")
                    println(s"remain : " +
                            inBuff.take(ret).map(_.toByte).grouped(2).map(a => f"${a(0)}%02X${a(1)}%02X").mkString(","))
                    loopFlag = false
                } else {
                    def parseHexString(srcBytes: Array[Byte], dstStr: String)(funList: List[Tuple2[Int,Array[Byte] => String]]): String
                    = {
                        funList match {
                            case Nil => dstStr
                            case (n, _) :: _ if(srcBytes.length < n) => dstStr
                            case (n, func) :: lt => parseHexString(srcBytes.drop(n), dstStr + func(srcBytes.take(n)))(lt)
                        }
                    }

                    val formatedLine = parseHexString(inBuff, "")(List(
                        (1, bs => bin2hex(bs) + " "),           /* date */
                        (1, bs => bin2hex(bs) + ":"),           /* hour */
                        (1, bs => bin2hex(bs) + ":"),           /* minute */
                        (1, bs => bin2hex(bs) + " "),           /* second */
                        (2, bs => bin2hex(bs) + " "),           /* status */
                        (2, bs => f"${bin2Int(bs)}%5d" + " "),   /* millisecond */
                        (4, bs => f"${bin2Long(bs)}%6d" + " "),  /* nanosecond */
                        (4, bs => InetAddress.getByAddress(bs).toString)
                                                                /* IP Address */
                    ))
                    pWriter.println(formatedLine)
                }
            }
        }
    }
}

