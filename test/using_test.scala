:reset

import org.hirosezouen.hzutil.HZIO._
import java.io._
import scala.collection.mutable

val reader = new BufferedReader(new FileReader("test_data_out.txt"))

using(reader) { r =>
    var lines = mutable.ArrayBuffer.empty[String]
    var loopFlag = true
    while(loopFlag) {
        val line = r.asInstanceOf[BufferedReader].readLine
        if(line == null)
            loopFlag = false
        else
            lines += line
    }
    lines
}


