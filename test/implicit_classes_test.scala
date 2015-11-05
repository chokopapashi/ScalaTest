
object Test { /* dummy */

implicit class RichT1(t1: T1) { def mkString: String = t1.toString + ":" + t1.x.toString }
trait T1 { val x: Int }
case class CC1(y: Int)(implicit val x: Int) extends T1

}   /* dummy */

import Test._
