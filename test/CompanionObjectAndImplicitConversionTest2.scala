:reset
:paste

import scala.language.implicitConversions

case class COT2[A](x: A)

implicit def cot2string[A](cot2: COT2[A]): A = cot2.x

class Test[A](pf: PartialFunction[A,Unit]) {
    def processData(data: A) = pf(data)
}
object Test {
    def apply[A](pf: PartialFunction[A,Unit]): Test[A] = new Test(pf)
}

val test = Test[String] {
    case s: String => println(s)
}



