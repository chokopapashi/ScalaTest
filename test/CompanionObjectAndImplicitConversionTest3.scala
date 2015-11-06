:reset
:paste

import scala.language.implicitConversions

case class COT2[A](x: A)

implicit def cot2string[A](cot2: COT2[A]): A = cot2.x

type PFParam = Int with String
type PF[A >: PFParam] =PartialFunction[A,Unit]

class Test[A](pf: PF[A]) {
    def processData(data: A) = pf(data)
}
object Test {
    def apply[A](pf: PF[A]): Test[A] = new Test(pf)
}

val test = Test [Any]{
    case s: String => println(s)
    case _ => println("Oops!")
}



