:reset
:paste

import scala.language.implicitConversions
/*
case class COT(x: Int, y: String)

object COT {
    def apply(x: Int): COT = new COT(x, "")
    def apply(y: String): COT = new COT(0, y)
}

object COTPx {
    def unapply(cot: COT) = Some((cot.x))
}
object COTPy {
    def unapply(cot: COT) = Some((cot.y))
}

implicit class COT_Converter(self: COT) {
    def unapply(cot: COT): Option[String] = Some((cot.y))
}

implicit def cot2string(cot: COT): String = cot.y

val pfx: PartialFunction[COT,Int] = {case COTPx(x) => x}
val pfy: PartialFunction[COT,String] = {case COTPy(y) => y}
val pfa: PartialFunction[Any,String] = {case COTPy(y) => y}
val cot = COT(1,"a")

def func(any: Any, pfa: PartialFunction[Any,String]): String = pfa(any)
*/

/* -------------------------------------------------------------------------- */

case class COT2[A](x: A)

val cot2i = COT2(1)
val cot2s = COT2("a")

//implicit def cot2string2[A](cot2: COT2[A]): A = cot2.x
implicit def cot2string2(cot2: COT2[String]): String = cot2.x

val pfCot2i: PartialFunction[Int,Int] = {case i => i}
val pfCot2s: PartialFunction[String,String] = {case s => s}

def func2[A,B](a: A, pfcot2: PartialFunction[A,B]):B = pfcot2(a)



