:reset
:paste

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

val pfx: PartialFunction[COT,Int] = {case COTPx(x) => x}
val pfy: PartialFunction[COT,String] = {case COTPy(y) => y}
val cot = COT(1,"a")



