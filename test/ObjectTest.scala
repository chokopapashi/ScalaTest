/*
trait T1 {
    protected val init_x: Int
    private var x = init_x
    def getX = x
}

object O1 extends T1 {
    protected val init_x = 1
}

object O2 extends T1 {
    protected val init_x = 2
}

(O1.getX, O2.getX)
*/

trait T1 {
    def init_x: Int
    val x = init_x
    println("★1")
    def getX = x
}

case class CC1() extends T1 {
    def init_x = 1
    println("★2")
}

object Test extends App {

    println(f"${CC1().getX}%d")

}


