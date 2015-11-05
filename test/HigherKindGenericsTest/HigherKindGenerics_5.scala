import scala.language.higherKinds

trait TA[A]
trait TB[B]
trait TAB[A,B]
trait TC[C]
trait TD[D[C] <: TC[C]]

//trait TX[A, B, C[A,B] <: TAB[A,B], D[C] <: TC[C]] extends TD[D]
class CX[A, B, C[A,B] <: TAB[A,B], D[C] <: TC[C]] extends TD[D]


