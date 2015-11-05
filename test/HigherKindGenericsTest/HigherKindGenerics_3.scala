import scala.language.higherKinds

trait T11[A]
trait T12[B[X] <: T11[X]]
trait T21[A]
//trait T22[B[X] <: T21[X]]

trait T31[C[X] <: T11[X]] extends T12[C]
//trait T4[A, B[A] <: T21[A], C[B] <: T11[B]] extends T12[C]

/*
trait T1[A]
trait T2[B[X] <: T1[X]]
type T3[C[X] <: T1[X]] = T2[C]
//trait T3 extends T2[B]
//class C1[A, B[A]] extends T2[B]
*/

/*

type T2[A] = T1[Int,A]
trait T3[A, B, C[B] <: T1[A,B]]
class C1[A] extends T3[Int, A, T2]

trait T11[A] { def getA: A }
trait T12[A, B[C] <: T11[C]] { def getB: B[A] }
class C11[A](x: A) extends T11[A] with T12[A, C11] {
    def getA = x
    def getB = new C11(x)
}
*/
/*
trait T21[A] { def getA: A }

trait GenTraversable[+A]
trait GenericTraversableTemplate[+A, +CC[X] <: GenTraversable[X]]
trait Traversable[+A] extends GenTraversable[A] with GenericTraversableTemplate[A, Traversable]
abstract class GenericCompanion[+CC[X] <: Traversable[X]]
abstract class TraversableFactory[CC[X] <: Traversable[X] with GenericTraversableTemplate[X, CC]] extends GenericCompanion[CC]
object Traversable extends TraversableFactory[Traversable]
*/

