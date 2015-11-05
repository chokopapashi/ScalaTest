import scala.language.higherKinds

object Test {

trait T1[A,B]
type T2[A] = T1[Int,A]
trait T3[A, B, C[B] <: T1[A,B]]
class C1[A] extends T3[Int, A, T2]

trait T11[A] { def getA: A }
trait T12[A, B[C] <: T11[C]] { def getB: B[A] }
class C11[A](x: A) extends T11[A] with T12[A, C11] {
    def getA = x
    def getB = new C11(x)
}

trait MyTrait[A]
type MT[A] = MyTrait[A]

trait GenTraversable[+A]
trait GenericTraversableTemplate[+A, +CC[X] <: GenTraversable[X]]
trait Traversable[+A] extends GenTraversable[A] with GenericTraversableTemplate[A, Traversable]
abstract class GenericCompanion[+CC[X] <: Traversable[X]]
abstract class TraversableFactory[CC[X] <: Traversable[X] with GenericTraversableTemplate[X, CC]] extends GenericCompanion[CC]
object Traversable extends TraversableFactory[Traversable]

}   // Test
import Test._
