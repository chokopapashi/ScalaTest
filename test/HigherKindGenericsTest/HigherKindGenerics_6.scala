import scala.language.higherKinds

object Test {

trait T11
class C11 extends T11
trait T12[A <: T11]
class C12[A <: T11] extends T12[A]

trait T21[CC[A <: T11] <: T12[A]]
trait T22[CC[A <: T11] <: T12[A]] extends T21[CC]
trait T23[A, CC[A <: T11] <: T12[A]] extends T22[CC]

new T23[C11, C12] {}

//object O1 extends T22[C12]

}   /* object Test */
import Test._
