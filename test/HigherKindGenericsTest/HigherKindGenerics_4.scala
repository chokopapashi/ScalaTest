import scala.language.higherKinds
import scala.collection._
import scala.collection.immutable.Set
import scala.collection.mutable.Builder
import scala.collection.generic._

object Test {

trait T1

trait MyGenTraversable[+A] extends GenTraversable[A]
//trait MyCompanionTrait[A, +B[A] <: T1[A], +CC[B] <: GenTraversable[B]] extends GenericCompanion[CC]
//trait MyCompanionTrait[B <: T1, +CC[B] <: GenTraversable[B]] extends GenericCompanion[CC]
trait MyCompanionTrait[+CC[B <: T1] <: GenTraversable[B]] extends GenericCompanion[CC]
/*
object MyCompanionClass extends MyCompanionTrait[T1, MyGenTraversable] {
    def newBuilder[A]: Builder[A, MyGenTraversable[A]] = null
}
*/
}   /* object Test */
import Test._
