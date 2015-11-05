
import scala.language.higherKinds

object ExtendsTest {

trait GenTraversable[+A]
trait Traversable[+A] extends GenTraversable[A]
trait Iterable[+A] extends Traversable[A]
trait GenSetLike[A, +Repr]
trait SetLike[A, +This <: SetLike[A, This] with Set[A]]
    extends GenSetLike[A, This]
trait Set[A] extends (A => Boolean) with Iterable[A] with SetLike[A, Set[A]]

trait T1

//class TestSet1[A] extends Set[A] with SetLike[A, TestSet1[A]] {def apply(x: A) = true}
class TestSet[A <: T1] extends Set[A] with SetLike[A, TestSet[A]] {def apply(x: A) = true}
//class TestSet[A, B[A] <: T1[A]] extends Set[B[A]] with SetLike[B[A], TestSet[A, B]] {def apply(x: B[A]) = true}

/* ------------------------------------------------------------------------ */

abstract class GenericCompanion[+CC[X] <: GenTraversable[X]]
trait GenericSeqCompanion[CC[X] <: Traversable[X]] extends GenericCompanion[CC]
abstract class SetFactory[CC[X] <: Set[X] with SetLike[X, CC[X]]]
    extends GenericSeqCompanion[CC]
abstract class ImmutableSetFactory[CC[X] <: Set[X] with SetLike[X, CC[X]]]
    extends SetFactory[CC]
abstract class MySetFactory[+A <: T1, CC[A] <: Set[A] with SetLike[A, CC[A]]]
    extends ImmutableSetFactory[CC]

object TestSet extends MySetFactory[T1, TestSet]
}   //ExtendsTest
import ExtendsTest._
