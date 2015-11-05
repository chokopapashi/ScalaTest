
import scala.language.higherKinds

object ExtendsTest {

trait GenTraversable[+A]
trait Traversable[+A] extends GenTraversable[A]
abstract class GenericCompanion[+CC[X] <: GenTraversable[X]]
trait GenericSeqCompanion[CC[X] <: Traversable[X]] extends GenericCompanion[CC]
trait Iterable[+A] extends Traversable[A]
trait GenSetLike[A, +Repr]
trait SetLike[A, +This <: SetLike[A, This] with Set[A]]
    extends GenSetLike[A, This]
trait Set[A] extends (A => Boolean) with Iterable[A] with SetLike[A, Set[A]]
abstract class SetFactory[CC[X] <: Set[X] with SetLike[X, CC[X]]]
    extends GenericSeqCompanion[CC]
abstract class ImmutableSetFactory[CC[X] <: Set[X] with SetLike[X, CC[X]]]
    extends SetFactory[CC]

class TestSet[A] extends Set[A] with SetLike[A, TestSet[A]] {def apply(x: A) = true}
object TestSet extends ImmutableSetFactory[TestSet]
}   //ExtendsTest
import ExtendsTest._
