
import scala.collection.AbstractSet
import scala.collection.generic.{CanBuildFrom,GenericCompanion,GenericSetTemplate,SetFactory}
import scala.collection.immutable.Set
import scala.collection.Iterable
import scala.collection.mutable.Builder
import scala.collection.mutable.SetBuilder
import scala.collection.SetLike

object Test {

trait TestTrait
case class AAA() extends TestTrait

class TestSet[A <: TestTrait](set: Set[A] = Set.empty[A])
    extends AbstractSet[A]
    with Set[A]
    with SetLike[A, TestSet[A]]
    with Serializable
{
    def +(elem: A): TestSet[A] = new TestSet(set + elem)
    def -(elem: A): TestSet[A] = new TestSet(set - elem)
    def contains(elem: A): Boolean = set.contains(elem)
    def iterator: Iterator[A] = set.iterator
    override def empty = new TestSet()
}

object TestSet
{
    def empty[A <: TestTrait]: TestSet[A] = new TestSet[A]()
    private def newBuilder[A <: TestTrait]: Builder[A, TestSet[A]] = new SetBuilder[A, TestSet[A]](empty)
    def apply[A <: TestTrait](elems: A*): TestSet[A] = (empty[A] /: elems) (_ + _)
    private def testSetCanBuildFrom[A <: TestTrait] = new CanBuildFrom[TestSet[A], A, TestSet[A]] {
        def apply(from: TestSet[A]) = newBuilder
        def apply() = newBuilder
    }
}

}
import Test._
