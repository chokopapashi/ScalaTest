
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

class TestSet[A <: TestTrait]
    (set: Set[A])
    extends AbstractSet[A]
    with Set[A]
//    with GenericSetTemplate[A, TestSet]
    with SetLike[A, TestSet[A]]
    with Serializable
{
    def +(elem: A): TestSet[A] = new TestSet(set + elem)
    def -(elem: A): TestSet[A] = new TestSet(set - elem)
    def contains(elem: A): Boolean = set.contains(elem)
    def iterator: Iterator[A] = set.iterator
    override def empty = new TestSet(Set.empty[A])
//    override def companion: GenericCompanion[TestSet] = TestSet
}

//object TestSet extends SetFactory[TestSet] with Set[TestTrait] with SetLike[TestTrait, TestSet[TestTrait]] {
object TestSet extends SetFactory[TestSet]
{
/*
    implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, TestSet[A]] = setCanBuildFrom[A]
//    def newBuilder[A]: Builder[A, TestSet[A]] = new Builder() {}
    def newBuilder[A]: Builder[A, TestSet[A]] = new SetBuilder[A, TestSet[A]](empty[A])
*/
//    override def empty: TestSet[TestTrait] = new TestSet(Set.empty[TestTrait])
    def newBuilder[A]: Builder[A, TestSet[A]] = null
}

}
import Test._
