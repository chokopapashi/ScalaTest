import scala.language.higherKinds

import scala.collection.immutable
import scala.collection.{Iterable, IterableLike}

type IntMap[A] = immutable.Map[Int,A]
type StringMap[A] = immutable.Map[String,A]

trait MapProxy[A, B, ReprMap[B] <: Map[A,B]] {
    val underlying: ReprMap[B]
}

class StringMapProxy[A](elems: (String, A)*) extends MapProxy[String, A, StringMap] {
    val underlying: StringMap[A] = Map(elems:_*)
    def apply(key: String): A = underlying(key)
}

class IntMapProxy[A](elems: (Int, A)*) extends MapProxy[Int, A, IntMap] {
    val underlying: IntMap[A] = Map(elems:_*)
    def apply(key: Int): A = underlying(key)
}

trait TypedIterableProxy[A, Repr <: Iterable[A]] extends Iterable[A] with IterableLike[A, Repr] {
    import scala.collection.generic.CanBuildFrom
    import scala.collection.mutable.{ListBuffer, Builder}
    val self:Iterable[A]
    def newTo(from:Iterable[A]):Repr

    def iterator = self.iterator
    override def newBuilder:Builder[A, Repr] = new ListBuffer[A] mapResult {x => newTo(x) }
    implicit def canBuildFrom: CanBuildFrom[Repr, A, Repr] = new CanBuildFrom[Repr, A, Repr] {
        def apply(from: Repr):Builder[A, Repr] = newBuilder
        def apply() = newBuilder
    }
}

// 特定の型のIterableへProxyするクラス
class StringItrableProxy(v: String*) extends Iterable[String] with TypedIterableProxy[String, StringItrableProxy] {
    val self= v.toIterable
    def newTo(from:Iterable[String]) = new StringItrableProxy(from.toSeq:_*)
//    def hoge = map{ "hoge" +  ""}
}

