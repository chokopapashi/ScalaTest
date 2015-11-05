
import scala.language.higherKinds

object Test {

trait T1 
trait T2 extends T1
trait T3 extends T2

trait TA[A]
trait TB[B <: T2]

class Test[C[D]]

trait Foo[X[_]] {
    def create[A]: X[A]
}

object ListFoo extends Foo[List] {
    def create[A]: List[A] = Nil
}

trait MyIntOptionThingy[A <: Option[Int]]

object OptionFoo extends Foo[MyIntOptionThingy]

}   //Test
import Test._
