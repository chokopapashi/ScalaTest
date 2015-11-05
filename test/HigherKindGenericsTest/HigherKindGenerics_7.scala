import scala.language.higherKinds

object Test {

trait Ta1

trait Tb1[+A]
trait Tb2[+A[X] <: Tb1[X]]

class Ca1[A] extends Tb1[A]

object Ca1 extends Tb2[Ca1]



}   /* object Test */
import Test._
