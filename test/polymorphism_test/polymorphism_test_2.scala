:reset 

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}

object Test {

trait TA { val s: String }
class CA(val s: String) extends TA
class CB(val s: String) extends TA

type TASub = (CA with CB)

//def newInstance[A >: TASub <: TA : ru.TypeTag](s: String): A = {
def newInstance[A : ru.TypeTag](s: String): A = {
    /* Instantiating a Type at Runtime for 2.10 */
    val m = ru.runtimeMirror(getClass.getClassLoader)               /* Obtaining a mirror */
    val cs = ru.typeOf[A].typeSymbol.asClass                        /* Obtaining a class */
    val cm = m.reflectClass(cs)                                     /* Obtaining a ClassMirror */ 
    val ctor = ru.typeOf[A].decl(ru.termNames.CONSTRUCTOR).asMethod /* Obtaining a constructor method */
    val ctorm = cm.reflectConstructor(ctor)                         /* Invoking constructor */
    ctorm(s).asInstanceOf[A]
}

def parseParam[A >: TASub <: TA : ru.TypeTag](x: Int, s: String): A
//def parseParam[A : ru.TypeTag](x: Int, s: String): A
= {
    x match {
        case 1 => newInstance[CA](s).asInstanceOf[A]
        case 2 => newInstance[CB](s).asInstanceOf[A]
    }
}

def func2(x: CA)  = "This is CA"
def func2(x: CB)  = "This is CB"

//def func1(x: Int) {
//    val c = x match {
//        case 1 => parseParam[CA]("a").asInstanceOf[CA]
//        case 2 => parseParam[CB]("B").asInstanceOf[CB]
//    }
////    func2(c)
//}

}

import Test._

//func1(1)

func2(parseParam[TASub](1, "a"))

