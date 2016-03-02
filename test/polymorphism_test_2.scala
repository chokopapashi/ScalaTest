


import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}

object test {

trait TA { val s: String }
class CA(val s: String) extends TA
class CB(val s: String) extends TA

type TASub = (CA with CB)

def newInstance[A >: TASub <: TA](s: String)(implicit tA: ru.TypeTag[A]): A = {
    /* Instantiating a Type at Runtime for 2.10 */
    val m = ru.runtimeMirror(getClass.getClassLoader)               /* Obtaining a mirror */
    val cs = ru.typeOf[A].typeSymbol.asClass                        /* Obtaining a class */
    val cm = m.reflectClass(cs)                                     /* Obtaining a ClassMirror */ 
    val ctor = ru.typeOf[A].decl(ru.termNames.CONSTRUCTOR).asMethod /* Obtaining a constructor method */
    val ctorm = cm.reflectConstructor(ctor)                         /* Invoking constructor */
    ctorm(s).asInstanceOf[A]
}

//def parseParam[A >: TASub <: TA](s: String)(implicit tA: ru.TypeTag[A]): A
def parseParam[A >: TASub <: TA : ru.TypeTag](s: String): A = {
    newInstance[A](s)
}

def func1(x: Int) {
    x match {
        case 1 =>
        case 2 =>
    }
}

}

import test._

parseParam[CA]("a")
parseParam[CB]("b")

