:reset

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}

object test

class CA(val s: String)

def newInstance[A <: CA](s: String)(implicit tA: ru.TypeTag[A]): A = {
    /* Instantiating a Type at Runtime for 2.10 */
    val m = ru.runtimeMirror(getClass.getClassLoader)               /* Obtaining a mirror */
    val cs = ru.typeOf[A].typeSymbol.asClass                        /* Obtaining a class */
    val cm = m.reflectClass(cs)                                     /* Obtaining a ClassMirror */ 
    val ctor = ru.typeOf[A].decl(ru.termNames.CONSTRUCTOR).asMethod /* Obtaining a constructor method */
    val ctorm = cm.reflectConstructor(ctor)                         /* Invoking constructor */
    ctorm(s).asInstanceOf[A]
}

/*
def newInstance(x: Int) {

}

def parseParam[A](x: Int): A = {

}

def func1(x: Int) {
    x match {
        case 1 =>
        case 2 =>
    }
}
*/



newInstance[CA]("a")
