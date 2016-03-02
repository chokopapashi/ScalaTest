:reset

:paste

trait TA

class C1 extends TA
class C2 extends TA

type CC = C1 with C2

def func1[A >: CC <: TA](x: Int): A = x match  {
    case 1 => (new C1).asInstanceOf[A]
    case 2 => (new C2).asInstanceOf[A]
}

def func2() = {
    val a = func1(1)
//    pfunc(a)
}

def pfunc(x: C1) = x
def pfunc(x: C2) = x

