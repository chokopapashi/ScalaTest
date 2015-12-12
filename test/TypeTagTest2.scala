
:reset

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicLong

import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.{universe => ru}

def execTime[A](v: => A) {
    val st = System.currentTimeMillis
    v
    println(f"exec time = ${System.currentTimeMillis - st}%d")
}

type MyType = String with Long

val tpe_String = ru.typeOf[String] 
val tpe_Long = ru.typeOf[Long] 

def func1[A >: MyType : ru.TypeTag](s: String): A = {
    ru.typeOf[A] match {
        case t if t =:= tpe_String => s.asInstanceOf[A]
        case t if t =:= tpe_Long   => java.lang.Long.parseLong(s).asInstanceOf[A]
    }
}


def execFunc1() {
    val countDownLatch = new CountDownLatch(1)
    val atomicLong = new AtomicLong(100000L)

    @tailrec
    def loop(l: Long) {
        if(l != 0) {
            Future {
                if((l % 2) == 0)
                    println("String:" + func1[String](f"$l%d"))
                else
                    println("Long  :" + func1[Long](f"$l%d"))
            } onSuccess {
                case _ => {
                    val cnt = atomicLong.decrementAndGet 
                    if(cnt == 0) countDownLatch.countDown
                }
            }
            loop(l - 1)
        }
    }

    loop(100000L)

    countDownLatch.await
}

execTime(execFunc1())

