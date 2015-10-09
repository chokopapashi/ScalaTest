:reset

object Problem3 {
    def problem3_01(): List[(BigInt,Int)] =
        (3 to 100).foldLeft(List[BigInt](1, 0))((z,_) => (z(0)+z(1)) :: z).reverse.zipWithIndex

    def problem3_02(): List[(BigInt,Int)] = {
        def fibonacci(lf: List[Int]): List[Int] = { }
    }
}

import Problem3._

println("problem3_01 = " + problem3_01())

