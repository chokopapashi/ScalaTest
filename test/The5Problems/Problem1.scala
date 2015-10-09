:reset

object Problem1 {

    def problem1_for(seq: Seq[Int]): Int = {
        var sum = 0;
        for(i <- seq) {sum += i}
        sum
    }

    def problem1_while(seq: Seq[Int]): Int = {
        var sum = 0;
        var elem = seq
        while(elem.nonEmpty) {
            sum += elem.head
            elem = elem.tail
        }
        sum
    }

    def problem1_recursive(seq: Seq[Int]): Int = {
        def fuc_recursive(tseq: Seq[Int], sum: Int): Int = {
/*
            if(tseq.isEmpty) sum
            else fuc_recursive(tseq.tail, sum + tseq.head)
*/
            tseq match {
                case Nil => sum
                case tseq_h :: tseq_t => fuc_recursive(tseq_t, sum + tseq_h)
            }
        }
        fuc_recursive(seq, 0)
    }


}

import Problem1._

println("problem1_for       = " + problem1_for(List(1,2,3,4,5)))
println("problem1_while     = " + problem1_while(List(1,2,3,4,5)))
println("problem1_recursive = " + problem1_recursive(List(1,2,3,4,5)))

