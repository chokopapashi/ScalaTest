:reset

object Problem2 {
    def problem2_01(seq1: Seq[Int], seq2: Seq[Int]): Seq[Int] = {
        def getShortSeq(seqs: Seq[Int], seql: Seq[Int]) =
                seqs ++ Seq.fill(seql.size-seqs.size)(0)
        val (tseq1, tseq2) =
            if(seq1.size < seq2.size) (getShortSeq(seq1,seq2), seq2)
            else (seq1, getShortSeq(seq2,seq1))

        (for(i <- 0 to (tseq1.size-1))
         yield Seq(tseq1(i), tseq2(i))).flatten
    }

    def problem2_02(seq1: Seq[Int], seq2: Seq[Int]): Seq[Int] = {
        def margeSeq(s1: Seq[Int], s2: Seq[Int], s3: Seq[Int]): Seq[Int] = {
            if(s1.isEmpty && s2.isEmpty) s3
            else if(s1.isEmpty) margeSeq(s1, s2.tail, s3 ++ Seq(0, s2.head))
            else if(s2.isEmpty) margeSeq(s1.tail, s2, s3 ++ Seq(s1.head,0))
            else margeSeq(s1.tail, s2.tail, s3 ++ Seq(s1.head, s2.head))
        }
        margeSeq(seq1, seq2, Seq.empty[Int])
    }

    def problem2_03(seq1: Seq[Int], seq2: Seq[Int]): Seq[Int] =
        seq1.zipAll(seq2, 0, 0).flatMap(pair => Seq(pair._1,pair._2))
}

import Problem2._

println("problem2_01 = " + problem2_01(List(1,2,3,4,5), List(9,8,7)))
println("problem2_02 = " + problem2_02(List(1,2,3,4,5), List(9,8,7)))
println("problem2_03 = " + problem2_03(List(1,2,3,4,5), List(9,8,7)))

