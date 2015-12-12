:reset

val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }

val x = 1.618034

(fibs take 15).map(_.toInt).foldLeft(List.empty[Tuple2[Int,Int]]){(z,n) =>
    if(z.isEmpty) z :+ (0, n)
    else z :+ (z.last._2,n)
}.foldLeft(List.empty[Tuple3[Int,Double,Double]]){(z,tp2) =>
    val (an_1,an) = tp2
    val b = an - (x * an_1)
    if(z.isEmpty) z :+ (an, 0.0, b)
    else z :+ (an, z.last._3, b)
}.foldLeft(1){(z,tp3) =>
    val (an,b_1,b) = tp3
    val c = b_1/b
    println(f"a$z%02d:$an%3d:$b%+.06f:$c%f")
    z + 1
}


