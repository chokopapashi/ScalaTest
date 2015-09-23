:reset

def execTime[A](v: => A) {
    val st = System.currentTimeMillis
    v
    println(f"exec time = ${System.currentTimeMillis - st}%d")
}

:load BinaryStreamParser1.scala

execTime(BinaryStreamParser02.parseFromFile("004b81_150915.ifd0"))

