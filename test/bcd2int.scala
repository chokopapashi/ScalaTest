
   
def bcd2int(data: Array[Byte]) = data.takeWhile(_ != 0).reverse.foldLeft((0,1)) { (z, b) =>
    def map(z: Int, t2: (Int,Int)) = (z + t2._1, t2._2)
    map(z._1, (List(b & 0x0000000f, (b >> 4) & 0x0000000f)).foldLeft((0,z._2)) { (y, n) =>
        def add(x: Int, d: Int) = (x*d, d*10)
        map(y._1, n match {
            case 0    => (0, y._2)
            case 0x0a => add(0, y._2)
            case _    => add(n, y._2)
        })
    })
}


