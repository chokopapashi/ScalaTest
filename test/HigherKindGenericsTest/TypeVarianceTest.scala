/*
object Sample_001 {
    class Nonvariant[A]
    def func(arg: Nonvariant[Number]) = println(arg)
    def test1 = func(new Nonvariant[Any])
    def test2 = func(new Nonvariant[Number])
    def test3 = func(new Nonvariant[Integer])
}
*/
/*
object Sample_002 {
    class Nonvariant[+A]
    def func(arg: Nonvariant[Number]) = println(arg)
    def test1 = func(new Nonvariant[Any])
    def test2 = func(new Nonvariant[Number])
    def test3 = func(new Nonvariant[Integer])
}
*/
/*
object Sample_003 {
    class Nonvariant[-A]
    def func(arg: Nonvariant[Number]) = println(arg)
    def test1 = func(new Nonvariant[Any])
    def test2 = func(new Nonvariant[Number])
    def test3 = func(new Nonvariant[Integer])
}
*/
/*
object Sample_004 {
    class ValueHolder[A](var value: A)
    def intVal(vh: ValueHolder[Number]) = vh.value.intValue
  
    def test {
        val vh1 = new ValueHolder[Number](0)
        vh1.value = 100         // Integer as sub type of Number
        println(intVal(vh1))    // 100
    
        vh1.value = 3.14        // Double as sub type of Number
        println(intVal(vh1))    // 3
  }
}
*/
/*
object Sample_005 {
    class ValueHolder[A](val value: A)
    def intVal(vh: ValueHolder[Number]) = vh.value.intValue
  
    def test {
        val vh1 = new ValueHolder[Number](100)  // Integer as sub type of Number
        println(intVal(vh1))    // 100
    
        val vh2 = new ValueHolder[Number](3.14) // Double as sub type of Number
        println(intVal(vh2))    // 3
  }
}
*/
object Sample_006 {
    class ValueHolder[+A](val value: A)
    def intVal(vh: ValueHolder[Number]) = vh.value.intValue
  
    def test {
        val vh1 = new ValueHolder[Number](100)  // Integer as sub type of Number
        println(intVal(vh1))    // 100
    
        val vh2 = new ValueHolder[Number](3.14) // Double as sub type of Number
        println(intVal(vh2))    // 3
  }
}
