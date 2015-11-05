:paste

class CObjectTest(_a: Int = 0, _b: Int = 0, _c: Int = 0) {
    def a = _a
    def b = _b
    def c = _c
    override def toString = "%s,%s,%s".format(a,b,c)
}

object CObjectTest {
    def apply(x: Int) = new CObjectTest(x)
    def apply(x: Int, y: Int) = new CObjectTest(x,y)
    def apply(x: Int, y: Int, z: Int) = new CObjectTest(x,y,z)

    def unapply(test: CObjectTest) = Some((test.a,test.b,test.c))
/*
    def unapply(test: Any) = test match {
        case x:CObjectTest => Some((x.a,x.b,x.c))
        case _ => None
    }
*/

}

