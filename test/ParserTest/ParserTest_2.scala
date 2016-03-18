:reset

import scala.util.parsing.combinator.RegexParsers

case class Bytes(bs: Array[Byte])
case class AB(a: String, b: String)

object TestParser2 extends RegexParsers {

    def bytes = log('[' ~> """\p{XDigit}{2}""".r ~ rep(':' ~> """\p{XDigit}{2}""".r) <~ ']')("bytes") ^^ {
        case bh ~ bt => Bytes((bh :: bt).map(b => Integer.parseInt(b,16).toByte).toArray)
    }
    def all = bytes 

//    def a = log("[a-zA-Z]+".r)("b")
//    def b = log("[0-9]+".r)("a")
//    def all = log((a <~ "(") ~ (b <~ ")"))("all") ^^ {case a ~ b => AB(a,b)}

    def parse(input: String) = parseAll(all, input)
}

//val s = "[123abc456def]"
val s = "[12:ab:cd]"

TestParser2.parse(s)

