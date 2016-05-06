:reset

import scala.util.parsing.combinator.RegexParsers

trait AST
case class AST_FixedLengthData(length: String, padding: Option[String], data: String) extends AST
case class AST_Comment(c: String) extends AST
case class AST_Padding(s: String) extends AST
case class AST_String(s: String) extends AST

object TestParser4 extends RegexParsers {
    override def skipWhitespace = false

    def eol = "\r\n" | "\r" | "\n"

    def fixed_length_data = log("[" ~ """\d+""".r ~ ":" ~ opt(""".""".r) ~ ":" ~ "[^]\r\n]+".r ~ "]")("fixed_length_data") ^^ {
        case _ ~ len ~ _ ~ optPad ~ _ ~ data ~ _ => AST_FixedLengthData(len, optPad, data)
    }

//    def fixed_length_data = "[" ~ """\d+""".r ~ ":" ~ opt(""".""".r) ~ "]" ^^ {
//        case _ ~ len ~ _ ~ optPad ~ _ => AST_FixedLengthData(len, optPad)
//    }


    def string = log("""\w+""".r)("string") ^^ {s => AST_String(s)}
//    def padding = log("""\p{XDigit}{2}""".r | """0x\p{XDigit}{2}"""  )("padding")
    def padding = log("""0x\p{XDigit}{2}""".r |"""'.'""".r)("padding") ^^ {p => AST_Padding(p)}
    def comment = log('#' ~> ".*".r)("comment") ^^ {c => AST_Comment(c)}
    def elem = log(comment | padding | fixed_length_data | string)("elem")
    def line = log(elem <~ eol)("line")
    def all = rep(line)

    def parse(input: String) = parseAll(all, input)
}

val s1 = """|abc
            |[2:.: abc ]
            |#def
            |0xAB
            |'''
            |""".stripMargin

TestParser4.parse(s1)

