:reset

import scala.util.parsing.combinator.RegexParsers

trait AST
case class AST_Comment(c: String) extends AST
case class AST_String(s: String) extends AST

object TestParser3 extends RegexParsers {
//    def eol = log('\r' ~ '\n' | elem('\r') | elem('\n'))("eol")

    override def skipWhitespace = true

    def comment = log('#' ~> ".*".r)("comment") ^^ {c => AST_Comment(c)}
    def string = log("""\w+""".r)("string") ^^ {s => AST_String(s)}
    def elem = log(comment | """\w+""".r)("elem")
    def elems = log(elem ~ rep(rep1(' ') ~> elem))("elems") ^^ {
        case eh ~ et => eh :: et
    }
    def all = rep(elems)

    def parse(input: String) = parseAll(all, input)
}

val s1 = """|abc 123 456
            |def
            |ghi
            |""".stripMargin

val s2 = """|#abc
            |#def
            |""".stripMargin

TestParser3.parse(s1)

