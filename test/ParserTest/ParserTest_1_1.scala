:reset

import scala.util.parsing.combinator.RegexParsers

trait AST
case class AST_String(s: String) extends AST
case class AST_Bytes(bs: String) extends AST
case class AST_Comment(c: String) extends AST
case class AST_Command(cmd_label: String, cmd_arg: String) extends AST
case class AST_Elements(es: List[AST]) extends AST
case class AST_Empty() extends AST

object TestParser extends RegexParsers {
    override def skipWhitespace = false

    override def log[T](p: =>Parser[T])(name: String): Parser[T] = p
//    def eol = log('\r' ~ '\n' | elem('\r') | elem('\n'))("eol")
    def eol = log("\r\n" | "\r" | "\n")("eol")
    def string = log("""[^\s]+""".r)("string") ^^ {s => AST_String(s)}
    def bytes = log('[' ~> """\p{XDigit}{2}""".r ~ rep(':' ~> """\p{XDigit}{2}""".r) <~ ']')("bytes") ^^ {
        case bh ~ bt => AST_Bytes((bh :: bt).mkString)
    }
    def cmd_arg = log("""\w+""".r)("cmd_arg")
    def cmd_label = log("""\w+""".r)("cmd_label")
    def cmd = log(cmd_label ~ "(" ~ cmd_arg ~ ")")("cmd") ^^ {case cl ~ _ ~ ca ~ _ => AST_Command(cl,ca)}
    def comment = log('#' ~> ".*".r)("comment") ^^ {c => AST_Comment(c)}
    def element = log(comment | cmd | bytes | string)("element")
    def elements = log(element ~ rep(rep1(' ') ~> element))("elements") ^^ {
        case eh ~ et => AST_Elements(eh :: et)
    }
    def line = log((comment | elements) <~ eol)("line")
    def emptyLine = eol ^^ {_ => AST_Empty()}
    def lines = log(rep(emptyLine | line) ~ opt(element))("lines") ^^ {
        case ls ~ optl => {
            val lsl = optl match {
                case Some(l) => ls :+ l
                case None    => ls
            }
            lsl.filter {
                case ast: COMU_AST_Empty => false
                case _ => true
            }
        }
    }

    def all: Parser[List[AST]] = lines
//    def all = lines

    def parse(input: String) = parseAll(all, input)
}

val s = """|abc def #ghi
           |#123 456
           |def(ghi)
           |456
           |
           |[01:02:03:04]
           |""".stripMargin

TestParser.parse(s)


