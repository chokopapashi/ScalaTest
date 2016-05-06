:reset

import scala.util.parsing.combinator.RegexParsers

trait COMU_AST
case class COMU_AST_String(s: String) extends COMU_AST
case class COMU_AST_Decimal(d: String) extends COMU_AST
case class COMU_AST_Bytes(bs: String) extends COMU_AST
case class COMU_AST_FixedLengthData(length: String, padding: Option[String], data: String) extends COMU_AST
case class COMU_AST_Comment(c: String) extends COMU_AST
case class COMU_AST_Command(cmd_label: String, cmd_arg: List[COMU_AST]) extends COMU_AST
case class COMU_AST_Elements(es: List[COMU_AST]) extends COMU_AST
case class COMU_AST_Empty() extends COMU_AST

object COMU_DataParser extends RegexParsers {
    override def skipWhitespace = false
//    override def log[T](p: =>Parser[T])(name: String): Parser[T] = p

//    def eol = log('\r' ~ '\n' | elem('\r') | elem('\n'))("eol")
    def eol = log("\r\n" | "\r" | "\n")("eol")
    def string = log('"' ~> """[^"\r\n]+""".r <~ '"')("string") ^^ {s => COMU_AST_String(s)}
    def decimal = log("""\d+""".r)("decimal") ^^ {d => COMU_AST_Decimal(d)}
    def hex = """\p{XDigit}{2}""".r
    def bytes = log('[' ~> hex ~ rep(':' ~> hex) <~ ']')("bytes") ^^ {
        case bh ~ bt => COMU_AST_Bytes((bh :: bt).mkString)
    }
    def fixed_length_data = log("(" ~ """\d+""".r ~ ":" ~ opt(""".""".r) ~ ":" ~ "[^)\r\n]+".r ~ ")")("fixed_length_data") ^^ {
        case _ ~ len ~ _ ~ optPad ~ _ ~ data ~ _ => COMU_AST_FixedLengthData(len, optPad, data)
    }
    def cmd_arg = log(fixed_length_data | bytes | decimal | string)("cmd_arg")
    def cmd_args = log(cmd_arg ~  rep(rep1(' ') ~> cmd_arg)("cmd_arg")
    def cmd_label = log("""\w+""".r)("cmd_label")
    def cmd = log(cmd_label ~ "(" ~ cmd_arg ~ ")")("cmd") ^^ {case cl ~ _ ~ ca ~ _ => COMU_AST_Command(cl,ca)}
    def comment = log('#' ~> "[^\r\n]*".r)("comment") ^^ {c => COMU_AST_Comment(c)}
    def element = log(comment | cmd | fixed_length_data | bytes | string | decimal)("element")
    def elements = log(element ~ rep(rep1(' ') ~> element))("elements") ^^ {
        case eh ~ et => COMU_AST_Elements(eh :: et)
    }
    def line = log((comment | elements) <~ eol)("line")
    def emptyLine = eol ^^ {_ => COMU_AST_Empty()}
    def lines = log(rep(emptyLine | line) ~ opt(element))("lines") ^^ {
        case ls ~ optl => {
            val asts = optl match {
                case Some(l) => ls :+ l
                case None    => ls
            }
            asts.filter {
                case ast: COMU_AST_Empty => false
                case _ => true
            }
        }
    }

    def parse(input: String): Either[String,List[COMU_AST]] = parseAll(lines, input) match {
        case Success(asts, _)   => Right(asts)
        case failure: NoSuccess => Left(failure.msg)
    }
}


val s1 = """|"abc" "def" #ghi
           |#123 456
           |def("ghi")
           |456
           |
           |[01:02:03:04]
           |""".stripMargin

val s2 = """|"abc" "def"
            |""".stripMargin


COMU_DataParser.parse(s1)


