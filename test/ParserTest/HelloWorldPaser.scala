:reset

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

object HelloWorld extends Parsers {
    type Elem = Char
    val EOF = elem("EOF", _ == CharSequenceReader.EofCh)
    def str(s: String): Parser[String] = 
        if(s.length > 0) 
            elem(s.charAt(0)) ~ str(s.substring(1)) ^^ { case a ~ b => a + b }
        else success("")
//    lazy val message: Parser[Any] = 
//        elem('H') ~ elem('e') ~ elem('l') ~ elem('l') ~ elem('o') ~ elem(',') ~ 
//        elem(' ') ~ elem('W') ~ elem('o') ~ elem('r') ~ elem('l') ~ elem('d') ~ 
//        elem('!') ~ (message | success(null))
//    lazy val message: Parser[Any] =
//        str("Hello, World!") ~ (message  | success(null))
//    lazy val message: Parser[Any] = rep1(str("Hello, World!"))
    lazy val message: Parser[Int] = rep1(str("Hello, World!")) ^^ {xs => xs.length}
//    val messageAll: Parser[Any] = message ~ EOF
    val messageAll: Parser[Any] = message <~ EOF
}

import HelloWorld._

val s = "Hello, World!" + CharSequenceReader.EofCh

//messageAll(new CharSequenceReader(s)) match
messageAll(new CharSequenceReader(s)) match
{
//    case Success(_, _) => println("Hello, World!")
    case Success(v, _) => println(v + "times")
    case _ => println("Error!")
}

