import scala.util.matching.Regex

object Tokenizer {
  val Number = "([0-9]+)".r
  val Operation = "([\\+-/*]{1})".r
  val Word = "([a-z-A-Z]+)".r

  def tokenize(list: List[String]): List[Token] = {
    list.map(_ match {
      case Number(n) => T_NUMBER(n)
      case Operation(o) => T_OPERATION(o)
      case Word(w) => T_WORD(w.toLowerCase)
      case ":" => T_COLON()
      case ";" => T_SEMICOLON()
      case value => T_UNKNOWN(value)
    })
  }
}

sealed class Token
case class T_NUMBER(value: String) extends Token {
  def toInt = value.toInt
}
case class T_WORD(value: String) extends Token

case class T_OPERATION(value: String) extends Token

case class T_COLON() extends Token

case class T_SEMICOLON() extends Token

case class T_UNKNOWN(value: String) extends Token

object InstructionIdentifier {
  def unapply(token: Token): Option[String] = {
    token match {
      case T_WORD(value) => Some(value)
      case T_OPERATION(value) => Some(value)
      case _ => None
    }
  }
}

