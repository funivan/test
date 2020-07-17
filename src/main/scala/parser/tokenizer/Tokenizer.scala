package parser.tokenizer

object Tokenizer {
  private val Number = "([0-9]+)".r
  private val Operation = "([+-/*]{1})".r
  private val Word = "([a-z-A-Z]+)".r

  def tokenize(list: List[String]): List[Token] = list.map {
    case Number(n) => T_NUMBER(n.toInt)
    case Operation(o) => T_OPERATION(o)
    case Word(w) => T_WORD(w.toLowerCase)
    case ":" => T_COLON()
    case ";" => T_SEMICOLON()
    case value => T_UNKNOWN(value)
  }
}
