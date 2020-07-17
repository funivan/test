package parser.tokenizer

sealed class Token
case class T_NUMBER(value: Int) extends Token
case class T_WORD(value: String) extends Token
case class T_OPERATION(value: String) extends Token
case class T_UNKNOWN(value: String) extends Token
case class T_SEMICOLON() extends Token
case class T_COLON() extends Token

object InstructionIdentifier {
  def unapply(token: Token): Option[String] = {
    token match {
      case T_WORD(value) => Some(value)
      case T_OPERATION(value) => Some(value)
      case _ => None
    }
  }
}

