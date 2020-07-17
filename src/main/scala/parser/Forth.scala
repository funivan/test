package parser

import definitions.{Program, UserInstructions}
import definitions.Operation.Result
import parser.tokenizer.Tokenizer

class Forth extends Program {
  private val parser = Parser
  private val tokenizer = Tokenizer

  override def eval(text: String): Result = {
    val r = parser.parse(
      tokenizer.tokenize(text.split(" ").toList),
      new UserInstructions()
    )
    r.compute(NumbersStack())
  }
}
