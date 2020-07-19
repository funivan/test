package parser.transformer

import definitions.UserInstructions
import parser.{Parser, StepResult}
import parser.tokenizer.Token

trait Transformer {
  def transform(tokens: List[Token], userInstructions: UserInstructions, parser: Parser): Option[StepResult]
}

