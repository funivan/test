package parser.transformer

import definitions.UserInstructions
import parser.{Parser, StepResult}
import parser.tokenizer.{InstructionIdentifier, Token}

object ProcessInstructions extends Transformer {
  override def transform(tokens: List[Token], userInstructions: UserInstructions, parser: Parser): Option[StepResult] = {
    Some(tokens.head) collect {
      case InstructionIdentifier(id) if userInstructions.contains(id) => StepResult(userInstructions(id), tokens.tail, userInstructions)
    }
  }
}
