package parser.transformer

import definitions.{InvalidWord, UserInstructions}
import parser.operations.Empty
import parser.{Parser, StepResult}
import parser.tokenizer.{InstructionIdentifier, T_COLON, T_SEMICOLON, Token}

object ScanInstructions extends Transformer {
  override def transform(tokens: List[Token], userInstructions: UserInstructions, parser: Parser): Option[StepResult] = {
    Some(tokens.head) collect {
      case T_COLON() => parseInstruction(tokens.tail, userInstructions, parser)
    }
  }

  private def parseInstruction(
                        chars: List[Token],
                        instructions: UserInstructions, parser: Parser): StepResult = {
    val raw = chars.takeWhile {
      !_.isInstanceOf[T_SEMICOLON]
    }
    raw match {
      case InstructionIdentifier(head) :: tail =>
        val operation = parser.parse(tail, instructions)
        StepResult(
          Empty,
          chars.drop(raw.size + 1),
          instructions + (head -> operation)
        )
      case _ => StepResult(InvalidWord)
    }
  }
}
