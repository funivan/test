package parser.transformer

import definitions.{Operation, UserInstructions}
import parser.{Parser, StepResult}
import parser.operations.{Divide, Drop, Duplicate, Math, Over, Push, Swap}
import parser.tokenizer.{T_NUMBER, T_OPERATION, T_WORD, Token}

object Main extends Transformer {
  override def transform(tokens: List[Token], userInstructions: UserInstructions, parser: Parser): Option[StepResult] = {
    val op: Option[Operation] = Some(tokens.head) collect {
      case T_NUMBER(x) => Push(x)
      case T_OPERATION("*") => Math(_ * _)
      case T_OPERATION("+") => Math(_ + _)
      case T_OPERATION("-") => Math(_ - _)
      case T_OPERATION("/") => Divide
      case T_WORD("dup") => Duplicate
      case T_WORD("drop") => Drop
      case T_WORD("swap") => Swap
      case T_WORD("over") => Over
    }
    op.flatMap { v => Some(StepResult(v, tokens.tail, userInstructions)) }
  }
}
