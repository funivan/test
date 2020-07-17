package parser

import definitions.{Error, Operation, UserInstructions}
import parser.operations.ErrorOperation
import parser.tokenizer.Token

case class StepResult(
                       operation: Operation,
                       next: List[Token],
                       userInstructions: UserInstructions
                     )

object StepResult {
  def apply(error: Error): StepResult = StepResult(ErrorOperation(error), List.empty, new UserInstructions())
}
