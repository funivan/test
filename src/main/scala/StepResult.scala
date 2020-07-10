import ForthError.ForthError

case class StepResult(operation: Operation, next: List[Token], userInstructions: Instructions)

object StepResult {
  def apply(error: ForthError) : StepResult = StepResult(Error(error), List(), Instructions())
}
