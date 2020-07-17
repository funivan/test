package definitions
import definitions.Operation.Result
trait Operation {
  def compute(items: Stack): Result
}

object Operation {
  type Result = Either[Error, Stack]
}
