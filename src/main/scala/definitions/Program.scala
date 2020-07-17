package definitions
import definitions.Operation.Result

trait Program {
  def eval(text: String): Result
}
trait Stack {
  val numbers : List[Int]
  def toString: String
}

sealed trait Error
object DivisionByZero extends Error
object StackUnderflow extends Error
object InvalidWord extends Error
object UnknownWord extends Error
