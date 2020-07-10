import ForthError.ForthError

import Parser._
import Tokenizer.tokenize

object ForthError extends Enumeration {
  type ForthError = Value
  val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord = Value
}

trait ForthEvaluatorState {
  // TODO: Implement. return the current stack as Text with the element
  // on top of the stack being the rightmost element in the output."
  override def toString: String
}


trait ForthEvaluator  {
  def eval(text: String): Either[ForthError, ForthEvaluatorState]
}

case class Stack(numbers: List[Int] = List()) extends ForthEvaluatorState {
  override def toString: String = numbers.mkString(" ")
}

class Forth extends ForthEvaluator {
  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    Left(ForthError.UnknownWord)
    parse(tokenize(text.split(" ").toList), Instructions())
      .compute()
  }

}
