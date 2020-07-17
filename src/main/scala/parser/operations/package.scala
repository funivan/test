package parser

import definitions.Operation.Result
import definitions.{Stack, StackUnderflow}

package object operations {

  implicit final class StackChange(val x: Stack) extends AnyVal {
    def |>(min: Int): Option[List[Int]] = {
      if (x.numbers.size < min) None else Option(x.numbers)
    }
  }

  implicit final class OptionalListChange(val x: Option[List[Int]]) extends AnyVal {
    def ~>(fn: List[Int] => List[Int]): Result = {
      x match {
        case Some(value) => fn(value).toResult
        case None => Left(StackUnderflow)
      }
    }
  }

  implicit final class ListChange(val x: List[Int]) extends AnyVal {
    def toResult = Right(NumbersStack(x))
  }

}
