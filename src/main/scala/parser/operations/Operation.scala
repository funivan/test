package parser.operations

import definitions.Operation.Result
import definitions.{Operation, Error, Stack}

case class Next(a: Operation, b: Operation) extends Operation {
  override def compute(items: Stack): Result = {
    a.compute(items) match {
      case Right(value) => b.compute(value)
      case err => err
    }
  }
}

object Empty extends Operation {
  override def compute(stack: Stack): Result = Right(stack)
}

case class ErrorOperation(error: Error) extends Operation {
  override def compute(items: Stack): Result = Left(error)
}
