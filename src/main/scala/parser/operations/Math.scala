package parser.operations

import definitions.Operation.Result
import definitions.{DivisionByZero, Operation, Stack, StackUnderflow}
import parser.{IntManipulation, NumbersStack}

case class Math(f: (Int, Int) => Int) extends Operation {
  override def compute(stack: Stack): Result = (stack |> 2) ~> (_.reduce(f).toList)
}

object Divide extends Operation {
  override def compute(stack: Stack): Result = {
    stack |> 2 match {
      case None => Left(StackUnderflow)
      case Some(_ :: 0 :: Nil) => Left(DivisionByZero)
      case Some(numbers) => Right(NumbersStack(List(numbers.reduce(_ / _))))
    }
  }
}
