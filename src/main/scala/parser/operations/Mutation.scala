package parser.operations

import definitions.Operation.Result
import definitions.{Operation, Stack}
import parser.IntListManipulation

case class Push(int: Int) extends Operation {
  override def compute(stack: Stack): Result = (stack.numbers :+ int).toResult
}
object Duplicate extends Operation {
  override def compute(stack: Stack): Result = (stack |> 1) ~> (_.copyRight(1))
}
object Drop extends Operation {
  override def compute(stack: Stack): Result = (stack |> 1) ~> (_.dropRight(1))
}
object Swap extends Operation {
  override def compute(stack: Stack): Result = {
    (stack |> 2) ~> (numbers => {
      numbers.dropRight(2) ::: numbers.takeRight(2).reverse
    })
  }
}
object Over extends Operation {
  override def compute(stack: Stack): Result = (stack |> 2) ~> (_.copyRight(2))
}
