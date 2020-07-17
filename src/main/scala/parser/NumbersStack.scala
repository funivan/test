package parser

import definitions.Stack


case class NumbersStack(numbers: List[Int] = List()) extends Stack {
  override def toString: String = numbers.mkString(" ")
}
