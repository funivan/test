import ForthError.ForthError

sealed trait Operation {
  def compute(items: Stack): Either[ForthError, Stack];
  def compute(): Either[ForthError, Stack] = compute(Stack())
}

case class Next(a: Operation, b: Operation) extends Operation {
  override def compute(items: Stack): Either[ForthError, Stack] = {
    a.compute(items) match {
      case Right(value) => b.compute(value)
      case err          => err
    }
  }
}

trait Computation extends Operation {
  val fn: (List[Int]) => List[Int]
  override def compute(stack: Stack): Either[ForthError, Stack] = {
    stack.numbers match {
      case Nil => Left(ForthError.StackUnderflow)
      case v   => Right(Stack(fn(v)))
    }
  }
}

case class Push(int: Int) extends Operation {
  override def compute(stack: Stack): Either[ForthError, Stack] = {
    Right(Stack(stack.numbers :+ int))
  }
}

case class Duplicate() extends Computation {
  val fn = (v) => { v :+ v.last }
}
case class Drop() extends Computation {
  val fn = _.dropRight(1)
}

case class Divide() extends Operation {
  override def compute(stack: Stack): Either[ForthError, Stack] = {
    stack.numbers match {
      case Nil           => Left(ForthError.StackUnderflow)
      case _ :: Nil      => Left(ForthError.StackUnderflow)
      case _ :: 0 :: Nil => Left(ForthError.DivisionByZero)
      case numbers       => Right(Stack(List(numbers.reduce(_ / _))))
    }
  }
}
case class Swap() extends Operation {
  override def compute(stack: Stack): Either[ForthError, Stack] = {
    stack.numbers match {
      case Nil      => Left(ForthError.StackUnderflow)
      case _ :: Nil => Left(ForthError.StackUnderflow)
      case numbers: List[Int] => {
        val right = numbers.takeRight(2).reverse
        Right(Stack(numbers.dropRight(2) ::: right))
      }
    }
  }
}

case class Over() extends Operation {
  override def compute(stack: Stack): Either[ForthError, Stack] = {
    stack.numbers match {
      case Nil      => Left(ForthError.StackUnderflow)
      case _ :: Nil => Left(ForthError.StackUnderflow)
      case numbers: List[Int] => {
        Right(Stack(numbers :+ numbers(numbers.size - 2)))
      }
    }
  }
}
case class Empty() extends Operation{
  override def compute(stack: Stack): Either[ForthError, Stack] = {
    Right(stack)
  }
}
case class Math(f: (Int, Int) => Int) extends Operation {
  override def compute(stack: Stack): Either[ForthError, Stack] = {
    stack.numbers match {
      case Nil      => Left(ForthError.StackUnderflow)
      case _ :: Nil => Left(ForthError.StackUnderflow)
      case numbers  => Right(Stack(List(numbers.reduce(f))))
    }
  }
}

case class Error(error: ForthError) extends Operation {
  override def compute(items: Stack): Either[ForthError, Stack] = {
    Left(error)
  }
}

object Operation {
  implicit class ItemCombine(x: Operation) {
    def |+|(y: Operation) = Next(x, y)
  }
}
