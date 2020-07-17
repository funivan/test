package object parser {
  implicit final class IntListManipulation(val x: List[Int]) extends AnyVal {
    def copyRight(index: Int): List[Int] = {
      x :+ x(x.size - index)
    }
  }
  implicit final class IntManipulation(val x : Int) extends AnyVal{
    def toList = List(x)
  }
}
