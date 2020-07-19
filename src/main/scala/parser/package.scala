import definitions.UserInstructions
import parser.tokenizer.Token
import parser.transformer.Transformer

package object parser {
  implicit final class IntListManipulation(val x: List[Int]) extends AnyVal {
    def copyRight(index: Int): List[Int] = {
      x :+ x(x.size - index)
    }
  }
  implicit final class IntManipulation(val x : Int) extends AnyVal{
    def toList = List(x)
  }
  implicit class FnCombine(x: Transformer) {
    def <|>(y: Transformer): Transformer = {
      (value, instructions, parser) => x.transform(value, instructions, parser) orElse y.transform(value, instructions, parser)
    }
  }
}
