package parser

import definitions.{Operation, SemigroupSyntax, UnknownWord, UserInstructions}
import instances.mOperation
import parser.operations._
import parser.tokenizer.Token
import parser.transformer.Transformer

case class Parser(private val transformer: Transformer) {

  def parse(tokens: List[Token], userInstructions: UserInstructions): Operation = {
    val res: Option[StepResult] = transformer.transform(tokens, userInstructions, this)
    res match {
      case Some(value) => value.next match {
        case Nil => value.operation
        case list => value.operation |+| parse(list, value.userInstructions)
      }
      case None => ErrorOperation(UnknownWord)
    }
  }
}
