package parser

import definitions.{InvalidWord, Operation, SemigroupSyntax, UnknownWord, UserInstructions}
import instances.mOperation
import parser.operations._
import parser.tokenizer._

object Parser {
  def parseInstruction(
                        chars: List[Token],
                        instructions: UserInstructions): StepResult = {
    val raw = chars.takeWhile {
      !_.isInstanceOf[T_SEMICOLON]
    }
    raw match {
      case InstructionIdentifier(head) :: tail =>
        val operation = parse(tail, instructions)
        StepResult(
          Empty,
          chars.drop(raw.size + 1),
          instructions + (head -> operation)
        )
      case _ => StepResult(InvalidWord)
    }

  }

  def parse(tokens: List[Token],
            userInstructions: UserInstructions): Operation = {
    val processed = (x: Operation) => StepResult(x, tokens.tail, userInstructions)
    val res: StepResult = tokens.head match {
      case T_COLON() => parseInstruction(tokens.tail, userInstructions)
      case InstructionIdentifier(id) if userInstructions.contains(id) =>
        processed(userInstructions(id))
      case T_NUMBER(x) => processed(Push(x))
      case T_OPERATION("*") => processed(Math(_ * _))
      case T_OPERATION("+") => processed(Math(_ + _))
      case T_OPERATION("-") => processed(Math(_ - _))
      case T_OPERATION("/") => processed(Divide)
      case T_WORD("dup") => processed(Duplicate)
      case T_WORD("drop") => processed(Drop)
      case T_WORD("swap") => processed(Swap)
      case T_WORD("over") => processed(Over)
      case _ => StepResult(UnknownWord)
    }
    res.next match {
      case Nil  => res.operation
      case list => res.operation |+| parse(list, res.userInstructions)
    }
  }
}
