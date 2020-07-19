package parser

import definitions.Operation.Result
import definitions.{Program, UserInstructions}
import parser.tokenizer.Tokenizer.tokenize
import parser.transformer.{Main, ProcessInstructions, ScanInstructions}

class Forth extends Program {

  override def eval(text: String): Result = {
    Parser(ScanInstructions <|> ProcessInstructions <|> Main)
      .parse(
        tokenize(text.split(" ").toList),
        new UserInstructions()
      ).compute(NumbersStack())
  }
}
