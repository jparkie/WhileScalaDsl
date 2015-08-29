package com.github.jparkie.bsd.parser

import com.github.jparkie.bsd.literals.{WhileBooleanLiteral, WhileNumberLiteral}
import com.github.jparkie.bsd.operators._
import com.github.jparkie.bsd.programs.WhileProgram
import com.github.jparkie.bsd.statements._
import com.github.jparkie.bsd.types.{WhileBooleanType, WhileNumberType, WhileStatementType}
import com.github.jparkie.bsd.variables.WhileNumberVariable
import com.github.jparkie.bsd.{WhileKeywords, WhileRegex}

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

object WhileParser extends RegexParsers {
  import ExpressionGrammar._
  import LexicalGrammar._
  import LiteralGrammar._
  import OperatorGrammar._
  import ProgramGrammar._
  import StatementGrammar._
  import VariableGrammar._
  
  val TrySuccess = scala.util.Success
  val TryFailure = scala.util.Failure

  def parseWhileScript(whileScript: String): Try[WhileProgram] = {
    parseAll(whileProgram, whileScript) match {
      case Success(actualWhileProgram, _) => TrySuccess(actualWhileProgram)
      case Failure(errorMessage, _) => TryFailure(WhileParseException(errorMessage))
    }
  }

  object ProgramGrammar {
    lazy val whileProgram: Parser[WhileProgram] = WhileKeywords.`BEGIN` ~ allStatement ~ WhileKeywords.`END` ^^ {
      case WhileKeywords.`BEGIN` ~ whileStatement ~ WhileKeywords.`END` => WhileProgram(whileStatement)
    }
  }

  object StatementGrammar {
    lazy val allStatement: Parser[WhileStatementType] = statementPrecedence4

    lazy val statementPrecedence4: Parser[WhileStatementType] =
      multipleStatement |
      statementPrecedence3

    lazy val statementPrecedence3: Parser[WhileStatementType] =
      ifThenElseStatement |
      whileDoStatement |
      statementPrecedence2

    lazy val statementPrecedence2: Parser[WhileStatementType] =
      writeBooleanStatement |
      writeNumberStatement |
      readStatement |
      assignmentStatement |
      statementPrecedence1

    lazy val statementPrecedence1: Parser[WhileStatementType] =
      skipStatement

    lazy val assignmentStatement: Parser[WhileStatementType] = lexicalIdentifier ~ WhileKeywords.`:=` ~ numberExpression ^^ {
      case variableName ~ WhileKeywords.`:=` ~ variableValue => WhileAssignmentStatement(variableName, variableValue)
    }

    lazy val skipStatement: Parser[WhileStatementType] = WhileKeywords.`SKIP` ^^ {
      case WhileKeywords.`SKIP` => WhileSkipStatement
    }

    lazy val multipleStatement: Parser[WhileStatementType] = rep1sep(statementPrecedence3, WhileKeywords.`;`) ^^ {
      case whileStatements => WhileMultipleStatement(whileStatements)
    }

    lazy val ifThenElseStatement: Parser[WhileStatementType] = WhileKeywords.`IF` ~ booleanExpression ~ WhileKeywords.`THEN` ~ allStatement ~ WhileKeywords.`ELSE` ~ allStatement ^^ {
      case WhileKeywords.`IF` ~ booleanCondition ~ WhileKeywords.`THEN` ~ thenStatement ~ WhileKeywords.`ELSE` ~ elseStatement => WhileIfThenElseStatement(booleanCondition, thenStatement, elseStatement)
    }

    lazy val whileDoStatement: Parser[WhileStatementType] = WhileKeywords.`WHILE` ~ booleanExpression ~ WhileKeywords.`DO` ~ allStatement ^^ {
      case WhileKeywords.`WHILE` ~ booleanCondition ~ WhileKeywords.`DO` ~ doStatement => WhileWhileDoStatement(booleanCondition, doStatement)
    }

    lazy val writeBooleanStatement: Parser[WhileStatementType] = WhileKeywords.`WRITE` ~ booleanExpression ^^ {
      case WhileKeywords.`WRITE` ~ booleanValue => WhileWriteBooleanStatement(booleanValue)
    }

    lazy val writeNumberStatement: Parser[WhileStatementType] = WhileKeywords.`WRITE` ~ numberExpression ^^ {
      case WhileKeywords.`WRITE` ~ numberValue => WhileWriteNumberStatement(numberValue)
    }

    lazy val readStatement: Parser[WhileStatementType] = WhileKeywords.`READ` ~ lexicalIdentifier ^^ {
      case WhileKeywords.`READ` ~ variableName => WhileReadStatement(variableName)
    }
  }

  object ExpressionGrammar {
    lazy val booleanExpression: Parser[WhileBooleanType] = booleanPrecedence4

    lazy val booleanPrecedence4: Parser[WhileBooleanType] = chainl1(booleanPrecedence3,
      andOperator |
      orOperator
    )

    lazy val booleanPrecedence3: Parser[WhileBooleanType] =
      equalOperator |
      notEqualOperator |
      lessThanOperator |
      lessThanEqualOperator |
      greaterThanOperator |
      greaterThanEqualOperator |
      booleanPrecedence2

    lazy val booleanPrecedence2: Parser[WhileBooleanType] =
      notOperator |
      booleanPrecedence1

    lazy val booleanPrecedence1: Parser[WhileBooleanType] =
      literalBoolean |
      "(" ~> booleanExpression <~ ")"

    lazy val numberExpression: Parser[WhileNumberType] = numberPrecedence3

    lazy val numberPrecedence3: Parser[WhileNumberType] = chainl1(numberPrecedence2,
      differenceOperator |
      sumOperator
    )

    lazy val numberPrecedence2: Parser[WhileNumberType] = chainl1(numberPrecedence1,
        quotientOperator |
        productOperator |
        modulusOperator
    )

    lazy val numberPrecedence1: Parser[WhileNumberType] =
      literalNumber |
      variableNumber |
      "(" ~> numberExpression <~ ")"
  }

  object OperatorGrammar {
    lazy val andOperator: Parser[(WhileBooleanType, WhileBooleanType) => WhileBooleanType] = WhileKeywords.`AND` ^^^ {
      (firstValue: WhileBooleanType, secondValue: WhileBooleanType) => WhileAndOperator(firstValue, secondValue)
    }

    lazy val orOperator: Parser[(WhileBooleanType, WhileBooleanType) => WhileBooleanType] = WhileKeywords.`OR` ^^^ {
      (firstValue: WhileBooleanType, secondValue: WhileBooleanType) => WhileOrOperator(firstValue, secondValue)
    }

    lazy val equalOperator: Parser[WhileBooleanType] = numberExpression ~ WhileKeywords.`==` ~ numberExpression ^^ {
      case firstValue ~ WhileKeywords.`==` ~ secondValue => WhileEqualOperator(firstValue, secondValue)
    }

    lazy val notEqualOperator: Parser[WhileBooleanType] = numberExpression ~ WhileKeywords.`!=` ~ numberExpression ^^ {
      case firstValue ~ WhileKeywords.`!=` ~ secondValue => WhileNotEqualOperator(firstValue, secondValue)
    }

    lazy val lessThanOperator: Parser[WhileBooleanType] = numberExpression ~ WhileKeywords.`<` ~ numberExpression ^^ {
      case firstValue ~ WhileKeywords.`<` ~ secondValue => WhileLesserThanOperator(firstValue, secondValue)
    }

    lazy val lessThanEqualOperator: Parser[WhileBooleanType] = numberExpression ~ WhileKeywords.`<=` ~ numberExpression ^^ {
      case firstValue ~ WhileKeywords.`<=` ~ secondValue => WhileLesserThanEqualOperator(firstValue, secondValue)
    }

    lazy val greaterThanOperator: Parser[WhileBooleanType] = numberExpression ~ WhileKeywords.`>` ~ numberExpression ^^ {
      case firstValue ~ WhileKeywords.`>` ~ secondValue => WhileGreaterThanOperator(firstValue, secondValue)
    }

    lazy val greaterThanEqualOperator: Parser[WhileBooleanType] = numberExpression ~ WhileKeywords.`>=` ~ numberExpression ^^ {
      case firstValue ~ WhileKeywords.`>=` ~ secondValue => WhileGreaterThanEqualOperator(firstValue, secondValue)
    }
    
    lazy val notOperator: Parser[WhileBooleanType] = WhileKeywords.`NOT` ~ booleanExpression ^^ {
      case WhileKeywords.`NOT` ~ firstValue => WhileNotOperator(firstValue)
    }
    
    lazy val differenceOperator: Parser[(WhileNumberType, WhileNumberType) => WhileNumberType] = WhileKeywords.`-` ^^^ {
      (firstValue: WhileNumberType, secondValue: WhileNumberType) => WhileDifferenceOperator(firstValue, secondValue)
    }

    lazy val sumOperator: Parser[(WhileNumberType, WhileNumberType) => WhileNumberType] = WhileKeywords.`+` ^^^ {
      (firstValue: WhileNumberType, secondValue: WhileNumberType) => WhileSumOperator(firstValue, secondValue)
    }

    lazy val quotientOperator: Parser[(WhileNumberType, WhileNumberType) => WhileNumberType] = WhileKeywords.`/` ^^^ {
      (firstValue: WhileNumberType, secondValue: WhileNumberType) => WhileQuotientOperator(firstValue, secondValue)
    }

    lazy val productOperator: Parser[(WhileNumberType, WhileNumberType) => WhileNumberType] = WhileKeywords.`*` ^^^ {
      (firstValue: WhileNumberType, secondValue: WhileNumberType) => WhileProductOperator(firstValue, secondValue)
    }

    lazy val modulusOperator: Parser[(WhileNumberType, WhileNumberType) => WhileNumberType] = WhileKeywords.`%` ^^^ {
      (firstValue: WhileNumberType, secondValue: WhileNumberType) => WhileModulusOperator(firstValue, secondValue)
    }
  }

  object VariableGrammar {
    lazy val variableNumber: Parser[WhileNumberType] = lexicalIdentifier ^^ {
      case variableName => WhileNumberVariable(variableName)
    }
  }

  object LiteralGrammar {
    lazy val literalBoolean: Parser[WhileBooleanType] = lexicalBoolean ^^ {
      case booleanValue => WhileBooleanLiteral(booleanValue)
    }

    lazy val literalNumber: Parser[WhileNumberType] = lexicalNumber ^^ {
      case numberValue => WhileNumberLiteral(numberValue)
    }
  }
  
  object LexicalGrammar {
    lazy val lexicalIdentifier: Parser[String] = WhileRegex.IDENTIFIER ^^ (_.toString)
    
    lazy val lexicalBoolean: Parser[Boolean] = WhileRegex.BOOLEAN ^^ (_.toBoolean)
    
    lazy val lexicalNumber: Parser[Double] = WhileRegex.NUMBER ^^ (_.toDouble)
  }
}
