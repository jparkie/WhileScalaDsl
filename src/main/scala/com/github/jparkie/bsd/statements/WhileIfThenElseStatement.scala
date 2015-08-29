package com.github.jparkie.bsd.statements

import com.github.jparkie.bsd.WhileKeywords
import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.{WhileBooleanType, WhileStatementType}

import scala.util.Try

case class WhileIfThenElseStatement(booleanCondition: WhileBooleanType, thenStatement: WhileStatementType, elseStatement: WhileStatementType) extends WhileStatementType {
  override def nodeName: String = classOf[WhileIfThenElseStatement].getSimpleName

  override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Unit] = {
    for {
      actualBooleanCondition <- booleanCondition.evaluate()
      actualEvaluation <- {
        if (actualBooleanCondition)
          thenStatement.evaluate()
        else
          elseStatement.evaluate()
      }
    } yield actualEvaluation
  }

  override def asWhile: Try[String] = {
    val booleanConditionWhileTry = booleanCondition.asWhile
    val thenStatementWhileTry = thenStatement.asWhile
    val elseStatementWhileTry = elseStatement.asWhile

    for {
      booleanConditionWhile <- booleanConditionWhileTry
      thenStatementWhile <- thenStatementWhileTry
      elseStatementWhile <- elseStatementWhileTry
    } yield
      s"""
        |${WhileKeywords.`IF`} $booleanConditionWhile
        | ${WhileKeywords.`THEN`}
        |   $thenStatementWhile
        | ${WhileKeywords.`ELSE`}
        |   $elseStatementWhile
      """.stripMargin
  }

  override def asScala: Try[String] = {
    val booleanConditionScalaTry = booleanCondition.asScala
    val thenStatementScalaTry = thenStatement.asScala
    val elseStatementScalaTry = elseStatement.asScala

    for {
      booleanConditionScala <- booleanConditionScalaTry
      thenStatementScala <- thenStatementScalaTry
      elseStatementScala <- elseStatementScalaTry
    } yield
      s"""
        |if ($booleanConditionScala) {
        | $thenStatementScala
        |} else {
        | $elseStatementScala
        |}
      """.stripMargin
  }
}
