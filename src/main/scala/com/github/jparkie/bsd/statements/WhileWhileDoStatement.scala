package com.github.jparkie.bsd.statements

import com.github.jparkie.bsd.WhileKeywords
import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.{WhileBooleanType, WhileStatementType}

import scala.util.{Success, Try}

case class WhileWhileDoStatement(booleanCondition: WhileBooleanType, doStatement: WhileStatementType) extends WhileStatementType {
  override def nodeName: String = classOf[WhileWhileDoStatement].getSimpleName

  override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Unit] = {
    booleanCondition.evaluate()
      .flatMap { actualBooleanCondition =>
      if (actualBooleanCondition) {
        val actualEvaluationTry = doStatement.evaluate()
        if (actualEvaluationTry.isSuccess)
          evaluate()
        else
          actualEvaluationTry
      } else {
        Success(Unit)
      }
    }
  }

  override def asWhile: Try[String] = {
    val booleanConditionWhileTry = booleanCondition.asWhile
    val doStatementWhileTry = doStatement.asWhile

    for {
      booleanConditionWhile <- booleanConditionWhileTry
      doStatementWhile <- doStatementWhileTry
    } yield
      s"""
        |${WhileKeywords.`WHILE`} $booleanConditionWhile
        | ${WhileKeywords.`DO`}
        |   $doStatementWhile
      """.stripMargin
  }

  override def asScala: Try[String] = {
    val booleanConditionScalaTry = booleanCondition.asScala
    val doStatementScalaTry = doStatement.asScala

    for {
      booleanConditionScala <- booleanConditionScalaTry
      doStatementScala <- doStatementScalaTry
    } yield
      s"""
        |while ($booleanConditionScala) {
        | $doStatement
        |}
      """.stripMargin
  }
}
