package com.github.jparkie.bsd.statements

import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.{WhileNumberType, WhileStatementType}

import scala.util.{Failure, Success, Try}

case class WhileAssignmentStatement(variableName: String, variableValue: WhileNumberType) extends WhileStatementType {
  override def nodeName: String = classOf[WhileAssignmentStatement].getSimpleName

  override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Unit] = {
    val variableValueTry = variableValue.evaluate()

    variableValueTry match {
      case Success(actualVariableValue) =>
        whileEnvironment.put(variableName, actualVariableValue)
        Success(Unit)
      case Failure(exception) =>
        Failure(exception)
    }
  }

  override def asWhile: Try[String] = {
    val variableValueWhileTry = variableValue.asWhile

    for {
      variableValueWhile <- variableValueWhileTry
    } yield
      s"""
        |$variableName := $variableValueWhile
      """.stripMargin
  }

  override def asScala: Try[String] = {
    val variableValueScalaTry = variableValue.asScala

    for {
      variableValueScala <- variableValueScalaTry
    } yield
      s"""
        |var `$variableName` = ($variableValueScala)
      """.stripMargin
  }
}
