package com.github.jparkie.bsd.statements

import com.github.jparkie.bsd.WhileKeywords
import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.WhileStatementType

import scala.util.{Failure, Success, Try}

case class WhileReadStatement(variableName: String) extends WhileStatementType {
  override def nodeName: String = classOf[WhileReadStatement].getSimpleName

  override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Unit] = {
    val temporaryInput = Console.in.readLine()

    val doubleValueTry = Try(temporaryInput.toDouble)
    doubleValueTry match {
      case Success(actualValue) =>
        whileEnvironment.put(variableName, doubleValueTry.get)
        Success(Unit)
      case Failure(exception) =>
        Failure(exception)
    }
  }

  override def asWhile: Try[String] = {
    Success(s"${WhileKeywords.`READ`} $variableName")
  }

  override def asScala: Try[String] = {
    Success(
      s"""
        |var $variableName = Console.in.readLine().toDouble
      """.stripMargin
    )
  }
}
