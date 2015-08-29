package com.github.jparkie.bsd.statements

import com.github.jparkie.bsd.WhileKeywords
import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.{WhileBooleanType, WhileStatementType}

import scala.util.Try

case class WhileWriteBooleanStatement(booleanValue: WhileBooleanType) extends WhileStatementType {
  override def nodeName: String = classOf[WhileWriteBooleanStatement].getSimpleName

  override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Unit] = {
    for {
      actualBooleanValue <- booleanValue.evaluate()
    } yield Console.out.println(actualBooleanValue)
  }

  override def asWhile: Try[String] = {
    val booleanValueWhileTry = booleanValue.asWhile

    for {
      booleanValueWhile <- booleanValueWhileTry
    } yield
      s"""
        |${WhileKeywords.`WRITE`} $booleanValueWhile
      """.stripMargin
  }

  override def asScala: Try[String] = {
    val booleanValueScalaTry = booleanValue.asScala

    for {
      booleanValueScala <- booleanValueScalaTry
    } yield
      s"""
        |Console.out.println($booleanValueScala)
      """.stripMargin
  }
}
