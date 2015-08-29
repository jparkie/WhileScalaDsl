package com.github.jparkie.bsd.statements

import com.github.jparkie.bsd.WhileKeywords
import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.{WhileNumberType, WhileStatementType}

import scala.util.Try

case class WhileWriteNumberStatement(numberValue: WhileNumberType) extends WhileStatementType {
  override def nodeName: String = classOf[WhileWriteNumberStatement].getSimpleName

  override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Unit] = {
    for {
      actualNumberValue <- numberValue.evaluate()
    } yield Console.out.println(actualNumberValue)
  }

  override def asWhile: Try[String] = {
    val numberValueWhileTry = numberValue.asWhile

    for {
      numberValueWhile <- numberValueWhileTry
    } yield
      s"""
        |${WhileKeywords.`WRITE`} $numberValueWhile
      """.stripMargin
  }

  override def asScala: Try[String] = {
    val numberValueScalaTry = numberValue.asScala

    for {
      numberValueScala <- numberValueScalaTry
    } yield
      s"""
        |Console.out.println($numberValueScala)
      """.stripMargin
  }
}
