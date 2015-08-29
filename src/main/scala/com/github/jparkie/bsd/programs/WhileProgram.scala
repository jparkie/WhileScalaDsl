package com.github.jparkie.bsd.programs

import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.WhileStatementType
import com.github.jparkie.bsd.{WhileAbstractSyntaxTreeNode, WhileKeywords}

import scala.util.Try

case class WhileProgram(whileStatement: WhileStatementType) extends WhileAbstractSyntaxTreeNode[Unit] {
  override def nodeName: String = classOf[WhileProgram].getClass.getSimpleName

  override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Unit] = {
    whileStatement.evaluate()
  }

  override def asWhile: Try[String] = {
    val whileStatementWhileTry = whileStatement.asWhile

    for {
      whileStatementWhile <- whileStatementWhileTry
    } yield
      s"""
         |${WhileKeywords.`BEGIN`}
         |$whileStatementWhile
         |${WhileKeywords.`END`}
       """.stripMargin
  }

  override def asScala: Try[String] = {
    val whileStatementScalaTry = whileStatement.asScala

    for {
      whileStatementScala <- whileStatementScalaTry
    } yield
      s"""
         |$whileStatementScala
       """.stripMargin
  }
}
