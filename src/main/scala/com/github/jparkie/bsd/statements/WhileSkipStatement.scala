package com.github.jparkie.bsd.statements

import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.WhileStatementType

import scala.util.{Success, Try}

case object WhileSkipStatement extends WhileStatementType {
  override def nodeName: String = WhileSkipStatement.getClass.getSimpleName

  override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Unit] = {
    Success(Unit)
  }

  override def asWhile: Try[String] = {
    Success(
      """
        |skip
      """.stripMargin)
  }

  override def asScala: Try[String] = {
    Success("")
  }
}
