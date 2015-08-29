package com.github.jparkie.bsd.literals

import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.WhileNumberType

import scala.util.{Success, Try}

case class WhileNumberLiteral(literalNumber: Double) extends WhileNumberType {
  override def nodeName: String = classOf[WhileNumberLiteral].getSimpleName

  override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Double] = {
    Success(literalNumber)
  }

  override def asWhile: Try[String] = {
    Success(s"$literalNumber")
  }

  override def asScala: Try[String] = {
    Success(s"$literalNumber")
  }
}
