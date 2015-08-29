package com.github.jparkie.bsd.literals

import com.github.jparkie.bsd.WhileKeywords
import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.WhileBooleanType

import scala.util.{Success, Try}

case class WhileBooleanLiteral(literalValue: Boolean) extends WhileBooleanType {
  override def nodeName: String = classOf[WhileBooleanLiteral].getSimpleName

  override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Boolean] = {
    Success(literalValue)
  }

  override def asWhile: Try[String] = {
    if (literalValue)
      Success(s"${WhileKeywords.`TRUE`}")
    else
      Success(s"${WhileKeywords.`FALSE`}")
  }

  override def asScala: Try[String] = {
    Success(s"$literalValue")
  }
}
