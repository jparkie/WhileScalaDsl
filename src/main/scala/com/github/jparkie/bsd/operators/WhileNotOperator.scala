package com.github.jparkie.bsd.operators

import com.github.jparkie.bsd.WhileKeywords
import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.WhileBooleanType

import scala.util.Try

case class WhileNotOperator(unaryArgument: WhileBooleanType) extends WhileBooleanType {
  override def nodeName: String = classOf[WhileNotOperator].getSimpleName

  override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Boolean] = {
    val unaryIdentityTry = unaryArgument.evaluate()

    for {
      unaryIdentity <- unaryIdentityTry
    } yield !unaryIdentity
  }

  override def asWhile: Try[String] = {
    val unaryArgumentWhileTry = unaryArgument.asWhile

    for {
      unaryArgumentWhile <- unaryArgumentWhileTry
    } yield s"${WhileKeywords.`NOT`} $unaryArgumentWhile"
  }

  override def asScala: Try[String] = {
    val unaryArgumentScalaTry = unaryArgument.asScala

    for {
      unaryArgumentScala <- unaryArgumentScalaTry
    } yield s"!($unaryArgumentScala)"
  }
}
