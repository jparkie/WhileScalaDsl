package com.github.jparkie.bsd.statements

import com.github.jparkie.bsd.interpreter.{WhileEnvironment, WhileInterpretException}
import com.github.jparkie.bsd.parser.WhileParseException
import com.github.jparkie.bsd.types.WhileStatementType

import scala.util.{Failure, Success, Try}

case class WhileMultipleStatement(whileStatementTypes: Seq[WhileStatementType]) extends WhileStatementType {
  override def nodeName: String = classOf[WhileMultipleStatement].getSimpleName

  override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Unit] = {
    val whileStatementEvaluateTries = for {
      currentWhileStatementType <- whileStatementTypes
    } yield currentWhileStatementType.evaluate()

    if (whileStatementEvaluateTries.forall(_.isSuccess))
      Success(Unit)
    else
      Failure(WhileInterpretException(s"$nodeName: Failed evaluate()."))
  }

  override def asWhile: Try[String] = {
    val whileStatementWhileTries = whileStatementTypes
      .map(_.asWhile)

    if (whileStatementWhileTries.forall(_.isSuccess))
      Success(
        whileStatementWhileTries
          .map(_.get)
          .foldLeft("")((resultWhile, currentWhile) => resultWhile + currentWhile + ";")
          .dropRight(";".length)
      )
    else
      Failure(WhileParseException(s"$nodeName: Failed asWhile()."))
  }

  override def asScala: Try[String] = {
    val whileStatementScalaTries = whileStatementTypes
      .map(_.asScala)

    if (whileStatementScalaTries.forall(_.isSuccess))
      Success(
        whileStatementScalaTries
          .map(_.get)
          .foldLeft("")((resultWhile, currentWhile) => resultWhile + currentWhile + "\n")
      )
    else
      Failure(WhileParseException(s"$nodeName: Failed asScala()."))
  }
}
