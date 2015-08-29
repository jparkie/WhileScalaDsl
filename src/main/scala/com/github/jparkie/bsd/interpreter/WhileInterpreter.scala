package com.github.jparkie.bsd.interpreter

import com.github.jparkie.bsd.programs.WhileProgram

import scala.util.{Failure, Success}

object WhileInterpreter {
  def run(whileProgram: WhileProgram): Unit = {
    Console.out.println(
      s"""
         |//
         |// WhileInterpreter
         |//
         |// Name: Jacob Park
         |// Date: Friday, August 28, 2015
         |//
       """.stripMargin
    )

    val executionResult = whileProgram.evaluate()(WhileEnvironment())

    executionResult match {
      case Success(_) =>
        Console.out.println("Program exited upon completion.")
      case Failure(exception) =>
        Console.out.println(s"Program exited upon failure: ${exception.getMessage}.")
    }
  }
}
