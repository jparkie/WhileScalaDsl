package com.github.jparkie.bsd.compiler

import com.github.jparkie.bsd.programs.WhileProgram

import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.util.{Try, Failure, Success}

object WhileCompiler extends WhileCompilerQuasiquotes {
  val whileTemplateType = typeOf[WhileTemplate]

  def getMirror: Mirror =
    runtimeMirror(Thread.currentThread().getContextClassLoader)

  val generateTemplateToolbox = getMirror.mkToolBox()

  def compileWhileTemplate(whileProgram: WhileProgram): Try[WhileTemplate] = {
    def generateClassString(whileScalaString: String): String = {
      val temporaryString = s"new $whileTemplateType { override def run(): Unit = { $whileScalaString } }"

      temporaryString
    }

    def generateClassAST(whileClassString: String): Tree = {
      val temporaryTree = generateTemplateToolbox.parse(whileClassString)

      temporaryTree
    }

    def generateWhileTemplate(whileClassTree: Tree): WhileTemplate = {
      val temporaryMethod = generateTemplateToolbox.compile(whileClassTree)

      temporaryMethod().asInstanceOf[WhileTemplate]
    }

    try {
      whileProgram.asScala
        .map(generateClassString(_))
        .map(generateClassAST(_))
        .map(generateWhileTemplate(_))
    } catch {
      case exception: Exception => Failure(exception)
    }
  }

  def run(whileTemplate: WhileTemplate): Unit = {
    Console.out.println(
      s"""
         |//
         |// WhileCompiler
         |//
         |// Name: Jacob Park
         |// Date: Friday, August 28, 2015
         |//
       """.stripMargin
    )

    val executionResult = Try(whileTemplate.run())

    executionResult match {
      case Success(_) =>
        Console.out.println("Program exited upon completion.")
      case Failure(exception) =>
        Console.out.println(s"Program exited upon failure: ${exception.getMessage}.")
    }
  }
}
