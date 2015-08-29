package com.github.jparkie.bsd

import com.github.jparkie.bsd.compiler.WhileCompiler
import com.github.jparkie.bsd.interpreter.WhileInterpreter
import com.github.jparkie.bsd.parser.WhileParser

object WhileApplication extends App {
  val whileScript =
    """
      |BEGIN
      |variable1 := 1.0;
      |variable2 := 2.0;
      |IF variable2 > variable1 THEN
      | WRITE true
      |ELSE
      | WRITE false
      |END
    """.stripMargin

  // WhileParser:

  val whileProgram = WhileParser
    .parseWhileScript(whileScript)
    .get

  // WhileInterpreter:

  WhileInterpreter.run(whileProgram)
  
  // WhileCompiler:
  
  val whileTemplate = WhileCompiler
    .compileWhileTemplate(whileProgram)
    .get
  
  WhileCompiler.run(whileTemplate)
}
