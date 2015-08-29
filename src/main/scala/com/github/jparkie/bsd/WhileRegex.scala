package com.github.jparkie.bsd

object WhileRegex {
  val IDENTIFIER = """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

  val BOOLEAN = """(true|false)""".r

  val NUMBER = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
}
