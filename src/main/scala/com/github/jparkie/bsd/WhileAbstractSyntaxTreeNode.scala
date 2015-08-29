package com.github.jparkie.bsd

import com.github.jparkie.bsd.interpreter.WhileEnvironment

import scala.util.Try

trait WhileAbstractSyntaxTreeNode[T] {
  def nodeName: String

  def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[T]

  def asWhile: Try[String]

  def asScala: Try[String]
}