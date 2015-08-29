package com.github.jparkie.bsd.variables

import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.WhileNumberType

import scala.util.{Failure, Success, Try}

case class WhileNumberVariable(variableName: String) extends WhileNumberType {
   override def nodeName: String = classOf[WhileNumberVariable].getSimpleName

   override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Double] = {
     try {
       Success(whileEnvironment(variableName).asInstanceOf[Double])
     } catch {
       case exception: Throwable =>
         Failure(exception)
     }
   }

   override def asWhile: Try[String] = {
     Success(s"$variableName")
   }

   override def asScala: Try[String] = {
     Success(s"`$variableName`")
   }
 }
