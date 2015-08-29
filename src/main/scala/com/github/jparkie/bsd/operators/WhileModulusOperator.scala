package com.github.jparkie.bsd.operators

import com.github.jparkie.bsd.WhileKeywords
import com.github.jparkie.bsd.interpreter.WhileEnvironment
import com.github.jparkie.bsd.types.WhileNumberType

import scala.util.Try

case class WhileModulusOperator(leftArgument: WhileNumberType, rightArgument: WhileNumberType) extends WhileNumberType {
   override def nodeName: String = classOf[WhileModulusOperator].getSimpleName

   override def evaluate()(implicit whileEnvironment: WhileEnvironment): Try[Double] = {
     val leftIdentityTry = leftArgument.evaluate()
     val rightIdentityTry = rightArgument.evaluate()

     for {
       leftIdentity <- leftIdentityTry
       rightIdentity <- rightIdentityTry
     } yield leftIdentity % rightIdentity
   }

   override def asWhile: Try[String] = {
     val leftArgumentWhileTry = leftArgument.asWhile
     val rightArgumentWhileTry = rightArgument.asWhile

     for {
       leftArgumentWhile <- leftArgumentWhileTry
       rightArgumentWhile <- rightArgumentWhileTry
     } yield s"$leftArgumentWhile ${WhileKeywords.`%`} $rightArgumentWhile"
   }

   override def asScala: Try[String] = {
     val leftArgumentScalaTry = leftArgument.asScala
     val rightArgumentScalaTry = rightArgument.asScala

     for {
       leftArgumentScala <- leftArgumentScalaTry
       rightArgumentScala <- rightArgumentScalaTry
     } yield s"($leftArgumentScala) % ($rightArgumentScala)"
   }
 }
