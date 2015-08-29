import sbt._

object Dependencies {
  // Versions:
  val SCALA_PARSER_COMBINATOR_VERSION = "1.0.4"
  val SCALA_COMPILER_VERSION = "2.11.7"
  val SCALA_REFLECT_VERSION = "2.11.7"
  val SCALA_LIBRARY_VERSION = "2.11.7"
  val SCALATEST_VERSION = "2.2.4"

  // Libraries:
  val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % SCALA_PARSER_COMBINATOR_VERSION
  val scalaCompiler          = "org.scala-lang"          % "scala-compiler"           % SCALA_COMPILER_VERSION
  val scalaReflect           = "org.scala-lang"          % "scala-reflect"            % SCALA_REFLECT_VERSION
  val scalaLibrary           = "org.scala-lang"          % "scala-library"            % SCALA_LIBRARY_VERSION
  val scalatest              = "org.scalatest"          %% "scalatest"                % SCALATEST_VERSION % "test"

  // Project Dependencies:
  val projectDependencies = Seq(scalaCompiler, scalaReflect, scalaLibrary, scalaParserCombinators, scalatest)
}