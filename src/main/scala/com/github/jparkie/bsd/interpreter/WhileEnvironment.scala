package com.github.jparkie.bsd.interpreter

import scala.collection.mutable

case class WhileEnvironment(defaultValueMap: Map[String, Any] = Map.empty)
  extends mutable.HashMap[String, Any] {
  defaultValueMap foreach { currentKeyValuePair =>
    this.put(currentKeyValuePair._1, currentKeyValuePair._2)
  }
}