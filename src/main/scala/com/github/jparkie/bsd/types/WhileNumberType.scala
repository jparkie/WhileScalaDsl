package com.github.jparkie.bsd.types

import com.github.jparkie.bsd.WhileAbstractSyntaxTreeNode

trait WhileNumberType extends WhileAbstractSyntaxTreeNode[Double] {
  override def nodeName: String = classOf[WhileNumberType].getSimpleName
}
