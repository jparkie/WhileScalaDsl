package com.github.jparkie.bsd.types

import com.github.jparkie.bsd.WhileAbstractSyntaxTreeNode

trait WhileBooleanType extends WhileAbstractSyntaxTreeNode[Boolean] {
  override def nodeName: String = classOf[WhileBooleanType].getSimpleName
}
