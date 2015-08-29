package com.github.jparkie.bsd.types

import com.github.jparkie.bsd.WhileAbstractSyntaxTreeNode

trait WhileStatementType extends WhileAbstractSyntaxTreeNode[Unit] {
  override def nodeName: String = classOf[WhileStatementType].getSimpleName
}
