/* Typescript to scala macro
 * Copyright 2013 Apyx
 * @author  Arnaud PEZEL
 */

package org.jscala.typescript

import com.apyx.scala.ts2scala.dialect._
import com.apyx.scala.ts2scala.definition.{Name, QualifiedName}

object JScalaDialect extends Dialect {
	import com.apyx.scala.ts2scala.definition.{QualifiedName, Name}

	val root = QualifiedName(Name("org"), Name("jscala"))
	
	override def dictionnary():Map[Name, QualifiedName] = {
		return Map(
				Name("String") -> (root dot Name("String")),
				Name("RegExp") -> (root dot Name("RegExp")),
				Name("Dynamic") -> (root dot Name("Dynamic")),
				Name("Node") -> (root dot Name("Node")),
				Name("NodeMap") -> (root dot Name("NodeMap")),
				Name("NodeList") -> (root dot Name("NodeList")),
				Name("Attribute") -> (root dot Name("Attribute")),
				Name("Element") -> (root dot Name("Element")),
				Name("Document") -> (root dot Name("Doc")),
				/* TODO map this types */
				Name("Location") -> QualifiedName.Any,
				Name("Window") -> QualifiedName.Any,
				Name("HTMLElement") -> QualifiedName.Any,
				Name("Event") -> QualifiedName.Any
				)
	}
}