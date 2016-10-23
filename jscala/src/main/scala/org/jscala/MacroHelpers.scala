package org.jscala

import scala.collection.generic.{MapFactory, SeqFactory}
import scala.reflect.macros.blackbox

/**
 * Author: Alexander Nemish
 * Date: 10/25/13
 * Time: 10:35 PM
 */
trait MacroHelpers[C <: blackbox.Context] {
  val c: C
  import c.universe._
  type PFT[A] = PartialFunction[Tree, A]
  type ToExpr[A] = PFT[Tree]

  implicit class TreeHelper(tree: Tree) {
    def is(p: String): Boolean = tree.equalsStructure(select(p)) || tree.equalsStructure(select(p, s => This(TypeName(s))))
    lazy val isArrow: Boolean = is("scala.Predef.ArrowAssoc") /*2.11.x*/
    def raw: String = showRaw(tree)
    def isNum = tree.tpe.widen weak_<:< typeOf[Long]
    def isCaseClass = tree.tpe.typeSymbol.isClass && tree.tpe.typeSymbol.asClass.isCaseClass
  }

  implicit class NameHelper(name: Name) {
    def isArrow: Boolean = name.decodedName.toString == "->" || name.decodedName.toString == "→"
  }

  object Name {
    def unapply(name: Name): Option[String] = Some(name.decodedName.toString)
  }

  protected lazy val seqFactorySym = c.typeOf[SeqFactory[Seq]].typeSymbol
  protected lazy val mapFactorySym = c.typeOf[MapFactory[collection.Map]].typeSymbol
  protected lazy val arraySym = c.mirror.staticClass("scala.Array")
  protected lazy val jarraySym = c.mirror.staticClass("org.jscala.JArray")
  protected lazy val seqSym = c.mirror.staticClass("scala.collection.Seq")
  protected lazy val traversableSym = c.mirror.staticClass("scala.collection.Traversable")
  protected lazy val mapSym = c.mirror.staticClass("scala.collection.Map")
  protected lazy val setSym = c.mirror.staticClass("scala.collection.Set")
  protected lazy val functionTypes = List(typeOf[Function1[_,_]], typeOf[Function2[_, _,_]], typeOf[Function3[_,_,_,_]], typeOf[Function4[_,_,_,_,_]])


  protected def select(p: String, init: String => Tree = s => Ident(TermName(s))): Tree = {
    p.split("\\.").foldLeft(EmptyTree) {
      case (EmptyTree, el) => init(el)
      case (t, el) => Select(t, TermName(el))
    }
  }
  protected def isUnit(tree: Tree) = tree.equalsStructure(q"()")
  protected def isNull(tree: Tree) = tree.equalsStructure(q"null")
  protected def isArray(path: c.Tree) =
    path.tpe.typeSymbol == definitions.ArrayClass || path.tpe.typeSymbol == jarraySym || path.tpe.baseClasses.contains(seqSym)
  protected def listToExpr(exprs: List[Tree]): Tree = q"List(..$exprs)"
  protected def mapToExpr(m: Map[String, Tree]): Tree = {
    val args: List[Tree] =  m.map { case (k, v) => q"$k -> $v" }.toList
    q"Map(..$args)"
  }

  def prn(t: Tree) {
    println(s"Tpe: ${t.tpe}, S: ${t.symbol}, STS: " + (if (t.symbol ne null) t.symbol.typeSignature.toString else "null"))
  }

  def tpe(t: Tree) = t.tpe.widen
}
