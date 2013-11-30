package org.jscala

import scala.reflect.macros.Context
import scala.collection.generic.{MapFactory, SeqFactory}

/**
 * Author: Alexander Nemish
 * Date: 10/25/13
 * Time: 10:35 PM
 */
trait MacroHelpers[C <: Context] {
  val c: C
  import c.universe._
  type PFT[A] = PartialFunction[Tree, A]
  type ToExpr[A] = PFT[Expr[A]]

  implicit class TreeHelper(tree: Tree) {
    def is(p: String): Boolean = tree.equalsStructure(select(p)) || tree.equalsStructure(select(p, s => This(TypeName(s))))
    lazy val isArrow: Boolean = is("scala.Predef.any2ArrowAssoc") /*2.10.x*/ || is("scala.Predef.ArrowAssoc") /*2.11.x*/
    def raw: String = showRaw(tree)
    def isNum = tree.tpe.widen weak_<:< typeOf[Long]
  }

  implicit class NameHelper(name: Name) {
    def isArrow: Boolean = name.decoded == "->" || name.decoded == "→"
  }

  object Name {
    def unapply(name: Name): Option[String] = Some(name.decoded)
  }

  object TermName {
    def apply(s: String) = newTermName(s)
    def unapply(name: TermName): Option[String] = Some(name.toString)
  }

  object TypeName {
    def apply(s: String) = newTypeName(s)
    def unapply(name: TypeName): Option[String] = Some(name.toString)
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
  protected def listToExpr[T](exprs: List[Expr[T]]): Expr[List[T]] = c.Expr[List[T]](q"List(..$exprs)")
  protected def mapToExpr[V](m: Map[String, Expr[V]]): Expr[Map[String, V]] = {
    val args: List[Tree] =  m.map { case (k, v) => q"$k -> $v" }.toList
    c.Expr[Map[String, V]](q"Map(..$args)")
  }

  def prn(t: Tree) {
    println(s"Tpe: ${t.tpe}, S: ${t.symbol}, STS: " + (if (t.symbol ne null) t.symbol.typeSignature.toString else "null"))
  }

  def tpe(t: Tree) = t.tpe.widen
}
