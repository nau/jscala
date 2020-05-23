package org.jscala

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.whitebox

object MacroAnnotations {

  class JavascriptAnnotation [C <: whitebox.Context](val c: C, debug: Boolean) {
    import c.universe._
    def transform(annottees: c.Expr[Any]*): c.Expr[Any] = {
      val inputs = annottees.map(_.tree).toList
      val expandees = inputs match {
        case (cd@ClassDef(mods, name, tparams, tpl@Template(parents, sf, body))) :: comp =>
          val javascriptMacro = if (debug) TermName("javascriptDebug") else TermName("javascript")
          val jsDef = if (mods hasFlag Flag.CASE) EmptyTree
          else DefDef(Modifiers(), TermName("javascript"), List(), List(), Ident(TypeName("JsAst")),
            Apply(Select(Select(Ident(TermName("org")), TermName("jscala")), javascriptMacro), List(Block(
              List(ClassDef(mods, name, List(), Template(parents, noSelfType, body))), Literal(Constant(()))))))
          val cd1 = ClassDef(mods, name, tparams, Template(parents, sf, body))
          val companion = comp match {
            case Nil => q"""object ${name.toTermName} { object jscala {
                $jsDef
              }
            }"""
            case List(co@ModuleDef(mods, name, Template(p, t, impl))) => ModuleDef(mods, name, Template(p, t, impl :+ jsDef))
          }

          List(cd1, companion)
        case _ => c.abort(c.enclosingPosition, "Javascript annotation is only supported on class/trait definitions")
      }
      if (debug) println(s"Expandees: $expandees")
      c.Expr[Any](Block(expandees, Literal(Constant(()))))
    }
  }

  def annotationImpl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {

    var debug = false
/*
    c.macroApplication match {
      case Apply(Select(Apply(Select(New(Ident(js)), termNames.CONSTRUCTOR), args), _), _) if js.decodedName.toString == "Javascript" =>
        args match {
          case List(Literal(Constant(dbg: Boolean))) => debug = dbg
          case _ =>
            args.foreach {
            case AssignOrNamedArg(Ident(dbg), Literal(Constant(v: Boolean))) if dbg.decodedName.toString == "debug" => debug = v
            case _ =>
          }
        }
      case _ => c.warning(c.enclosingPosition, "Can't parse @Javascript annotation arguments")
    }
*/

    new JavascriptAnnotation[c.type](c, debug).transform(annottees:_*)
  }
}

class Javascript(val debug: Boolean = false) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MacroAnnotations.annotationImpl
}

class Transient extends StaticAnnotation