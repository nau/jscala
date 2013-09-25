package org.jscala

import scala.annotation.StaticAnnotation

import language.experimental.macros
import language.implicitConversions
import scala.reflect.macros.Context

object MacroAnnotations {
  def annotationImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    val expandees = inputs match {
      case (cd@ClassDef(mods, name, _, tpl@Template(parents, _, body))) :: comp =>
        val jsDef = DefDef(Modifiers(), newTermName("javaScript"), List(), List(), Ident(newTypeName("JsAst")),
            Apply(Select(Select(Ident(newTermName("org")), newTermName("jscala")), newTermName("javascript")), List(Block(
                List(ClassDef(mods, name, List(), Template(parents, emptyValDef, body))), Literal(Constant(()))))))
//        println("Generated: " + showRaw(jsDef))
        val companion = comp match {
          case Nil => q"""object ${name.toTermName} { $jsDef }"""
          case List(co@ModuleDef(mods, name, Template(p, t, impl))) => ModuleDef(mods, name, Template(p, t, impl :+ jsDef))
        }
        
        List(cd, companion)
      case _ => c.abort(c.enclosingPosition, "JavaScript annotation is only supported on class/trait definitions")
    }
//    println(s"Annottee: $annottees")
//    println(s"Expandees: $expandees")
    c.Expr[Any](Block(expandees, Literal(Constant(()))))
  }
}

class JavaScript extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro MacroAnnotations.annotationImpl
}