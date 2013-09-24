package org.jscala

import scala.annotation.StaticAnnotation

import language.experimental.macros
import language.implicitConversions
import scala.reflect.macros.Context

object MacroAnnotations {
  def annotationImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    println(showRaw(annottees))
    val inputs = annottees.map(_.tree).toList
    val (annottee, expandees) = inputs match {
      case List(param@ClassDef(_, _, _, tpl@Template(_, _, body))) =>
//        val pe = c.Expr[ClassDef](param)

        val collect = param.collect {
          case vd: DefDef if vd.name != nme.CONSTRUCTOR => vd
        }
        val pe1 = List(Block(collect, Literal(Constant(()))))
//        val pe = q"class MyClass"
        val pe = ClassDef(Modifiers(), param.name, Nil, Template(param.impl.parents, emptyValDef, pe1))
        pe.setSymbol(param.symbol)
        val js = Apply(Select(Ident(newTermName("package")), newTermName("javascript")), List(pe))
//        val js = Macros.javascriptImpl(c)(c.Expr[ClassDef](param)).tree
        val jsDef = DefDef(NoMods, newTermName("javaScript"), Nil, Nil, tq"JsAst", js)
        val impl = Template(tpl.parents, tpl.self, body :+ jsDef)
        val cd = ClassDef(param.mods, param.name, param.tparams, impl)
        (param, cd :: Nil)
      case _ => c.abort(c.enclosingPosition, "JavaScript annotation is only supported on class/trait definitions")
    }
    println((annottee, expandees))
    val outputs = expandees
    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}

class JavaScript extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro MacroAnnotations.annotationImpl
}