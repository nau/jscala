package org.jscala

import scala.annotation.StaticAnnotation

import language.experimental.macros
import language.implicitConversions
import scala.reflect.macros.Context

object MacroAnnotations {

  class JavascriptAnnotation [C <: Context](val c: C, json: Boolean, debug: Boolean) extends JsBasis[C] {
    import c.universe._
    def transform(annottees: c.Expr[Any]*): c.Expr[Any] = {
      val inputs = annottees.map(_.tree).toList
//      println(inputs.raw)
//      println(inputs.map(_.raw).mkString("\n"))
      val expandees = inputs match {
//        case any => println(any); inputs
//        case (cd@ClassDef(mods, name, _, tpl@Template(parents, _, body))) :: Nil => inputs

        case (cd@ClassDef(mods, name, tparams, tpl@Template(parents, sf, body))) :: comp =>
          val jsDef = DefDef(Modifiers(), newTermName("javascript"), List(), List(), Ident(newTypeName("JsAst")),
            Apply(Select(Select(Ident(newTermName("org")), newTermName("jscala")), newTermName("javascript")), List(Block(
              List(ClassDef(mods, name, List(), Template(parents, emptyValDef, body))), Literal(Constant(()))))))
          /*println("Members of " + name)
          cd.symbol.typeSignature.declarations foreach println*/
          /*val jsonDef = DefDef(Modifiers(), newTermName("json"), List(), List(), Ident(newTypeName("JsAst")),
            Apply(Select(Select(Ident(newTermName("org")), newTermName("jscala")), newTermName("toJson")), List(Block(
              List(ClassDef(mods, newTypeName(name.decoded + "$JScala"), List(), Template(parents, emptyValDef, body))), Literal(Constant(()))))))*/
//          val jscalaObj = q"object js {$jsonDef}"
          val expanded = if (json) {
            val jscalaObj = q"""object js {
              def json: JsExpr = org.jscala.toJson1[$name]
            }"""
            body :+ jscalaObj
          } else body
          val cd1 = ClassDef(mods, name, tparams, Template(parents, sf, expanded))
          val companion = comp match {
            case Nil => q"""object ${name.toTermName} { object jscala {
                $jsDef
                def fromJson(json: String) = org.jscala.fromJson[$name](json)
              }
            }"""
            case List(co@ModuleDef(mods, name, Template(p, t, impl))) => ModuleDef(mods, name, Template(p, t, impl :+ jsDef))
          }

          List(cd1, companion)
        case _ => c.abort(c.enclosingPosition, "Javascript annotation is only supported on class/trait definitions")
      }
      //    println(s"Annottee: $annottees")
      if (debug) println(s"Expandees: $expandees")
      c.Expr[Any](Block(expandees, Literal(Constant(()))))
    }
  }

  def annotationImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    var dbg = false
    var json = true
    c.macroApplication match {
      case Apply(Select(Apply(Select(New(Ident(js)), nme.CONSTRUCTOR), args), _), _) if js.decoded == "Javascript" =>
        println(showRaw("ARGS " + args))
        args match {
          case List(Literal(Constant(js: Boolean))) => json = js
          case List(Literal(Constant(js: Boolean)), Literal(Constant(db: Boolean))) => json = js; dbg = db
          case _ =>
            args.foreach {
            case AssignOrNamedArg(Ident(debug), Literal(Constant(v: Boolean))) if debug.decoded == "debug" => dbg = v
            case AssignOrNamedArg(Ident(js), Literal(Constant(v: Boolean))) if js.decoded == "json" => json = v
            case _ =>
          }
        }
      case _ => println("MACRO " + showRaw(c.macroApplication))
    }

    new JavascriptAnnotation[c.type](c, json, dbg).transform(annottees:_*)
  }
}

class Javascript(val json: Boolean = true, val debug: Boolean = false) extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro MacroAnnotations.annotationImpl
}

class Transient extends StaticAnnotation