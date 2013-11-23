package org.jscala

import scala.reflect.macros.Context

/**
 * @author Alexander Nemish
 */
class JsonConverter[C <: Context](val c: C, debug: Boolean) extends JsBasis[C] {
  import c.universe._
  def toJson(tree: Tree) = tree match {
    case Block(cd@ClassDef(mods, name, _, tpl@Template(parents, _, body)) :: Nil, _) =>
      println("AAAAAAAAAAAAAAAAAA\n")// + body.map(e => showRaw(e)).mkString("\n"))
      val fs = fields(tpl.tpe)
      val fds = fs collect {
        case FieldIR(n, tp, p, Some(a)) if tp =:= typeOf[String] => q"""$n -> JsString(this.$a)"""

      }
//      val fds = List("asdf" -> "asdf").map { case (n, i) => q"""$n -> JsString($i)""" }
      c.Expr(q"""JsAnonObjDecl(List(..$fds))""")
//      reify(JsString("a"))
    case _ => c.abort(c.enclosingPosition, s"toJson can't generate JSON for this tree: ${tree.raw}")
  }

  private def isPrim(tp: Type) = {
    tp =:= typeOf[String] || tp =:= typeOf[Boolean] || (tp weak_<:< typeOf[Double])
  }

  def toJson1(tpe: Type): c.Expr[JsExpr] = {
    tpe.typeSymbol.typeSignature
    println("BBBBBBBBBBBBBBB " + tpe)
    println(showRaw(tpe))



    def primitive(tp: Type) = {
      if (tp =:= typeOf[String]) q"""(a: $tp) => if (a ne null) JsString(a) else JsNull"""
      else if (tp =:= typeOf[Boolean]) q"(a: $tp) => JsBoolean(a)"
      else if (tp weak_<:< typeOf[Long]) q"""(a: $tp) => JsNum(a, false)"""
      else if (tp weak_<:< typeOf[Double]) q"""(a: $tp) => JsNum(a, true)"""
      else c.abort(c.enclosingPosition, s"$tp is not a primitive!")
    }

    def _toJson(tp: Type, a: Name): Tree = {
      println("Type " + showRaw(tp) + ", annots: " + tp.typeSymbol.annotations)
      if (isPrim(tp)) q"${primitive(tp)}($a)"
      else if (tp.baseClasses.contains(mapSym)) {
        val TypeRef(_, _, List(_, arg)) = tp
        q"if ($a ne null) JsAnonObjDecl($a.map { case (k, v) => k -> ${_toJson(arg, newTermName("v"))}}.toList) else JsNull"
      } else if (tp.baseClasses.contains(traversableSym)) {
        val TypeRef(_, _, List(arg)) = tp
        q"""if ($a ne null) JsArray($a.map(n => ${_toJson(arg, newTermName("n"))}).toList) else JsNull"""
      } else if (tp.typeSymbol == arraySym) {
        val TypeRef(_, _, List(arg)) = tp
        q"""if ($a ne null) JsArray($a.map(n => ${_toJson(arg, newTermName("n"))}).toList) else JsNull"""
      } else
        q"""if ($a ne null) $a.js.json else JsNull"""
    }

    val fs = fields(tpe)
    val fds = fs collect {
      case FieldIR(n, tp, p, Some(a)) =>
        val s = _toJson(tp, a.name)
        q"$n -> $s"
    }
    val r = c.Expr(q"""JsAnonObjDecl(List(..$fds))""")
    println(r)
    r
    //      reify(JsString("a"))
  }

  def fromJson[A: WeakTypeTag](s: c.Expr[String]): c.Expr[A] = {

    def readField(nm: String, tp: Type) = {
      val n = c.literal(nm)
      q"""jsonType.asInstanceOf[JSONObject].obj($n).asInstanceOf[$tp]"""
    }

    val tpe = weakTypeOf[A]
    val fs = fields(tpe)
    val ctorFirs = fs.filter(_.param.isDefined)
    val ctorSig = ctorFirs.map(fir => (fir.param.get: Symbol, fir.tpe)).toMap
    val ctorArgs = {
      if (ctorSig.isEmpty) List(List())
      else {
        val ctorSym = ctorSig.head._1.owner.asMethod
        ctorSym.paramss.map(_.map(f => {
          readField(f.name.decoded, ctorSig(f))
        }))
      }
    }
    val instantiation = q"new $tpe(...$ctorArgs)"
    val r = c.Expr[A](q"""
    import scala.util.parsing.json._
    JSON.parseRaw($s) match {
      case None => sys.error("Can't parse JSON: " + $s)
      case Some(jsonType) => $instantiation
    }
    """)
    println(r)
    r
  }

  case class FieldIR(name: String, tpe: Type, param: Option[TermSymbol], accessor: Option[MethodSymbol]) {
    def field = accessor.map(_.accessed.asTerm)
    def getter = accessor.map(_.getter).flatMap(sym => if (sym != NoSymbol) Some(sym) else None)
    def setter = accessor.map(_.setter).flatMap(sym => if (sym != NoSymbol) Some(sym) else None)
    def isParam = param.map(_.owner.name == nme.CONSTRUCTOR).getOrElse(false)
    def isPublic = accessor.map(_.isPublic).getOrElse(false)

    // this part is interesting to picklers
    def hasGetter = getter.isDefined

    // this part is interesting to unpicklers
    def hasSetter = setter.isDefined
    def isErasedParam = isParam && accessor.isEmpty // TODO: this should somehow communicate with the constructors phase!
    def isReifiedParam = isParam && accessor.nonEmpty
    def isNonParam = !isParam
  }

  private type Q = List[FieldIR]

  private def fields(tp: Type): Q = {
    val ctor = tp.declaration(nme.CONSTRUCTOR) match {
      case overloaded: TermSymbol => overloaded.alternatives.head.asMethod // NOTE: primary ctor is always the first in the list
      case primaryCtor: MethodSymbol => primaryCtor
      case NoSymbol => NoSymbol
    }
    val ctorParams = if (ctor != NoSymbol) ctor.asMethod.paramss.flatten.map(_.asTerm) else Nil
    val allAccessors = tp.declarations.collect{ case meth: MethodSymbol if meth.isAccessor || meth.isParamAccessor => meth }
    val (paramAccessors, otherAccessors) = allAccessors.partition(_.isParamAccessor)

    def mkFieldIR(sym: TermSymbol, param: Option[TermSymbol], accessor: Option[MethodSymbol]) = {
      val (quantified, rawTp) = tp match { case ExistentialType(quantified, tpe) => (quantified, tpe); case tpe => (Nil, tpe) }
      val rawSymTp = accessor.getOrElse(sym).typeSignatureIn(rawTp) match { case NullaryMethodType(tpe) => tpe; case tpe => tpe }
      val symTp = existentialAbstraction(quantified, rawSymTp)
      FieldIR(sym.name.toString.trim, symTp, param, accessor)
    }

    val paramFields = ctorParams.map(sym => mkFieldIR(sym, Some(sym), paramAccessors.find(_.name == sym.name)))
    val varGetters = otherAccessors.collect{ case meth if meth.isGetter && meth.accessed != NoSymbol && meth.accessed.asTerm.isVar => meth }
    val varFields = varGetters.map(sym => mkFieldIR(sym, None, Some(sym)))

    paramFields ++ varFields
  }
}
