package org.jscala

import scala.reflect.macros.Context

/**
 * @author Alexander Nemish
 */
class JsonConverter[C <: Context](val c: C, val debug: Boolean) extends JsBasis[C] {
  import c.universe._
  import compat._

  private def isPrim(tp: Type) = {
    tp =:= typeOf[String] || tp =:= typeOf[Boolean] || (tp weak_<:< typeOf[Double])
  }

  private def isArray(tp: Type) = tp.typeSymbol == arraySym || tp.baseClasses.contains(traversableSym)

  def toJson(ref: Tree, tpe: Type): c.Expr[JsExpr] = {
    def _toJson(tp: Type, a: Tree): Tree = {
      if (tp =:= typeOf[String]) q"""if ($a ne null) JsString($a) else JsNull"""
      else if (tp =:= typeOf[Boolean]) q"JsBoolean($a)"
      else if (tp weak_<:< typeOf[Long]) q"""JsNum($a, false)"""
      else if (tp weak_<:< typeOf[Double]) q"""JsNum($a, true)"""
      else if (tp.baseClasses.contains(mapSym)) {
        val TypeRef(_, _, List(_, arg)) = tp
        q"if ($a ne null) JsAnonObjDecl($a.map { case (k, v) => k -> ${_toJson(arg, Ident(TermName("v")))}}.toList) else JsNull"
      } else if (isArray(tp)) {
        val TypeRef(_, _, List(arg)) = tp
        q"""if ($a ne null) JsArray($a.map(n => ${_toJson(arg, Ident(TermName("n")))}).toList) else JsNull"""
      } else {
        // println(tp.typeSymbol.typeSignature.typeSymbol.annotations)
        /*
        I'd like to do something like this:
        if (tp.typeSymbol.typeSignature.typeSymbol.annotations.map(_.fullName).contains("org.jscala.Javascript")
          q"""if ($a ne null) $a.js.json else JsNull"""
        else q"""if ($a ne null) ${_toJson(tp, a)} else JsNull"""
         */
        q"""if ($a ne null) $a.js.json else JsNull"""
      }
    }

    val tree = if (isPrim(tpe)) c.abort(c.enclosingPosition, s"Root JSON element can't be of primitive type $tpe")
    else if (tpe.baseClasses.contains(mapSym) || isArray(tpe)) {
      _toJson(tpe, ref)
    } else {
      val fs = fields(tpe)
      val fds = fs collect {
        case FieldIR(n, tp, p, Some(a)) =>
          val s = _toJson(tp, q"$ref.$a")
          q"$n -> $s"
      }
      q"""JsAnonObjDecl(List(..$fds))"""
    }
    if (debug) println(s"toJson for type $tpe:\n$tree")
    c.Expr(tree)
  }

  private val mapping = Map(
    "scala.collection.immutable.Set" -> q"new scala.collection.immutable.HashSet()",
    "scala.collection.immutable.Map" -> q"new scala.collection.immutable.HashMap()",
    "scala.collection.immutable.Seq" -> q"new scala.collection.immutable.List()",
    "scala.collection.mutable.Set" -> q"new scala.collection.mutable.HashSet()",
    "scala.collection.mutable.Map" -> q"new scala.collection.mutable.HashMap()",
    "scala.collection.mutable.Seq" -> q"new scala.collection.mutable.ArrayBuffer()"
  )

  def fromJson[A: WeakTypeTag](s: c.Expr[String]): c.Expr[A] = {

    def readField(jsonObj: Tree, tp: Type): Tree = {
      if (tp =:= typeOf[String]) q"$jsonObj.asInstanceOf[String]"
      else if (tp =:= typeOf[Byte]) q"$jsonObj.asInstanceOf[Double].toByte"
      else if (tp =:= typeOf[Short]) q"$jsonObj.asInstanceOf[Double].toShort"
      else if (tp =:= typeOf[Int]) q"$jsonObj.asInstanceOf[Double].toInt"
      else if (tp =:= typeOf[Long]) q"$jsonObj.asInstanceOf[Double].toLong"
      else if (tp =:= typeOf[Float]) q"$jsonObj.asInstanceOf[Double].toFloat"
      else if (tp =:= typeOf[Double]) q"$jsonObj.asInstanceOf[Double]"
        // check Map first, because it's also Traversable
      else if (tp.baseClasses.contains(mapSym)) {
        val TypeRef(_, _, List(key, arg)) = tp
        val func = readField(Ident(TermName("v")), arg)
        val col = mapping.get(tp.typeSymbol.fullName).getOrElse(c.abort(c.enclosingPosition, s"Can't find mapping for type $tp"))
        q"""$col ++ ($jsonObj.asInstanceOf[Map[String, Any]].map{case (k, v) => k -> $func})"""
      } else if (isArray(tp)) {
        val TypeRef(_, _, List(arg)) = tp
        val func = readField(Ident(TermName("e")), arg)
        val tree = if (tp.typeSymbol == arraySym)
          q"""scala.Array[$arg]($jsonObj.asInstanceOf[List[Any]].map(e => $func).toSeq:_*)"""
        else if (tp.baseClasses.contains(traversableSym)) {
          val col = mapping.get(tp.typeSymbol.fullName).getOrElse(c.abort(c.enclosingPosition, s"Can't find mapping for type $tp"))
          q"""$col ++ ($jsonObj.asInstanceOf[List[Any]].map(e => $func))"""
        } else c.abort(c.enclosingPosition, s"Unsupported collection type $tp")
        tree
      } else {
        val fs = fields(tp)
        val ctorFirs = fs.filter(_.param.isDefined)
        val ctorSig = ctorFirs.map(fir => (fir.param.get: Symbol, fir.tpe)).toMap
        val ctorArgs = {
          if (ctorSig.isEmpty) List(List())
          else {
            val ctorSym = ctorSig.head._1.owner.asMethod
            ctorSym.paramss.map(_.map(f => {
              readField(q"$jsonObj.asInstanceOf[Map[String, Any]](${f.name.decoded})", ctorSig(f))
            }))
          }
        }
        val instantiation = q"new $tp(...$ctorArgs)"
        if (debug) println(s"$jsonObj = $instantiation")
        instantiation
      }
    }

    val tpe = weakTypeOf[A]
    val instantiation = readField(q"jsonType", tpe)
    val tree = q"""
    scala.util.parsing.json.JSON.parseFull($s) match {
      case None => sys.error("Can't parse JSON: " + $s)
      case Some(jsonType) => $instantiation
    }
    """
    if (debug) println(s"fromJson for type $tpe: \n$tree")
    c.Expr[A](tree)
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
