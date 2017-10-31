package org

import javax.script.{ScriptEngine, ScriptEngineManager}

import com.yahoo.platform.yui.compressor.JavaScriptCompressor
import java.io.{StringReader, StringWriter}

import org.mozilla.javascript.ErrorReporter

import scala.reflect.macros.blackbox

package object jscala {
  import language.experimental.macros
  import language.implicitConversions
  import scala.reflect.macros.Context

  private lazy val engine: ScriptEngine = {
    val factory = new ScriptEngineManager(null)
    factory.getEngineByName("JavaScript")
  }

  implicit class JsAstOps(ast: JsAst) {

    def asString: String = JavascriptPrinter.print(ast, 0)
    def eval(): AnyRef = engine.eval(asString)
    def compress: String = {
      val compressor = new JavaScriptCompressor(new StringReader(asString), new ErrorReporter {
        def warning(p1: String, p2: String, p3: Int, p4: String, p5: Int) {
          println(s"Warn $p1 $p2, ${p3.toString} $p4 ${p5.toString}")
        }

        def error(p1: String, p2: String, p3: Int, p4: String, p5: Int) {
          println(s"Error $p1 $p2, ${p3.toString} $p4 ${p5.toString}")
        }

        def runtimeError(p1: String, p2: String, p3: Int, p4: String, p5: Int) = {
          println(s"Runtime $p1 $p2, ${p3.toString} $p4 ${p5.toString}")
          ???
        }
      })
      val buf = new StringWriter
      compressor.compress(buf, 1, true, false, false, false)
      buf.toString
    }
  }

  implicit class JsAnyOps(a: Any) {
    def as[A]: A = a.asInstanceOf[A]
    def instanceof[A]: Boolean = sys.error("Can't be used on JVM side")
    def instanceof(name: String): Boolean = sys.error("Can't be used on JVM side")
  }

  implicit def implicitString2JString(s: String): JString = new JString(s)
  implicit class implicitRichString(s: String) {
    def jstr: JString = new JString(s)
  }
  implicit def implicitJString2String(s: JString): String = ""
  implicit def implicitArray2JArray[A](s: Array[A]): JArray[A] = ???
  implicit def implicitJArray2Array[A](s: JArray[A]): Array[A] = ???
  implicit def implicitSeq2JArray[A](s: Seq[A]): JArray[A] = ???
  implicit def implicitJArray2Seq[A](s: JArray[A]): Seq[A] = ???

  trait JsSerializer[-A] {
    def apply(a: A): JsExpr
  }
  implicit object jsExprJsSerializer extends JsSerializer[JsExpr] { def apply(a: JsExpr): JsExpr = a }
  implicit object boolJsSerializer extends JsSerializer[Boolean] { def apply(a: Boolean): JsExpr = JsBool(a) }
  implicit object byteJsSerializer extends JsSerializer[Byte] { def apply(a: Byte): JsExpr = JsNum(a, false) }
  implicit object shortJsSerializer extends JsSerializer[Short] { def apply(a: Short): JsExpr = JsNum(a, false) }
  implicit object intJsSerializer extends JsSerializer[Int] { def apply(a: Int): JsExpr = JsNum(a, false) }
  implicit object longJsSerializer extends JsSerializer[Long] { def apply(a: Long): JsExpr = JsNum(a, false) }
  implicit object floatJsSerializer extends JsSerializer[Float] { def apply(a: Float): JsExpr = JsNum(a, true) }
  implicit object doubleJsSerializer extends JsSerializer[Double] { def apply(a: Double): JsExpr = JsNum(a, true) }
  implicit object stringJsSerializer extends JsSerializer[String] { def apply(a: String): JsExpr = JsString(a) }
  implicit def arrJsSerializer[A](implicit ev: JsSerializer[A]): JsSerializer[Array[A]] =
    new JsSerializer[Array[A]] {
      def apply(a: Array[A]): JsExpr = JsArray(a.map(ev.apply).toList)
    }

  implicit def seqJsSerializer[A](implicit ev: JsSerializer[A]): JsSerializer[collection.Seq[A]] =
    new JsSerializer[collection.Seq[A]] {
      def apply(a: collection.Seq[A]): JsExpr = JsArray(a.map(ev.apply).toList)
    }

  implicit def mapJsSerializer[A](implicit ev: JsSerializer[A]): JsSerializer[collection.Map[String, A]] =
    new JsSerializer[collection.Map[String, A]] {
      def apply(a: collection.Map[String, A]): JsExpr = JsAnonObjDecl(a.map{ case (k, v) => (k, ev.apply(v)) }.toList)
    }

  implicit def funcJsSerializer[A](implicit ev: JsSerializer[A]): JsSerializer[() => A] =
    new JsSerializer[() => A] {
      def apply(a: () => A): JsExpr = ev.apply(a())
    }

  implicit class ToJsExpr[A](a: A)(implicit ev: JsSerializer[A]) {
    def toJs: JsExpr = ev.apply(a)
  }

  // Javascript top-level functions/constants
  val Infinity = Double.PositiveInfinity
  val NaN = Double.NaN
  val undefined: AnyRef = null

  // Javascript Global Functions
  def decodeURI(uri: String): JString = null
  def decodeURIComponent(uri: String): JString = null
  def encodeURI(uri: String): JString = null
  def encodeURIComponent(uri: String): JString = null
  def escape(str: String): JString = null
  def unescape(str: String): JString = null
  def eval(str: String): AnyRef = null
  def isFinite(x: Any): Boolean = false
  def isNaN(x: Any): Boolean = false
  def parseFloat(str: String): Double = str.toDouble
  def parseInt(str: String, base: Int = 10): Int = java.lang.Integer.parseInt(str, base)
  def typeof(x: Any): String = ""
  def include(js: String): String = ""
  def print(x: Any): Unit = {
    System.out.println(x)
  }
  def delete(x: Any): Boolean = ???

  /**
   * Scala/JavaScript implementation of for..in
   *
   * {{{
   *   val coll = Seq("a", "b")
   *   forIn(coll)(ch => print(ch))
   * }}}
   * translates to
   * var coll = ["a", "b"];
   * for (var ch in coll) print(ch);
   */
  def forIn[A](coll: Seq[A])(f: Int => Unit): Unit = {
    var idx = 0
    val len = coll.length
    while (idx < len) {
      f(idx)
      idx += 1
    }
  }

  /**
   * Scala/JavaScript implementation of for..in
   *
   * {{{
   *   val coll = Map("a" -> 1, "b" -> 2)
   *   forIn(coll)(ch => print(ch))
   * }}}
   * translates to
   * var coll = {"a": 1, "b": 2};
   * for (var ch in coll) print(ch);
   */
  def forIn[A, B](map: Map[A, B])(f: A => Unit): Unit = {
    map.keysIterator.foreach(k => f(k))
  }

  /**
   * Scala/JavaScript implementation of for..in
   *
   * {{{
   *   val obj = new { val a = 1 }
   *   forIn(obj)(ch => print(ch))
   * }}}
   * translates to
   * var obj = {"a": 1};
   * for (var ch in obj) print(ch);
   * @note Doesn't work in Scala!
   */
  def forIn[A, B](obj: AnyRef)(f: String => Unit): Unit = ???

  def include(a: JsAst): Nothing = ???
  /**
   * Injects a value into generated JavaScript using JsSerializer
   */
  def inject[A](a: A)(implicit jss: JsSerializer[A]): A = a

  /**
   * Macro that generates JavaScript AST representation of its argument
   */
  def javascript(expr: Any): JsAst = macro Macros.javascriptImpl
  /**
   * Macro that generates JavaScript String representation of its argument
   */
  def javascriptString(expr: Any): String = macro Macros.javascriptStringImpl
  def javascriptDebug(expr: Any): JsAst = macro Macros.javascriptDebugImpl

  object Macros {
    def javascriptImpl(c: blackbox.Context)(expr: c.Expr[Any]): c.Expr[JsAst] = {
      val parser = new ScalaToJsConverter[c.type](c, debug = false)
      c.Expr(parser.convert(expr.tree))
    }
    def javascriptStringImpl(c: blackbox.Context)(expr: c.Expr[Any]): c.Expr[String] = {
      import c.universe._
      val parser = new ScalaToJsConverter[c.type](c, debug = false)
      val jsAst = parser.convert(expr.tree)
      val str = c.eval(c.Expr[String](q"new JsAstOps($jsAst).asString"))
      c.Expr[String](q"$str")
    }

    def javascriptDebugImpl(c: blackbox.Context)(expr: c.Expr[Any]): c.Expr[JsAst] = {
      val parser = new ScalaToJsConverter[c.type](c, debug = true)
      c.Expr(parser.convert(expr.tree))
    }
  }
}


