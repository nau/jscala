package org

import javax.script.ScriptEngineManager
import com.yahoo.platform.yui.compressor.JavaScriptCompressor
import java.io.{StringWriter, StringReader}
import org.mozilla.javascript.ErrorReporter
import scala.reflect.internal.Flags
import scala.collection.generic.{MapFactory, SeqFactory}

package object jscala {
  import language.experimental.macros
  import language.implicitConversions
  import scala.reflect.macros.Context

  private lazy val engine = {
    val factory = new ScriptEngineManager()
    factory.getEngineByName("JavaScript")
  }

  implicit class JsAstOps(ast: JsAst) {

    def asString = JavascriptPrinter.print(ast, 0)
    def eval() = engine.eval(asString)
    def compress = {
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
    def as[A] = a.asInstanceOf[A]
  }

  implicit def implicitString2JString(s: String): JString = new JString(s)
  implicit def implicitJString2String(s: JString): String = ""
  implicit def implicitArray2JArray[A](s: Array[A]): JArray[A] = ???
  implicit def implicitJArray2Array[A](s: JArray[A]): Array[A] = ???
  implicit def implicitSeq2JArray[A](s: Seq[A]): JArray[A] = ???
  implicit def implicitJArray2Seq[A](s: JArray[A]): Seq[A] = ???

  trait JsSerializer[A] {
    def apply(a: A): JsExpr
  }
  implicit object boolJsSerializer extends JsSerializer[Boolean] { def apply(a: Boolean) = JsBool(a) }
  implicit object byteJsSerializer extends JsSerializer[Byte] { def apply(a: Byte) = JsNum(a, false) }
  implicit object shortJsSerializer extends JsSerializer[Short] { def apply(a: Short) = JsNum(a, false) }
  implicit object intJsSerializer extends JsSerializer[Int] { def apply(a: Int) = JsNum(a, false) }
  implicit object longJsSerializer extends JsSerializer[Long] { def apply(a: Long) = JsNum(a, false) }
  implicit object floatJsSerializer extends JsSerializer[Float] { def apply(a: Float) = JsNum(a, true) }
  implicit object doubleJsSerializer extends JsSerializer[Double] { def apply(a: Double) = JsNum(a, true) }
  implicit object stringJsSerializer extends JsSerializer[String] { def apply(a: String) = JsString(a) }
  implicit object arrJsSerializer extends JsSerializer[collection.Seq[JsExpr]] { def apply(a: collection.Seq[JsExpr]) = JsArray(a.toList) }
  implicit object mapJsSerializer extends JsSerializer[collection.Map[String,JsExpr]] { def apply(a: collection.Map[String,JsExpr]) = JsAnonObjDecl(a.toMap) }
  implicit def funcJsSerializer[A](implicit ev: JsSerializer[A]): JsSerializer[() => A] = new JsSerializer[() => A] { def apply(a: () => A) = ev.apply(a()) }
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
  def escape(uri: String): JString = null
  def unescape(uri: String): JString = null
  def eval(uri: String): AnyRef = null
  def isFinite(uri: AnyRef) = false
  def isNaN(uri: AnyRef) = false
  def parseFloat(str: String) = 1.0
  def parseInt(str: String) = 1
  def typeof(x: Any) = ""
  def include(js: String) = ""

  def inject(a: JsAst) = ???
  /**
   * Injects a value into generated Javascript using Jsserializer
   */
  def inject[A](a: A)(implicit jss: JsSerializer[A]) = a

  /**
   * Macro that generates Javascript AST representation of its argument
   */
  def ajax[A, B](input: A)(server: A => B)(callback: B => Unit): JsAst = ???

  /**
   * Macro that generates Javascript AST representation of its argument
   */
  def javascript(expr: Any): JsAst = macro Macros.javascriptImpl

  object Macros {
    def javascriptImpl(c: Context)(expr: c.Expr[Any]): c.Expr[JsAst] = {
      val parser = new ScalaToJsConverter[c.type](c)
      parser.convert(expr.tree)
    }
  }
}
