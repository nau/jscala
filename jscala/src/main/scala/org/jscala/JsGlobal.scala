package org.jscala

import language.dynamics
import scala.util.matching.Regex
import java.lang

class JString(s: String) {
  val length = 0
  def charAt(i: Int) = ""
  def charCodeAt(i: Int) = 0
  def indexOf(item: String): Int = 0
  def lastIndexOf(item: String): Int = 0
  def `match`(regexp: Regex): JArray[JString] = JArray()
  def replace(searchvalue: Regex, newvalue: String) = this
  def replace(searchvalue: String, newvalue: String) = this
  def search(searchvalue: String) = 0
  def search(searchvalue: Regex) = 0
  def slice(start: Int, end: Int = 0) = 0
  def split(separator: Int, end: Int = 0) = 0
}

case class JArray[A](args: A*) {
  val length = 0
  def apply(idx: Int): A = null.asInstanceOf[A]
  def update(idx: Int, value: A) {}
  def concat(arrays: JArray[A]*): JArray[A] = this
  def indexOf(item: A): Int = 0
  def lastIndexOf(item: A): Int = 0
  def join(): JString = null
  def push(items: A*) = this
  def pop(): A = null.asInstanceOf[A]
  def reverse() = this
  def shift(): A = null.asInstanceOf[A]
  def unshift(items: A): Int = 0
  def slice(start: Int, end: Int) = this
  def sort(f: (A, A) => Int = (_, _) => 0) = this
  def splice(index: Int, howmany: Int, items: A*) = this
  def valueOf() = this
}

object Math {
  val E = lang.Math.E
  val LN2 = lang.Math.E
  val LN10 = lang.Math.E
  val LOG2E = lang.Math.E
  val LOG10E = lang.Math.E
  val PI = lang.Math.PI
  val SQRT1_2 = lang.Math.PI
  val SQRT2 = lang.Math.PI
  def abs(x: Double) = lang.Math.abs(x)
  def acos(x: Double) = lang.Math.acos(x)
  def asin(x: Double) = lang.Math.asin(x)
  def atan(x: Double) = lang.Math.atan(x)
  def atan2(y: Double, x: Double) = lang.Math.atan2(y, x)
  def ceil(x: Double) = lang.Math.ceil(x)
  def cos(x: Double) = lang.Math.cos(x)
  def exp(x: Double) = lang.Math.exp(x)
  def floor(x: Double) = lang.Math.floor(x)
  def log(x: Double) = lang.Math.log(x)
  def max(x1: Double, x2: Double, xs: Double*) = lang.Math.max(x1, x2)
  def min(x1: Double, x2: Double, xs: Double*) = lang.Math.min(x1, x2)
  def pow(x: Double, y: Double) = lang.Math.pow(x, y)
  def random() = lang.Math.random()
  def round(x: Double) = lang.Math.round(x)
  def sin(x: Double) = lang.Math.sin(x)
  def sqrt(x: Double) = lang.Math.sqrt(x)
  def tan(x: Double) = lang.Math.tan(x)
}

class RegExp(pattern: String, modifiers: String = "") {
  val global = false
  val ignoreCase = false
  val lastIndex = 1
  val multiline = false
  val source = pattern
  def compile(pattern: String, modifiers: String = "") {}
  def exec(str: String): JString = null
  def test(str: String) = false
}

class Date {
  def getDate() = 0

  //	Returns the day of the month (from 1-31)
  def getDay() = 0

  //	Returns the day of the week (from 0-6)
  def getFullYear() = 0

  //	Returns the year (four digits)
  def getHours() = 0

  //	Returns the hour (from 0-23)
  def getMilliseconds() = 0

  //	Returns the milliseconds (from 0-999)
  def getMinutes() = 0

  //	Returns the minutes (from 0-59)
  def getMonth() = 0

  //	Returns the month (from 0-11)
  def getSeconds() = 0

  //	Returns the seconds (from 0-59)
  def getTime() = 0

  //	Returns the number of milliseconds since midnight Jan 1, 1970
  def getTimezoneOffset() = 0

  //	Returns the time difference between UTC time and local time, in minutes
  def getUTCDate() = 0

  //	Returns the day of the month, according to universal time (from 1-31)
  def getUTCDay() = 0

  //	Returns the day of the week, according to universal time (from 0-6)
  def getUTCFullYear() = 0

  //	Returns the year, according to universal time (four digits)
  def getUTCHours() = 0

  //	Returns the hour, according to universal time (from 0-23)
  def getUTCMilliseconds() = 0

  //	Returns the milliseconds, according to universal time (from 0-999)
  def getUTCMinutes() = 0

  //	Returns the minutes, according to universal time (from 0-59)
  def getUTCMonth() = 0

  //	Returns the month, according to universal time (from 0-11)
  def getUTCSeconds() = 0

  //	Returns the seconds, according to universal time (from 0-59)
  def getYear() = 0

  //	Deprecated. Use the def getFullYear() = 0 // method instead
  def parse(date: String) = 0

  //	Parses a date string and returns the number of milliseconds since midnight of January 1, 1970
  def setDate(v: Int) {}

  //	Sets the day of the month of a date object
  def setFullYear(v: Int) {}

  //	Sets the year (four digits) of a date object
  def setHours(v: Int) {}

  //	Sets the hour of a date object
  def setMilliseconds(v: Int) {}

  //	Sets the milliseconds of a date object
  def setMinutes(v: Int) {}

  //	Set the minutes of a date object
  def setMonth(v: Int) {}

  //	Sets the month of a date object
  def setSeconds(v: Int) {}

  //	Sets the seconds of a date object
  def setTime(v: Int) {}

  //	Sets a date and time by adding or subtracting a specified number of milliseconds to/from midnight January 1, 1970
  def setUTCDate(v: Int) {}

  //	Sets the day of the month of a date object, according to universal time
  def setUTCFullYear(v: Int) {}

  //	Sets the year of a date object, according to universal time (four digits)
  def setUTCHours(v: Int) {}

  //	Sets the hour of a date object, according to universal time
  def setUTCMilliseconds(v: Int) {}

  //	Sets the milliseconds of a date object, according to universal time
  def setUTCMinutes(v: Int) {}

  //	Set the minutes of a date object, according to universal time
  def setUTCMonth(v: Int) {}

  //	Sets the month of a date object, according to universal time
  def setUTCSeconds(v: Int) {}

  //	Set the seconds of a date object, according to universal time
  def setYear(v: Int) {}

  //	Deprecated. Use the def setFullYear() = 0 // method instead
  def toDateString() = 0

  //	Converts the date portion of a Date object into a readable string
  def toGMTString() = ""

  //	Deprecated. Use the toUTCString // method instead
  def toISOString() = ""

  //	Returns the date as a string, using the ISO standard
  def toJSON() = ""

  //	Returns the date as a string, formatted as a JSON date
  def toLocaleDateString() = ""

  //	Returns the date portion of a Date object as a string, using locale conventions
  def toLocaleTimeString() = ""

  //	Returns the time portion of a Date object as a string, using locale conventions
  def toLocaleString() = ""

  //	Converts a Date object to a string
  def toTimeString() = ""

  //	Converts the time portion of a Date object to a string
  def toUTCString() = ""

  //	Converts a Date object to a string, according to universal time
  def UTC() = ""

  def valueOf() = 0
}

object console {
  def log(s: Any) {
    println(s)
  }
}

class JsDynamic extends Dynamic {
  def apply(a: Any*) = this
  def applyDynamic(name: String)(args: Any*) = this
  def selectDynamic(name: String) = this
  def updateDynamic(name: String)(arg: Any) = this
}
