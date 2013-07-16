package com.github.nau.jscala

trait Doc {
  // Returns a collection of all the anchors in the document
  def anchors(idx: Int): String = ""
  def anchors(idx: String): String = ""
  // Returns a collection of all the applets in the document
  def applets: Array[String] = Array()
  // 	Returns the absolute base URI of a document
  def baseURI: String = ""
  // 	Returns the body element of the document
  def body: Element = null
  // 	Closes the output stream previously opened with document.open()
  def close() {}
  // 	Returns all name/value pairs of cookies in the document
  def cookie: Map[String, String] = Map()
  // 	Creates an attribute node
  def createAttribute() {}
  // 	Creates an Element node
  def createElement(): Element = null
  // 	Creates a Text node
  def createTextNode(): Element = null
  // 	Returns the Document Type Declaration associated with the document
  def doctype: String = ""
  // Returns the Document Element of the document (the HTML element)
  def documentElement: Element = null
  //Returns the mode used by the browser to render the document
  def documentMode: String = ""
  def documentURI: String = ""
  def getElementById(id: String): Element = null
}

object document extends Doc
