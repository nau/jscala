package org.jscala

class NodeList[+A <: Node] {
  val length = 0
  def item(index: Int): A = null.asInstanceOf[A]
}

class NodeMap {
  def getNamedItem(key: String): Node = null
  val length = 0
  def item(index: Int): Node = null
  def removeNamedItem(key: String) {}
  def setNamedItem(item: Node) {}
}

trait Node {
  def baseURI = ""
  def childNodes: NodeList[Node] = null
  def firstChild: Node = null
  def lastChild: Node = null
  def localName = ""
  def namespaceURI	= ""
  def nextSibling: Node = null
  def nodeName = ""
  def nodeType = ""
  var nodeValue: AnyRef = null
  def ownerDocument: Doc = null
  def parentNode: Node = null
  var prefix = ""
  def previousSibling: Node = null
  var textContent	= ""
  def text = ""
  //	Adds a new child node, to an element, as the last child node
  def appendChild(node: Node)	{}
  def cloneNode(): Node = null
  def compareDocumentPosition() = 0
  def getFeature(feature: String, version: String): AnyRef = null
  def getUserData(key: String): AnyRef = null
  def hasAttributes = false
  def hasChildNodes	= false
  //	Inserts a new child node before a specified, existing, child node
  def insertBefore(node: Node) {}
  //	Returns true if a specified namespaceURI is the default, otherwise false
  def isDefaultNamespace(uri: String) = false
  //	Checks if two elements are equal
  def isEqualNode(node: Node) = false
  //	Checks if two elements are the same node
  def isSameNode(node: Node) = false
  //	Returns true if a specified feature is supported on the element
  def isSupported(feature: String, version: String) = false
  def lookupNamespaceURI(prefix: String) = ""
  def lookupPrefix(prefix: String) = ""
  def normalize() {}
  def removeChild(node: Node) {}
  def replaceChild(node: Node) {}
  def setUserData(key: String, data: AnyRef) {}
}

class Attribute extends Node {
  val isId = false
  val name = ""
  var value	= ""
  val specified	= false
}

trait DocElemCommon {
  //	Returns a NodeList containing all elements with the specified tagname
  def getElementsByTagName(name: String): NodeList[Element] = null
  //	Sets or returns the title attribute of an element
  var title = ""
}

class Element extends Node with DocElemCommon {
  // Sets or returns the accesskey for an element
  var accessKey = ""

  //	Returns a NamedNodeMap of an element's attributes
  def attributes: NodeMap = null

  //	Sets or returns the class attribute of an element
  var className = ""

  //	Returns the viewable height of an element
  def clientHeight = 0

  //	Returns the viewable width of an element
  def clientWidth = 0

  //	Sets or returns the text direction of an element
  var dir = ""

  //	Returns the specified attribute value of an element node
  def getAttribute(name: String) = ""

  //	Returns the specified attribute node
  def getAttributeNode(name: String): Attribute = null

  //	Sets or returns the id of an element
  var id = ""

  //	Sets or returns the content of an element
  var innerHTML = ""

  //	Sets or returns the language code for an element
  var lang = ""

  //	Returns the height of an element
  def offsetHeight = 0

  //	Returns the width of an element
  def offsetWidth = 0

  //	Returns the horizontal offset position of an element
  def offsetLeft = 0

  //	Returns the offset container of an element
  def offsetParent = 0

  //	Returns the vertical offset position of an element
  def offsetTop = 0

  //	Removes a specified attribute from an element
  def removeAttribute() {}

  //	Removes a specified attribute node, and returns the removed node
  def removeAttributeNode() {}

  //	Removes a child node from an element
  def removeChild() {}

  //	Replaces a child node in an element
  def replaceChild() {}

  //	Returns the entire height of an element
  def scrollHeight = 0

  //	Returns the distance between the left edge of an element and the view
  def scrollLeft = 0

  //	Returns the distance between the top edge of an element and the view
  def scrollTop = 0

  //	Returns the entire width of an element
  def scrollWidth = 0

  //	Sets or changes the specified attribute, to the specified value
  def setAttribute(name: String, value: String) {}

  //	Sets or changes the specified attribute node
  def setAttributeNode(attr: Attribute) {}

  //	Sets or returns the style attribute of an element
  var style: Attribute = null

  //	Sets or returns the tab order of an element
  def tabIndex = 1

  //	Returns the tag name of an element
  var tagName = ""

}

trait Doc extends Node with DocElemCommon {
  //(node)	Returns an adapded node from another document to this document.
  def adoptNode(node: Node): Node = null

  //	Returns a collection of all the anchors in the document
  def anchors: Array[Element] = null

  //	Returns a collection of all the applets in the document
  def applets: Array[Element] = null

  //	Returns the body element of the document
  def body: Element = null

  //	Closes the output stream previously opened with document.open()
  def close() {}

  //	Returns all name/value pairs of cookies in the document
  def cookie = ""

  //	Creates an attribute node
  def createAttribute(): Attribute = null

  //	Creates a Comment node with the specified text
  def createComment(): Element = null

  //	Creates an empty DocumentFragment node
  def createDocumentFragment(): Element = null

  //	Creates an Element node
  def createElement(): Element = null

  //	Creates a Text node
  def createTextNode(): Element = null

  //	Returns the Document Type Declaration associated with the document
  def doctype = ""

  //	Returns the Document Element of the document (the HTML element)
  def documentElement: Element = null

  //	Returns the mode used by the browser to render the document
  def documentMode = ""

  //	Sets or returns the location of the document
  def documentURI = ""

  //	Returns the domain name of the server that loaded the document
  def domain = ""

  //	Returns the configuration used when normalizeDocument() is invoked
  def domConfig = ""

  //	Returns a collection of all the forms in the document
  def forms: Array[Element] = null

  //	Returns the element that has the ID attribute with the specified value
  def getElementById(id: String): Element = null

  //	Accesses all elements with a specified name
  def getElementsByName(name: String): NodeList[Element] = null

  //	Returns a collection of all the images in the document
  def images: Array[Element] = null

  //	Returns the DOMImplementation object that handles this document
  def implementation: AnyRef = ""

  //	Imports a node from another document
  def importNode(node: Node, deep: Boolean) {}

  //	Returns the encoding, character set, used for the document
  def inputEncoding = ""

  //	Returns the date and time the document was last modified
  def lastModified = new Date

  //	Returns a collection of all the links in the document
  def links: Array[Element] = null

  //	Removes empty Text nodes, and joins adjacent nodes
  def normalizeDocument() {}

  //	Opens an HTML output stream to collect output from document.write()
  def open() {}

  //	Returns the (loading) status of the document
  def readyState = ""

  //	Returns the URL of the document that loaded the current document
  def referrer = ""

  //	Renames the specified node
  def renameNode() {}

  //	Sets or returns whether error-checking is enforced or not
  def strictErrorChecking = ""

  //	Returns the full URL of the document
  def URL = ""

  //	Writes HTML expressions or JavaScript code to a document
  def write() {}

  //	Same as write(), but adds a newline character after each statement
  def writeln() {}
}

object document extends Doc


