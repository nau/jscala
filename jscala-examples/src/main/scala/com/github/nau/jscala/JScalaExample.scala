package com.github.nau.jscala

import org.w3c.dom._


object JScalaExample {
  def main(args: Array[String]) {
    object $ {
      def css() = this
    }

//    trait Node
//    trait Document extends Node {
//      val doctype: DocumentType
//      val documentElement: Element

    object document extends Document {
      def getLocalName = ???

      def getDoctype = ???

      def getImplementation = ???

      def getDocumentElement = ???

      def createElement(tagName: String) = ???

      def createDocumentFragment() = ???

      def createTextNode(data: String) = ???

      def createComment(data: String) = ???

      def createCDATASection(data: String) = ???

      def createProcessingInstruction(target: String, data: String) = ???

      def createAttribute(name: String) = ???

      def createEntityReference(name: String) = ???

      def getElementsByTagName(tagname: String) = ???

      def importNode(importedNode: Node, deep: Boolean) = ???

      def createElementNS(namespaceURI: String, qualifiedName: String) = ???

      def createAttributeNS(namespaceURI: String, qualifiedName: String) = ???

      def getElementsByTagNameNS(namespaceURI: String, localName: String) = ???

      def getElementById(elementId: String): Element= ???

      def getInputEncoding = ???

      def getXmlEncoding = ???

      def getXmlStandalone = ???

      def setXmlStandalone(xmlStandalone: Boolean) {}

      def getXmlVersion = ???

      def setXmlVersion(xmlVersion: String) {}

      def getStrictErrorChecking = ???

      def setStrictErrorChecking(strictErrorChecking: Boolean) {}

      def getDocumentURI = ???

      def setDocumentURI(documentURI: String) {}

      def adoptNode(source: Node) = ???

      def getDomConfig = ???

      def normalizeDocument() {}

      def renameNode(n: Node, namespaceURI: String, qualifiedName: String) = ???

      def getNodeName = ???

      def getNodeValue = ???

      def setNodeValue(nodeValue: String) {}

      def getNodeType = ???

      def getParentNode = ???

      def getChildNodes = ???

      def getFirstChild = ???

      def getLastChild = ???

      def getPreviousSibling = ???

      def getNextSibling = ???

      def getAttributes = ???

      def getOwnerDocument = ???

      def insertBefore(newChild: Node, refChild: Node) = ???

      def replaceChild(newChild: Node, oldChild: Node) = ???

      def removeChild(oldChild: Node) = ???

      def appendChild(newChild: Node) = ???

      def hasChildNodes = ???

      def cloneNode(deep: Boolean) = ???

      def normalize() {}

      def isSupported(feature: String, version: String) = ???

      def getNamespaceURI = ???

      def getPrefix = ???

      def setPrefix(prefix: String) {}

      def hasAttributes = ???

      def getBaseURI = ???

      def compareDocumentPosition(other: Node) = ???

      def getTextContent = ???

      def setTextContent(textContent: String) {}

      def isSameNode(other: Node) = ???

      def lookupPrefix(namespaceURI: String) = ???

      def isDefaultNamespace(namespaceURI: String) = ???

      def lookupNamespaceURI(prefix: String) = ???

      def isEqualNode(arg: Node) = ???

      def getFeature(feature: String, version: String) = ???

      def setUserData(key: String, data: Any, handler: UserDataHandler) = ???

      def getUserData(key: String) = ???
    }
    val id = "id123"
    val js = javascript {
      def func1 = 1
      def func2() = ()
      def func3() = 2
      def func4(a: Int) = {
        val b = 5.0
        if (a > 2)
          (a + b) * 2
        else {
          val c = func3 * 2.0
          -c
        }
      }
      def func5(id: String) = {
        document.getElementById(id).getAttribute("something")
      }
      var a = 5.0
      val b = true
      val c = 8.8
      'a'
      +12
      -a
      !b
      (2 + a) * 3
      a = 6 - c
      func5(id)
      def func6(n: Int, f: Int => String) = f(n)
      func6(5, _.toString)
    }
    val js1 = javascript {
      val obj = new {
        val id = document.getElementById("label")
        val payload = 5
      }
      obj.id
    }
    println(js1)
    println(JavascriptPrinter.print(js1, 0))
  }
}

