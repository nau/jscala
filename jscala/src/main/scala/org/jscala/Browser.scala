package org.jscala

object history {
  val length = 0
  def back() {}
  def forward() {}
  def go(i: Int) {}
  def go(url: String) {}
}

object location {
  val hash = ""
  val host = ""
  val hostname = ""
  var href = ""
  val pathname = ""
  val port = 0
  val protocol = ""
  val search = ""
  def assign(url: String) {}
  def reload(forceGet: Boolean = false) {}
  def replace(url: String) {}
}

object navigator {
  val appCodeName = ""
  val appName = ""
  val appVersion = ""
  val cookieEnabled = true
  val onLine = true
  val platform = ""
  val userAgent = ""
  def javaEnabled() = false
  def taintEnabled() = false
}

object screen {
  val availHeight = 0
  // Returns the width of the screen (excluding the Windows Taskbar)
  val availWidth = 0
  // Returns the bit depth of the color palette for displaying images
  val colorDepth = 0
  // Returns the total height of the screen
  val height = 0
  // Returns the color resolution (in bits per pixel) of the screen
  val pixelDepth = 0
  // Returns the total width of the screen
  val width = 0
}

object window {
  // 	Returns a Boolean value indicating whether a window has been closed or not
  val closed = false
  // Sets or returns the default text in the statusbar of a window
  var defaultStatus = ""
  //Returns the Document object for the window (See Document object)
  val document: Doc = null
  // Returns an array of all the frames (including iframes) in the current window
  val frames: Array[Element] = null
  // Returns the History object for the window (See History object)
  val history = org.jscala.history
  // Sets or returns the inner height of a window's content area
  val innerHeight = 0
  // Sets or returns the inner width of a window's content area
  val innerWidth = 0
  // Returns the number of frames (including iframes) in a window
  val length = 0
  // Returns the Location object for the window (See Location object)
  val location = org.jscala.location
  // Sets or returns the name of a window
  var name = ""
  // Returns the Navigator object for the window (See Navigator object)
  val navigator = org.jscala.navigator
  // Returns a reference to the window that created the window
  val opener = this
  // Sets or returns the outer height of a window, including toolbars/scrollbars
  val outerHeight = 0
  // Sets or returns the outer width of a window, including toolbars/scrollbars
  val outerWidth = 0
  // Returns the pixels the current document has been scrolled (horizontally) from the upper left corner of the window
  val pageXOffset = 0
  // Returns the pixels the current document has been scrolled (vertically) from the upper left corner of the window
  val pageYOffset = 0
  // Returns the parent window of the current window
  val parent = this
  // Returns the Screen object for the window (See Screen object)
  val screen = org.jscala.screen
  // Returns the x coordinate of the window relative to the screen
  val screenLeft = 0
  // Returns the y coordinate of the window relative to the screen
  val screenTop = 0
  // Returns the x coordinate of the window relative to the screen
  val screenX = 0
  // Returns the y coordinate of the window relative to the screen
  val screenY = 0
  // Returns the current window
  val self = this
  // Sets the text in the statusbar of a window
  var status = ""
  // Returns the topmost browser window
  val top = this

  // Displays an alert box with a message and an OK button
  def alert(s: Any) {}

  // Removes focus from the current window
  def blur() {}

  // Clears a timer set with setInterval()
  def clearInterval(id: String) {}

  // Clears a timer set with setTimeout()
  def clearTimeout(id: String) {}

  // Closes the current window
  def close() {}

  // Displays a dialog box with a message and an OK and a Cancel button
  def confirm(message: String) = false

  // Creates a pop-up window
  def createPopup() = this

  // Sets focus to the current window
  def focus() {}

  // Moves a window relative to its current position
  def moveBy(x: Int, y: Int) {}

  // Moves a window to the specified position
  def moveTo(x: Int, y: Int) {}

  // Opens a new browser window
  def open(url: String = "", name: String = "", specs: String = "", replace: Boolean = false) {}

  // Prints the content of the current window
  def print() {}

  // Displays a dialog box that prompts the visitor for input
  def prompt(msg: String, defaultText: String = "") {}

  // Resizes the window by the specified pixels
  def resizeBy(width: Int, height: Int) {}

  // Resizes the window to the specified width and height
  def resizeTo(width: Int, height: Int) {}

  def scroll() {}

  // Scrolls the content by the specified number of pixels
  def scrollBy(xnum: Int, ynum: Int) {}

  // Scrolls the content to the specified coordinates
  def scrollTo(xpos: Int, ypos: Int) {}

  // Calls a function or evaluates an expression at specified intervals (in milliseconds)
  def setInterval(code: () => Unit, millisec: Int, lang: String = "") = ""

  def setTimeout(code: () => Unit, millisec: Int, lang: String = "") = ""
}