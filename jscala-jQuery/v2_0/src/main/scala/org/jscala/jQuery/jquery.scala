package org.jscala

package jquery {
  /*
trait JQueryAjaxSettings  {
  var accepts: Any = _
  var async: Boolean = _
  def beforeSend(jqXHR: JQueryXHR, settings: JQueryAjaxSettings): Dynamic
  var cache: Boolean = _
  def complete(jqXHR: JQueryXHR, textStatus: String): Dynamic
  var contents: Any = _
  var contentType: Any = _
  var context: Any = _
  var converters: Any = _
  var crossDomain: Boolean = _
  var data: Any = _
  def dataFilter(data: Any, ty: Any): Dynamic
  var dataType: String = _
  def error(jqXHR: JQueryXHR, textStatus: String, errorThrow: String): Dynamic
  var global: Boolean = _
  var headers: Any = _
  var ifModified: Boolean = _
  var isLocal: Boolean = _
  var jsonp: String = _
  var jsonpCallback: Any = _
  var mimeType: String = _
  var password: String = _
  var processData: Boolean = _
  var scriptCharset: String = _
  var statusCode: Any = _
  def success(data: Any, textStatus: String, jqXHR: JQueryXHR): Dynamic
  var timeout: Number = _
  var traditional: Boolean = _
  var `type`: String = _
  var url: String = _
  var username: String = _
  var xhr: Any = _
  var xhrFields: Any = _
}

trait JQueryXHR extends XMLHttpRequest with JQueryPromise[Any] {
  def overrideMimeType(mimeType: String): Dynamic
  def abort(statusText: String): Unit
  def abort(): Unit
}

trait JQueryCallback  {
  def add(callbacks: Any*): Dynamic
  def disable(): Dynamic
  def empty(): Dynamic
  def fire(arguments: Any*): Dynamic
  def fired(): Boolean
  def fireWith(context: Any, args: Any*): Dynamic
  def has(callback: Any): Boolean
  def lock(): Dynamic
  def locked(): Boolean
  def remove(callbacks: Any*): Dynamic
}

trait JQueryGenericPromise[T]  {
  def then[U](onFulfill: Function1[T, U], onReject: Function1[Any, U]): JQueryGenericPromise[U]
  def then[U](onFulfill: Function1[T, U]): JQueryGenericPromise[U]
  def then[U](onFulfill: Function1[T, JQueryGenericPromise[U]], onReject: Function1[Any, U]): JQueryGenericPromise[U]
  def then[U](onFulfill: Function1[T, JQueryGenericPromise[U]]): JQueryGenericPromise[U]
  def then[U](onFulfill: Function1[T, U], onReject: Function1[Any, JQueryGenericPromise[U]]): JQueryGenericPromise[U]
  def then[U](onFulfill: Function1[T, U]): JQueryGenericPromise[U]
  def then[U](onFulfill: Function1[T, JQueryGenericPromise[U]], onReject: Function1[Any, JQueryGenericPromise[U]]): JQueryGenericPromise[U]
  def then[U](onFulfill: Function1[T, JQueryGenericPromise[U]]): JQueryGenericPromise[U]
}

trait JQueryPromise[T]  {
  def always(alwaysCallbacks: Any*): JQueryPromise[T]
  def done(doneCallbacks: Any*): JQueryPromise[T]
  def fail(failCallbacks: Any*): JQueryPromise[T]
  def progress(progressCallbacks: Any*): JQueryPromise[T]
  def pipe(doneFilter: Function1[Any, Any], failFilter: Function1[Any, Any], progressFilter: Function1[Any, Any]): JQueryPromise[Any]
  def pipe(doneFilter: Function1[Any, Any], failFilter: Function1[Any, Any]): JQueryPromise[Any]
  def pipe(doneFilter: Function1[Any, Any]): JQueryPromise[Any]
  def pipe(): JQueryPromise[Any]
  def then[U](onFulfill: Function1[T, U], onReject: Function, onProgress: Function): JQueryPromise[U]
  def then[U](onFulfill: Function1[T, U], onReject: Function): JQueryPromise[U]
  def then[U](onFulfill: Function1[T, U]): JQueryPromise[U]
  def then[U](onFulfill: Function1[T, JQueryGenericPromise[U]], onReject: Function, onProgress: Function): JQueryPromise[U]
  def then[U](onFulfill: Function1[T, JQueryGenericPromise[U]], onReject: Function): JQueryPromise[U]
  def then[U](onFulfill: Function1[T, JQueryGenericPromise[U]]): JQueryPromise[U]
  def then[U](onFulfill: Function1[T, U], onReject: Function, onProgress: Function): JQueryPromise[U]
  def then[U](onFulfill: Function1[T, U], onReject: Function): JQueryPromise[U]
  def then[U](onFulfill: Function1[T, U]): JQueryPromise[U]
  def then[U](onFulfill: Function1[T, JQueryGenericPromise[U]], onReject: Function, onProgress: Function): JQueryPromise[U]
  def then[U](onFulfill: Function1[T, JQueryGenericPromise[U]], onReject: Function): JQueryPromise[U]
  def then[U](onFulfill: Function1[T, JQueryGenericPromise[U]]): JQueryPromise[U]
  def then[U](onFulfill: Function, onReject: Function, onProgress: Function): JQueryPromise[U]
  def then[U](onFulfill: Function, onReject: Function): JQueryPromise[U]
  def then[U](onFulfill: Function): JQueryPromise[U]
  def then[U](onFulfill: Function, onReject: Function, onProgress: Function): JQueryPromise[U]
  def then[U](onFulfill: Function, onReject: Function): JQueryPromise[U]
  def then[U](onFulfill: Function): JQueryPromise[U]
  def then[U](onFulfill: Function, onReject: Function, onProgress: Function): JQueryPromise[U]
  def then[U](onFulfill: Function, onReject: Function): JQueryPromise[U]
  def then[U](onFulfill: Function): JQueryPromise[U]
  def then[U](onFulfill: Function, onReject: Function, onProgress: Function): JQueryPromise[U]
  def then[U](onFulfill: Function, onReject: Function): JQueryPromise[U]
  def then[U](onFulfill: Function): JQueryPromise[U]
}

trait JQueryDeferred[T] extends JQueryPromise[T] {
  def always(alwaysCallbacks: Any*): JQueryDeferred[T]
  def done(doneCallbacks: Any*): JQueryDeferred[T]
  def fail(failCallbacks: Any*): JQueryDeferred[T]
  def progress(progressCallbacks: Any*): JQueryDeferred[T]
  def notify(args: Any*): JQueryDeferred[T]
  def notifyWith(context: Any, args: Any*): JQueryDeferred[T]
  def reject(args: Any*): JQueryDeferred[T]
  def rejectWith(context: Any, args: Any*): JQueryDeferred[T]
  def resolve(`val`: T): JQueryDeferred[T]
  def resolve(args: Any*): JQueryDeferred[T]
  def resolveWith(context: Any, args: Any*): JQueryDeferred[T]
  def state(): String
  def promise(target: Any): JQueryPromise[T]
  def promise(): JQueryPromise[T]
}

trait BaseJQueryEventObject extends Event {
  var data: Any = _
  var delegateTarget: Element = _
  def isDefaultPrevented(): Boolean
  def isImmediatePropogationStopped(): Boolean
  def isPropagationStopped(): Boolean
  var namespace: String = _
  def preventDefault(): Dynamic
  var relatedTarget: Element = _
  var result: Any = _
  def stopImmediatePropagation(): Unit
  def stopPropagation(): Unit
  var pageX: Number = _
  var pageY: Number = _
  var which: Number = _
  var metaKey: Any = _
}

trait JQueryInputEventObject extends BaseJQueryEventObject {
  var altKey: Boolean = _
  var ctrlKey: Boolean = _
  var metaKey: Boolean = _
  var shiftKey: Boolean = _
}

trait JQueryMouseEventObject extends JQueryInputEventObject {
  var button: Number = _
  var clientX: Number = _
  var clientY: Number = _
  var offsetX: Number = _
  var offsetY: Number = _
  var pageX: Number = _
  var pageY: Number = _
  var screenX: Number = _
  var screenY: Number = _
}

trait JQueryKeyEventObject extends JQueryInputEventObject {
  var char: Any = _
  var charCode: Number = _
  var key: Any = _
  var keyCode: Number = _
}

trait JQueryPopStateEventObject extends BaseJQueryEventObject {
  var originalEvent: PopStateEvent = _
}

trait JQueryEventObject extends BaseJQueryEventObject with JQueryInputEventObject with JQueryMouseEventObject with JQueryKeyEventObject with JQueryPopStateEventObject {
}

trait JQuerySupport  {
  var ajax: Boolean = _
  var boxModel: Boolean = _
  var changeBubbles: Boolean = _
  var checkClone: Boolean = _
  var checkOn: Boolean = _
  var cors: Boolean = _
  var cssFloat: Boolean = _
  var hrefNormalized: Boolean = _
  var htmlSerialize: Boolean = _
  var leadingWhitespace: Boolean = _
  var noCloneChecked: Boolean = _
  var noCloneEvent: Boolean = _
  var opacity: Boolean = _
  var optDisabled: Boolean = _
  var optSelected: Boolean = _
  def scriptEval(): Boolean
  var style: Boolean = _
  var submitBubbles: Boolean = _
  var tbody: Boolean = _
}

trait JQueryParam  {
  def apply(obj: Any): String
  def apply(obj: Any, traditional: Boolean): String
}
*/

trait JQueryStatic  {
  /*
  def ajax(settings: JQueryAjaxSettings): JQueryXHR
  def ajax(url: String, settings: JQueryAjaxSettings): JQueryXHR
  def ajax(url: String): JQueryXHR
  def ajaxPrefilter(dataTypes: String, handler: Function3[Any, Any, JQueryXHR, Any]): Dynamic
  def ajaxPrefilter(handler: Function3[Any, Any, JQueryXHR, Any]): Dynamic
  var ajaxSettings: JQueryAjaxSettings = _
  def ajaxSetup(): Unit
  def ajaxSetup(options: JQueryAjaxSettings): Unit
  def get(url: String, data: Any, success: Any, dataType: Any): JQueryXHR
  def get(url: String, data: Any, success: Any): JQueryXHR
  def get(url: String, data: Any): JQueryXHR
  def get(url: String): JQueryXHR
  def getJSON(url: String, data: Any, success: Any): JQueryXHR
  def getJSON(url: String, data: Any): JQueryXHR
  def getJSON(url: String): JQueryXHR
  def getScript(url: String, success: Any): JQueryXHR
  def getScript(url: String): JQueryXHR
  var param: JQueryParam = _
  def post(url: String, data: Any, success: Any, dataType: Any): JQueryXHR
  def post(url: String, data: Any, success: Any): JQueryXHR
  def post(url: String, data: Any): JQueryXHR
  def post(url: String): JQueryXHR
  def Callbacks(flags: String): JQueryCallback
  def Callbacks(): JQueryCallback
  def holdReady(hold: Boolean): Dynamic
  def apply(selector: String, context: Any): JQuery
  def apply(selector: String): JQuery
  def apply(element: Element): JQuery
  def apply(`object`: Any): JQuery
  def apply(elementArray: Array[Element]): JQuery
  def apply(`object`: JQuery): JQuery
  def apply(func: Function): JQuery
  def apply(array: Array[Any]): JQuery
  def apply(): JQuery
  def noConflict(removeAll: Boolean): Object
  def noConflict(): Object
  def when[T](deferreds: JQueryGenericPromise[T]*): JQueryPromise[T]
  def when[T](deferreds: T*): JQueryPromise[T]
  def when[T](deferreds: Any*): JQueryPromise[T]
  def css(e: Any, propertyName: String, value: Any): Dynamic
  def css(e: Any, propertyName: String): Dynamic
  def css(e: Any, propertyName: Any, value: Any): Dynamic
  def css(e: Any, propertyName: Any): Dynamic
  var cssHooks: Any = _
  var cssNumber: Any = _
  def data(element: Element, key: String, value: Any): Dynamic
  def data(element: Element, key: String): Dynamic
  def data(element: Element): Dynamic
  def dequeue(element: Element, queueName: String): Dynamic
  def dequeue(element: Element): Dynamic
  def hasData(element: Element): Boolean
  def queue(element: Element, queueName: String): Array[Any]
  def queue(element: Element): Array[Any]
  def queue(element: Element, queueName: String, newQueueOrCallback: Any): JQuery
  def removeData(element: Element, name: String): JQuery
  def removeData(element: Element): JQuery
  def Deferred[T](beforeStart: Function1[JQueryDeferred[T], Any]): JQueryDeferred[T]
  def Deferred[T](): JQueryDeferred[T]
  var fx: Any = _
  def proxy(fn: Function, context: Any, args: Any*): Dynamic
  def proxy(context: Any, name: String, args: Any*): Dynamic
  var Event: Any = _
  def error(message: Any): JQuery
  var expr: Any = _
  var fn: Any = _
  var isReady: Boolean = _
  var support: JQuerySupport = _
  def contains(container: Element, contained: Element): Boolean
  def each(collection: Any, callback: Function2[Any, Any, Any]): Dynamic
  def each(collection: JQuery, callback: Function2[Number, HTMLElement, Any]): Dynamic
  def each[T](collection: Array[T], callback: Function2[Number, T, Any]): Dynamic
  def extend(target: Any, objs: Any*): Dynamic
  def extend(deep: Boolean, target: Any, objs: Any*): Dynamic
  def globalEval(code: String): Dynamic
  def grep[T](array: Array[T], func: Function2[T, Number, Boolean], invert: Boolean): Array[T]
  def grep[T](array: Array[T], func: Function2[T, Number, Boolean]): Array[T]
  def inArray[T](value: T, array: Array[T], fromIndex: Number): Number
  def inArray[T](value: T, array: Array[T]): Number
  def isArray(obj: Any): Boolean
  def isEmptyObject(obj: Any): Boolean
  def isFunction(obj: Any): Boolean
  def isNumeric(value: Any): Boolean
  def isPlainObject(obj: Any): Boolean
  def isWindow(obj: Any): Boolean
  def isXMLDoc(node: Node): Boolean
  def makeArray(obj: Any): Array[Any]
  def map[T, U](array: Array[T], callback: Function2[T, Number, U]): Array[U]
  def map(array: Any, callback: Function2[Any, Any, Any]): Dynamic
  def merge[T](first: Array[T], second: Array[T]): Array[T]
  def merge[T, U](first: Array[T], second: Array[U]): Array[Any]
  def noop(): Dynamic
  def now(): Number
  def parseJSON(json: String): Dynamic
  def parseXML(data: String): Dynamic
  def queue(element: Element, queueName: String, newQueue: Array[Any]): JQuery
  def trim(str: String): String
  def `type`(obj: Any): String
  def unique(arr: Array[Any]): Array[Any]
  def parseHTML(data: String, context: HTMLElement, keepScripts: Boolean): Array[Any]
  def parseHTML(data: String, context: HTMLElement): Array[Any]
  def parseHTML(data: String): Array[Any]
  def Animation(elem: Any, properties: Any, options: Any): Dynamic
  */
}

  trait JQuery {
    /*
  def ajaxComplete(handler: Any): JQuery
  def ajaxError(handler: Function4[Any, Any, Any, Any, Any]): JQuery
  def ajaxSend(handler: Function4[Any, Any, Any, Any, Any]): JQuery
  def ajaxStart(handler: Function0[Any]): JQuery
  def ajaxStop(handler: Function0[Any]): JQuery
  def ajaxSuccess(handler: Function4[Any, Any, Any, Any, Any]): JQuery
  def load(url: String, data: Any, complete: Any): JQuery
  def load(url: String, data: Any): JQuery
  def load(url: String): JQuery
  def serialize(): String
  def serializeArray(): Array[Any]
  def addClass(classNames: String): JQuery
  def addClass(func: Function2[Any, Any, String]): JQuery
  def addBack(selector: String): JQuery
  def addBack(): JQuery
  def attr(attributeName: String): String
  def attr(attributeName: String, value: Any): JQuery
  def attr(map: Any): JQuery
  def attr(attributeName: String, func: Function2[Any, Any, Any]): JQuery
  def hasClass(className: String): Boolean
  def html(): String
  def html(htmlString: String): JQuery
  def html(htmlContent: Function2[Number, String, String]): JQuery
  def html(obj: JQuery): JQuery
  def prop(propertyName: String): Dynamic
  def prop(propertyName: String, value: Any): JQuery
  def prop(map: Any): JQuery
  def prop(propertyName: String, func: Function2[Any, Any, Any]): JQuery
  def removeAttr(attributeName: Any): JQuery
  def removeClass(className: Any): JQuery
  def removeClass(): JQuery
  def removeClass(func: Function2[Any, Any, Any]): JQuery
  def removeProp(propertyName: Any): JQuery
  def toggleClass(className: Any, swtch: Boolean): JQuery
  def toggleClass(className: Any): JQuery
  def toggleClass(swtch: Boolean): JQuery
  def toggleClass(): JQuery
  def toggleClass(func: Function3[Any, Any, Any, Any]): JQuery
  def `val`(): Dynamic
  def `val`(value: Array[String]): JQuery
  def `val`(value: String): JQuery
  def `val`(value: Number): JQuery
  def `val`(func: Function2[Any, Any, Any]): JQuery
  def css(propertyName: String): String
  def css(propertyNames: Array[String]): String
  def css(properties: Any): JQuery
  def css(propertyName: String, value: Any): JQuery
  def css(propertyName: Any, value: Any): JQuery
  def height(): Number
  def height(value: Number): JQuery
  def height(value: String): JQuery
  def height(func: Function2[Any, Any, Any]): JQuery
  def innerHeight(): Number
  def innerHeight(value: Number): JQuery
  def innerWidth(): Number
  def innerWidth(value: Number): JQuery
  def offset(): Any
  def offset(coordinates: Any): JQuery
  def offset(func: Function2[Any, Any, Any]): JQuery
  def outerHeight(includeMargin: Boolean): Number
  def outerHeight(): Number
  def outerHeight(value: Number, includeMargin: Boolean): JQuery
  def outerHeight(value: Number): JQuery
  def outerWidth(includeMargin: Boolean): Number
  def outerWidth(): Number
  def outerWidth(value: Number, includeMargin: Boolean): JQuery
  def outerWidth(value: Number): JQuery
  def position(): Any
  def scrollLeft(): Number
  def scrollLeft(value: Number): JQuery
  def scrollTop(): Number
  def scrollTop(value: Number): JQuery
  def width(): Number
  def width(value: Number): JQuery
  def width(value: String): JQuery
  def width(func: Function2[Any, Any, Any]): JQuery
  def clearQueue(queueName: String): JQuery
  def clearQueue(): JQuery
  def data(key: String, value: Any): JQuery
  def data(obj: Any): JQuery
  def data(key: String): Dynamic
  def data(): Dynamic
  def dequeue(queueName: String): JQuery
  def dequeue(): JQuery
  def removeData(nameOrList: Any): JQuery
  def removeData(): JQuery
  def promise(`type`: Any, target: Any): JQueryPromise[Any]
  def promise(`type`: Any): JQueryPromise[Any]
  def promise(): JQueryPromise[Any]
  def animate(properties: Any, duration: Any, complete: Function): JQuery
  def animate(properties: Any, duration: Any): JQuery
  def animate(properties: Any): JQuery
  def animate(properties: Any, duration: Any, easing: String, complete: Function): JQuery
  def animate(properties: Any, duration: Any, easing: String): JQuery
  def animate(properties: Any, duration: Any): JQuery
  def animate(properties: Any): JQuery
  def animate(properties: Any, options: Any): JQuery
  def delay(duration: Number, queueName: String): JQuery
  def delay(duration: Number): JQuery
  def fadeIn(duration: Any, callback: Any): JQuery
  def fadeIn(duration: Any): JQuery
  def fadeIn(): JQuery
  def fadeIn(duration: Any, easing: String, callback: Any): JQuery
  def fadeIn(duration: Any, easing: String): JQuery
  def fadeIn(duration: Any): JQuery
  def fadeIn(): JQuery
  def fadeOut(duration: Any, callback: Any): JQuery
  def fadeOut(duration: Any): JQuery
  def fadeOut(): JQuery
  def fadeOut(duration: Any, easing: String, callback: Any): JQuery
  def fadeOut(duration: Any, easing: String): JQuery
    def fadeOut(duration: Any): JQuery
  def fadeOut(): JQuery
  def fadeTo(duration: Any, opacity: Number, callback: Any): JQuery
  def fadeTo(duration: Any, opacity: Number): JQuery
  def fadeTo(duration: Any, opacity: Number, easing: String, callback: Any): JQuery
  def fadeTo(duration: Any, opacity: Number, easing: String): JQuery
  def fadeTo(duration: Any, opacity: Number): JQuery
  def fadeToggle(duration: Any, callback: Any): JQuery
  def fadeToggle(duration: Any): JQuery
  def fadeToggle(): JQuery
  def fadeToggle(duration: Any, easing: String, callback: Any): JQuery
  def fadeToggle(duration: Any, easing: String): JQuery
  def fadeToggle(duration: Any): JQuery
  def fadeToggle(): JQuery
  def finish(): JQuery
  def hide(duration: Any, callback: Any): JQuery
  def hide(duration: Any): JQuery
  def hide(): JQuery
  def hide(duration: Any, easing: String, callback: Any): JQuery
  def hide(duration: Any, easing: String): JQuery
  def hide(duration: Any): JQuery
  def hide(): JQuery
  def show(duration: Any, callback: Any): JQuery
  def show(duration: Any): JQuery
  def show(): JQuery
  def show(duration: Any, easing: String, callback: Any): JQuery
  def show(duration: Any, easing: String): JQuery
  def show(duration: Any): JQuery
  def show(): JQuery
  def slideDown(duration: Any, callback: Any): JQuery
  def slideDown(duration: Any): JQuery
  def slideDown(): JQuery
  def slideDown(duration: Any, easing: String, callback: Any): JQuery
  def slideDown(duration: Any, easing: String): JQuery
  def slideDown(duration: Any): JQuery
  def slideDown(): JQuery
  def slideToggle(duration: Any, callback: Any): JQuery
  def slideToggle(duration: Any): JQuery
  def slideToggle(): JQuery
  def slideToggle(duration: Any, easing: String, callback: Any): JQuery
  def slideToggle(duration: Any, easing: String): JQuery
  def slideToggle(duration: Any): JQuery
  def slideToggle(): JQuery
  def slideUp(duration: Any, callback: Any): JQuery
  def slideUp(duration: Any): JQuery
  def slideUp(): JQuery
  def slideUp(duration: Any, easing: String, callback: Any): JQuery
  def slideUp(duration: Any, easing: String): JQuery
  def slideUp(duration: Any): JQuery
  def slideUp(): JQuery
  def stop(clearQueue: Boolean, jumpToEnd: Boolean): JQuery
  def stop(clearQueue: Boolean): JQuery
  def stop(): JQuery
  def stop(queue: Any, clearQueue: Boolean, jumpToEnd: Boolean): JQuery
  def stop(queue: Any, clearQueue: Boolean): JQuery
  def stop(queue: Any): JQuery
  def stop(): JQuery
  def toggle(duration: Any, callback: Any): JQuery
  def toggle(duration: Any): JQuery
  def toggle(): JQuery
  def toggle(duration: Any, easing: String, callback: Any): JQuery
  def toggle(duration: Any, easing: String): JQuery
  def toggle(duration: Any): JQuery
  def toggle(): JQuery
  def toggle(showOrHide: Boolean): JQuery
  def bind(eventType: String, eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def bind(eventType: String, eventData: Any): JQuery
  def bind(eventType: String): JQuery
  def bind(eventType: String, eventData: Any, preventBubble: Boolean): JQuery
  def bind(eventType: String, preventBubble: Boolean): JQuery
  def bind(events: Any*): JQuery
  def blur(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def blur(eventData: Any): JQuery
  def blur(): JQuery
  def blur(handler: Function1[JQueryEventObject, Any]): JQuery
  def change(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def change(eventData: Any): JQuery
  def change(): JQuery
  def change(handler: Function1[JQueryEventObject, Any]): JQuery
  def click(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def click(eventData: Any): JQuery
  def click(): JQuery
  def click(handler: Function1[JQueryEventObject, Any]): JQuery
  def dblclick(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def dblclick(eventData: Any): JQuery
  def dblclick(): JQuery
  def dblclick(handler: Function1[JQueryEventObject, Any]): JQuery
  def delegate(selector: Any, eventType: String, handler: Function1[JQueryEventObject, Any]): JQuery
  def focus(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def focus(eventData: Any): JQuery
  def focus(): JQuery
  def focus(handler: Function1[JQueryEventObject, Any]): JQuery
  def focusin(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def focusin(handler: Function1[JQueryEventObject, Any]): JQuery
  def focusout(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def focusout(handler: Function1[JQueryEventObject, Any]): JQuery
  def hover(handlerIn: Function1[JQueryEventObject, Any], handlerOut: Function1[JQueryEventObject, Any]): JQuery
  def hover(handlerInOut: Function1[JQueryEventObject, Any]): JQuery
  def keydown(eventData: Any, handler: Function1[JQueryKeyEventObject, Any]): JQuery
  def keydown(eventData: Any): JQuery
  def keydown(): JQuery
  def keydown(handler: Function1[JQueryKeyEventObject, Any]): JQuery
  def keypress(eventData: Any, handler: Function1[JQueryKeyEventObject, Any]): JQuery
  def keypress(eventData: Any): JQuery
  def keypress(): JQuery
  def keypress(handler: Function1[JQueryKeyEventObject, Any]): JQuery
  def keyup(eventData: Any, handler: Function1[JQueryKeyEventObject, Any]): JQuery
  def keyup(eventData: Any): JQuery
  def keyup(): JQuery
  def keyup(handler: Function1[JQueryKeyEventObject, Any]): JQuery
  def load(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def load(eventData: Any): JQuery
  def load(): JQuery
  def load(handler: Function1[JQueryEventObject, Any]): JQuery
  def mousedown(): JQuery
  def mousedown(eventData: Any, handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mousedown(handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mouseevent(eventData: Any, handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mouseevent(handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mouseenter(): JQuery
  def mouseenter(eventData: Any, handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mouseenter(handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mouseleave(): JQuery
  def mouseleave(eventData: Any, handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mouseleave(handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mousemove(): JQuery
  def mousemove(eventData: Any, handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mousemove(handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mouseout(): JQuery
  def mouseout(eventData: Any, handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mouseout(handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mouseover(): JQuery
  def mouseover(eventData: Any, handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mouseover(handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mouseup(): JQuery
  def mouseup(eventData: Any, handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def mouseup(handler: Function1[JQueryMouseEventObject, Any]): JQuery
  def off(events: String, selector: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def off(events: String, selector: Any): JQuery
  def off(events: String): JQuery
  def off(): JQuery
  def off(eventsMap: Any, selector: Any): JQuery
  def off(eventsMap: Any): JQuery
  def on(events: String, selector: Any, data: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def on(events: String, selector: Any, data: Any): JQuery
  def on(events: String, selector: Any): JQuery
  def on(events: String): JQuery
  def on(events: String, selector: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def on(events: String, selector: Any): JQuery
  def on(events: String): JQuery
  def on(eventsMap: Any, selector: Any, data: Any): JQuery
  def on(eventsMap: Any, selector: Any): JQuery
  def on(eventsMap: Any): JQuery
  def one(events: String, selector: Any, data: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def one(events: String, selector: Any, data: Any): JQuery
  def one(events: String, selector: Any): JQuery
  def one(events: String): JQuery
  def one(eventsMap: Any, selector: Any, data: Any): JQuery
  def one(eventsMap: Any, selector: Any): JQuery
  def one(eventsMap: Any): JQuery
  def ready(handler: Any): JQuery
  def resize(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def resize(eventData: Any): JQuery
  def resize(): JQuery
  def resize(handler: Function1[JQueryEventObject, Any]): JQuery
  def scroll(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def scroll(eventData: Any): JQuery
  def scroll(): JQuery
  def scroll(handler: Function1[JQueryEventObject, Any]): JQuery
  def select(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def select(eventData: Any): JQuery
  def select(): JQuery
  def select(handler: Function1[JQueryEventObject, Any]): JQuery
  def submit(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def submit(eventData: Any): JQuery
  def submit(): JQuery
  def submit(handler: Function1[JQueryEventObject, Any]): JQuery
  def trigger(eventType: String, extraParameters: Any*): JQuery
  def trigger(event: JQueryEventObject): JQuery
  def triggerHandler(eventType: String, extraParameters: Any*): Object
  def unbind(eventType: String, handler: Function1[JQueryEventObject, Any]): JQuery
  def unbind(eventType: String): JQuery
  def unbind(): JQuery
  def unbind(eventType: String, fls: Boolean): JQuery
  def unbind(evt: Any): JQuery
  def undelegate(): JQuery
  def undelegate(selector: Any, eventType: String, handler: Function1[JQueryEventObject, Any]): JQuery
  def undelegate(selector: Any, eventType: String): JQuery
  def undelegate(selector: Any, events: Any): JQuery
  def undelegate(namespace: String): JQuery
  def unload(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def unload(eventData: Any): JQuery
  def unload(): JQuery
  def unload(handler: Function1[JQueryEventObject, Any]): JQuery
  var context: Element = _
  var jquery: String = _
  def error(handler: Function1[JQueryEventObject, Any]): JQuery
  def error(eventData: Any, handler: Function1[JQueryEventObject, Any]): JQuery
  def pushStack(elements: Array[Any]): JQuery
  def pushStack(elements: Array[Any], name: Any, arguments: Any): JQuery
  def after(content: Any*): JQuery
  def after(func: Function1[Any, Any]): JQuery
  def append(content: Any*): JQuery
  def append(func: Function2[Any, Any, Any]): JQuery
  def appendTo(target: Any): JQuery
  def before(content: Any*): JQuery
  def before(func: Function1[Any, Any]): JQuery
  def clone(withDataAndEvents: Boolean, deepWithDataAndEvents: Boolean): JQuery
  def clone(withDataAndEvents: Boolean): JQuery
  def clone(): JQuery
  def detach(selector: Any): JQuery
  def detach(): JQuery
  def empty(): JQuery
  def insertAfter(target: Any): JQuery
  def insertBefore(target: Any): JQuery
  def prepend(content: Any*): JQuery
  def prepend(func: Function2[Any, Any, Any]): JQuery
  def prependTo(target: Any): JQuery
  def remove(selector: Any): JQuery
  def remove(): JQuery
  def replaceAll(target: Any): JQuery
  def replaceWith(func: Any): JQuery
  def text(): String
  def text(textString: Any): JQuery
  def text(textString: Function2[Number, String, String]): JQuery
  def toArray(): Array[Any]
  def unwrap(): JQuery
  def wrap(wrappingElement: Any): JQuery
  def wrap(func: Function1[Any, Any]): JQuery
  def wrapAll(wrappingElement: Any): JQuery
  def wrapInner(wrappingElement: Any): JQuery
  def wrapInner(func: Function1[Any, Any]): JQuery
  def each(func: Function2[Any, Element, Any]): JQuery
  def get(index: Number): Dynamic
  def get(): Dynamic
  def index(): Number
  def index(selector: String): Number
  def index(element: Any): Number
  var length: Number = _
  var selector: String = _
  @scala.annotation.JSBracketAccess
  def apply(x: String): Any
  @scala.annotation.JSBracketAccess
  def update(x: String, v: Any): Unit
  @scala.annotation.JSBracketAccess
  def apply(x: Number): HTMLElement
  @scala.annotation.JSBracketAccess
  def update(x: Number, v: HTMLElement): Unit
  def add(selector: String, context: Any): JQuery
  def add(selector: String): JQuery
  def add(elements: Any*): JQuery
  def add(html: String): JQuery
  def add(obj: JQuery): JQuery
  def children(selector: Any): JQuery
  def children(): JQuery
  def closest(selector: String): JQuery
  def closest(selector: String, context: Element): JQuery
  def closest(selector: String): JQuery
  def closest(obj: JQuery): JQuery
  def closest(element: Any): JQuery
  def closest(selectors: Any, context: Element): Array[Any]
  def closest(selectors: Any): Array[Any]
  def contents(): JQuery
  def end(): JQuery
  def eq(index: Number): JQuery
  def filter(selector: String): JQuery
  def filter(func: Function1[Any, Any]): JQuery
  def filter(element: Any): JQuery
  def filter(obj: JQuery): JQuery
  def find(selector: String): JQuery
  def find(element: Any): JQuery
  def find(obj: JQuery): JQuery
  def first(): JQuery
  def has(selector: String): JQuery
  def has(contained: Element): JQuery
  def is(selector: String): Boolean
  def is(func: Function1[Any, Any]): Boolean
  def is(element: Any): Boolean
  def is(obj: JQuery): Boolean
  def last(): JQuery
  def map(callback: Function2[Any, Element, Any]): JQuery
  def next(selector: String): JQuery
  def next(): JQuery
  def nextAll(selector: String): JQuery
  def nextAll(): JQuery
  def nextUntil(selector: String, filter: String): JQuery
  def nextUntil(selector: String): JQuery
  def nextUntil(): JQuery
  def nextUntil(element: Element, filter: String): JQuery
  def nextUntil(element: Element): JQuery
  def nextUntil(): JQuery
  def nextUntil(obj: JQuery, filter: String): JQuery
  def nextUntil(obj: JQuery): JQuery
  def nextUntil(): JQuery
  def not(selector: String): JQuery
  def not(func: Function1[Any, Any]): JQuery
  def not(element: Any): JQuery
  def not(obj: JQuery): JQuery
  def offsetParent(): JQuery
  def parent(selector: String): JQuery
  def parent(): JQuery
  def parents(selector: String): JQuery
  def parents(): JQuery
  def parentsUntil(selector: String, filter: String): JQuery
  def parentsUntil(selector: String): JQuery
  def parentsUntil(): JQuery
  def parentsUntil(element: Element, filter: String): JQuery
  def parentsUntil(element: Element): JQuery
  def parentsUntil(): JQuery
  def parentsUntil(obj: JQuery, filter: String): JQuery
  def parentsUntil(obj: JQuery): JQuery
  def parentsUntil(): JQuery
  def prev(selector: String): JQuery
  def prev(): JQuery
  def prevAll(selector: String): JQuery
  def prevAll(): JQuery
  def prevUntil(selector: String, filter: String): JQuery
  def prevUntil(selector: String): JQuery
  def prevUntil(): JQuery
  def prevUntil(element: Element, filter: String): JQuery
  def prevUntil(element: Element): JQuery
  def prevUntil(): JQuery
  def prevUntil(obj: JQuery, filter: String): JQuery
  def prevUntil(obj: JQuery): JQuery
  def prevUntil(): JQuery
  def siblings(selector: String): JQuery
  def siblings(): JQuery
  def slice(start: Number, end: Number): JQuery
  def slice(start: Number): JQuery
  def queue(queueName: String): Array[Any]
  def queue(): Array[Any]
  def queue(queueName: String, newQueueOrCallback: Any): JQuery
  def queue(newQueueOrCallback: Any): JQuery
  * 
  */
  }

}
