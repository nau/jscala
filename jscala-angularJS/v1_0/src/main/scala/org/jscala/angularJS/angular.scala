package org.jscala

import org.jscala.jquery.JQuery
import org.jscala.jquery.JQueryStatic
import org.jscala.{Doc=>Document}
import scala.{AnyRef => obj}

package angularJS {

  trait Function {
    var $inject: Array[String]
  }

  package ng {

    trait IServiceProvider {
      def $get(): Dynamic
    }

    trait IAngularStatic {
      def bind(context: Any, fn: Function, args: Any*): Function
      def bootstrap(element: String, modules: Array[Any]): auto.IInjectorService
      def bootstrap(element: String): auto.IInjectorService
      def bootstrap(element: JQuery, modules: Array[Any]): auto.IInjectorService
      def bootstrap(element: JQuery): auto.IInjectorService
      def bootstrap(element: Element, modules: Array[Any]): auto.IInjectorService
      def bootstrap(element: Element): auto.IInjectorService
      def bootstrap(element: Document, modules: Array[Any]): auto.IInjectorService
      def bootstrap(element: Document): auto.IInjectorService
      def copy(source: Any, destination: Any): Dynamic
      def copy(source: Any): Dynamic
      var element: IAugmentedJQueryStatic
      def equals(value1: Any, value2: Any): Boolean
      def extend(destination: Any, sources: Any*): Dynamic
      def forEach(obj: Any, iterator: Function2[Any, Any, Any], context: Any): Dynamic
      def forEach(obj: Any, iterator: Function2[Any, Any, Any]): Dynamic
      def fromJson(json: String): Dynamic
      def identity(arg: Any): Dynamic
      def identity(): Dynamic
      def injector(modules: Array[Any]): auto.IInjectorService
      def injector(): auto.IInjectorService
      def isArray(value: Any): Boolean
      def isDate(value: Any): Boolean
      def isDefined(value: Any): Boolean
      def isElement(value: Any): Boolean
      def isFunction(value: Any): Boolean
      def isNumber(value: Any): Boolean
      def isobj(value: Any): Boolean
      def isString(value: Any): Boolean
      def isUndefined(value: Any): Boolean
      def lowercase(str: String): String
      def module(name: String, requires: Array[String], configFunction: Any): IModule
      def module(name: String, requires: Array[String]): IModule
      def module(name: String): IModule
      def noop(args: Any*): Unit
      def toJson(obj: Any, pretty: Boolean): String
      def toJson(obj: Any): String
      def uppercase(str: String): String
      var version: Any
    }

    trait IModule {
      def animation(name: String, animationFactory: Function): IModule
      def animation(name: String, inlineAnnotadedFunction: Array[Any]): IModule
      def animation(obj: obj): IModule
      def config(configFn: Function): IModule
      def config(inlineAnnotadedFunction: Array[Any]): IModule
      def constant(name: String, value: Any): IModule
      def constant(obj: obj): IModule
      def controller(name: String, controllerConstructor: Function): IModule
      def controller(name: String, inlineAnnotadedConstructor: Array[Any]): IModule
      def controller(obj: obj): IModule
      def directive(name: String, directiveFactory: Function): IModule
      def directive(name: String, inlineAnnotadedFunction: Array[Any]): IModule
      def directive(obj: obj): IModule
      def factory(name: String, serviceFactoryFunction: Function): IModule
      def factory(name: String, inlineAnnotadedFunction: Array[Any]): IModule
      def factory(obj: obj): IModule
      def filter(name: String, filterFactoryFunction: Function): IModule
      def filter(name: String, inlineAnnotadedFunction: Array[Any]): IModule
      def filter(obj: obj): IModule
      def provider(name: String, serviceProviderConstructor: Function): IModule
      def provider(name: String, inlineAnnotadedConstructor: Array[Any]): IModule
      def provider(obj: obj): IModule
      def run(initializationFunction: Function): IModule
      def run(inlineAnnotadedFunction: Array[Any]): IModule
      def service(name: String, serviceConstructor: Function): IModule
      def service(name: String, inlineAnnotadedConstructor: Array[Any]): IModule
      def service(obj: obj): IModule
      def value(name: String, value: Any): IModule
      def value(obj: obj): IModule
      var name: String
      var requires: Array[String]
    }

    trait IAttributes {
      def $set(name: String, value: Any): Unit
      def $observe(name: String, fn: Function1[Any, Any]): Unit
      var $attr: Any
    }

    trait IFormController {
      var $pristine: Boolean
      var $dirty: Boolean
      var $valid: Boolean
      var $invalid: Boolean
      var $error: Any
      def $setDirty(): Unit
      def $setPristine(): Unit
    }

    trait INgModelController {
      def $render(): Unit
      def $setValidity(validationErrorKey: String, isValid: Boolean): Unit
      def $setViewValue(value: String): Unit
      var $viewValue: Any
      var $modelValue: Any
      var $parsers: Array[IModelParser]
      var $formatters: Array[IModelFormatter]
      var $error: Any
      var $pristine: Boolean
      var $dirty: Boolean
      var $valid: Boolean
      var $invalid: Boolean
    }

    trait IModelParser {
      def apply(value: Any): Dynamic
    }

    trait IModelFormatter {
      def apply(value: Any): Dynamic
    }

    trait IScope {
      def $apply(): Dynamic
      def $apply(exp: String): Dynamic
      def $apply(exp: Function1[IScope, Any]): Dynamic
      def $broadcast(name: String, args: Any*): IAngularEvent
      def $destroy(): Unit
      def $digest(): Unit
      def $emit(name: String, args: Any*): IAngularEvent
      def $eval(expression: String): Dynamic
      def $eval(expression: Function1[IScope, Any]): Dynamic
      def $evalAsync(expression: String): Unit
      def $evalAsync(expression: Function1[IScope, Any]): Unit
      def $new(isolate: Boolean): IScope
      def $new(): IScope
      def $on(name: String, listener: Function): Function
      def $watch(watchExpression: String, listener: String, objEquality: Boolean): Function
      def $watch(watchExpression: String, listener: String): Function
      def $watch(watchExpression: String): Function
      def $watch(watchExpression: String, listener: Function3[Any, Any, IScope, Any], objEquality: Boolean): Function
      def $watch(watchExpression: String, listener: Function3[Any, Any, IScope, Any]): Function
      //def $watch(watchExpression: String): Function
      def $watch(watchExpression: Function1[IScope, Any]): Function
      def $watch(watchExpression: Function1[IScope, Any], listener: String, objEquality: Boolean): Function
      def $watch(watchExpression: Function1[IScope, Any], listener: String): Function
      //def $watch(watchExpression: Function1[IScope, Any]): Function
      def $watch(watchExpression: Function1[IScope, Any], listener: Function3[Any, Any, IScope, Any], objEquality: Boolean): Function
      def $watch(watchExpression: Function1[IScope, Any], listener: Function3[Any, Any, IScope, Any]): Function
      def $watchCollection(watchExpression: String, listener: Function3[Any, Any, IScope, Any]): Function
      def $watchCollection(watchExpression: Function1[IScope, Any], listener: Function3[Any, Any, IScope, Any]): Function
      var $parent: IScope
      var $id: Number
      var $$isolateBindings: Any
      var $$phase: Any
    }

    trait IAngularEvent {
      var targetScope: IScope
      var currentScope: IScope
      var name: String
      var preventDefault: Function
      var defaultPrevented: Boolean
      var stopPropagation: Function
    }

    trait IWindowService { //extends Window {
    }

    trait IBrowserService {
    }

    trait ITimeoutService {
      def apply(func: Function, delay: Number, invokeApply: Boolean): IPromise[Any]
      def apply(func: Function, delay: Number): IPromise[Any]
      def apply(func: Function): IPromise[Any]
      def cancel(promise: IPromise[Any]): Boolean
    }

    trait IFilterService {
      def apply(name: String): Function
    }

    trait IFilterProvider extends IServiceProvider {
      def register(name: String, filterFactory: Function): IServiceProvider
    }

    trait ILocaleService {
      var id: String
      var NUMBER_FORMATS: ILocaleNumberFormatDescriptor
      var DATETIME_FORMATS: ILocaleDateTimeFormatDescriptor
      var pluralCat: Function1[Any, String]
    }

    trait ILocaleNumberFormatDescriptor {
      var DECIMAL_SEP: String
      var GROUP_SEP: String
      var PATTERNS: Array[ILocaleNumberPatternDescriptor]
      var CURRENCY_SYM: String
    }

    trait ILocaleNumberPatternDescriptor {
      var minInt: Number
      var minFrac: Number
      var maxFrac: Number
      var posPre: String
      var posSuf: String
      var negPre: String
      var negSuf: String
      var gSize: Number
      var lgSize: Number
    }

    trait ILocaleDateTimeFormatDescriptor {
      var MONTH: Array[String]
      var SHORTMONTH: Array[String]
      var DAY: Array[String]
      var SHORTDAY: Array[String]
      var AMPMS: Array[String]
      var medium: String
      var short: String
      var fullDate: String
      var longDate: String
      var mediumDate: String
      var shortDate: String
      var mediumTime: String
      var shortTime: String
    }

    trait ILogService {
      var debug: ILogCall
      var error: ILogCall
      var info: ILogCall
      var log: ILogCall
      var warn: ILogCall
    }

    trait ILogCall {
      def apply(args: Any*): Unit
    }

    trait IParseService {
      def apply(expression: String): ICompiledExpression
    }

    trait ICompiledExpression {
      def apply(context: Any, locals: Any): Dynamic
      def apply(context: Any): Dynamic
      def assign(context: Any, value: Any): Dynamic
    }

    trait ILocationService {
      def absUrl(): String
      def hash(): String
      def hash(newHash: String): ILocationService
      def host(): String
      def path(): String
      def path(newPath: String): ILocationService
      def port(): Number
      def protocol(): String
      def replace(): ILocationService
      def search(): Dynamic
      def search(parametersMap: Any): ILocationService
      def search(parameter: String, parameterValue: Any): ILocationService
      def url(): String
      def url(url: String): ILocationService
    }

    trait ILocationProvider extends IServiceProvider {
      def hashPrefix(): String
      def hashPrefix(prefix: String): ILocationProvider
      def html5Mode(): Boolean
      def html5Mode(active: Boolean): ILocationProvider
    }

    trait IDocumentService extends Document {
    }

    trait IExceptionHandlerService {
      def apply(exception: Error, cause: String): Unit
      def apply(exception: Error): Unit
    }

    trait IRootElementService extends JQuery {
    }

    trait IQService {
      def all(promises: Array[IPromise[Any]]): IPromise[Array[Any]]
      def defer[T](): IDeferred[T]
      def reject(reason: Any): IPromise[Unit]
      def reject(): IPromise[Unit]
      def when[T](value: T): IPromise[T]
    }

    trait IPromise[T] {
      def then[TResult](successCallback: Function1[T, IHttpPromise[TResult]], errorCallback: Function1[Any, Any]): IPromise[TResult]
      def then[TResult](successCallback: Function1[T, IHttpPromise[TResult]]): IPromise[TResult]
      //def then[TResult](successCallback: Function1[T, IPromise[TResult]], errorCallback: Function1[Any, Any]): IPromise[TResult]
      //def then[TResult](successCallback: Function1[T, IPromise[TResult]]): IPromise[TResult]
      //def then[TResult](successCallback: Function1[T, TResult], errorCallback: Function1[Any, TResult]): IPromise[TResult]
      //def then[TResult](successCallback: Function1[T, TResult]): IPromise[TResult]
    }

    trait IDeferred[T] {
      def resolve(value: T): Unit
      def resolve(): Unit
      def reject(reason: Any): Unit
      def reject(): Unit
      var promise: IPromise[T]
    }

    trait IAnchorScrollService {
      def apply(): Unit
    }

    trait IAnchorScrollProvider extends IServiceProvider {
      def disableAutoScrolling(): Unit
    }

    trait ICacheFactoryService {
      def apply(cacheId: String, optionsMap: Any): ICacheobj
      def apply(cacheId: String): ICacheobj
      def info(): Dynamic
      def get(cacheId: String): ICacheobj
    }

    trait ICacheobj {
      def info(): Any
      def put(key: String, value: Any): Unit
      def put(key: String): Unit
      def get(key: String): Dynamic
      def remove(key: String): Unit
      def removeAll(): Unit
      def destroy(): Unit
    }

    trait ICompileService {
      def apply(element: String, transclude: ITemplateLinkingFunction, maxPriority: Number): ITemplateLinkingFunction
      def apply(element: String, transclude: ITemplateLinkingFunction): ITemplateLinkingFunction
      def apply(element: String): ITemplateLinkingFunction
      def apply(element: Element, transclude: ITemplateLinkingFunction, maxPriority: Number): ITemplateLinkingFunction
      def apply(element: Element, transclude: ITemplateLinkingFunction): ITemplateLinkingFunction
      def apply(element: Element): ITemplateLinkingFunction
      def apply(element: JQuery, transclude: ITemplateLinkingFunction, maxPriority: Number): ITemplateLinkingFunction
      def apply(element: JQuery, transclude: ITemplateLinkingFunction): ITemplateLinkingFunction
      def apply(element: JQuery): ITemplateLinkingFunction
    }

    trait ICompileProvider extends IServiceProvider {
      def directive(name: String, directiveFactory: Function): ICompileProvider
      def directive(directivesMap: Any): ICompileProvider
    }

    trait ITemplateLinkingFunction {
      def apply(scope: IScope, cloneAttachFn: Function2[JQuery, IScope, Any]): JQuery
      def apply(scope: IScope): JQuery
    }

    trait IControllerService {
      def apply(controllerConstructor: Function, locals: Any): Dynamic
      def apply(controllerConstructor: Function): Dynamic
      def apply(controllerName: String, locals: Any): Dynamic
      def apply(controllerName: String): Dynamic
    }

    trait IControllerProvider extends IServiceProvider {
      def register(name: String, controllerConstructor: Function): Unit
      def register(name: String, dependencyAnnotadedConstructor: Array[Any]): Unit
    }

    trait IHttpService {
      def apply(config: IRequestConfig): IHttpPromise[Any]
      def get(url: String, RequestConfig: Any): IHttpPromise[Any]
      def get(url: String): IHttpPromise[Any]
      def delete(url: String, RequestConfig: Any): IHttpPromise[Any]
      def delete(url: String): IHttpPromise[Any]
      def head(url: String, RequestConfig: Any): IHttpPromise[Any]
      def head(url: String): IHttpPromise[Any]
      def jsonp(url: String, RequestConfig: Any): IHttpPromise[Any]
      def jsonp(url: String): IHttpPromise[Any]
      def post(url: String, data: Any, RequestConfig: Any): IHttpPromise[Any]
      def post(url: String, data: Any): IHttpPromise[Any]
      def put(url: String, data: Any, RequestConfig: Any): IHttpPromise[Any]
      def put(url: String, data: Any): IHttpPromise[Any]
      var defaults: IRequestConfig
      var pendingRequests: Array[Any]
    }

    trait IRequestConfig {
      var method: String
      var url: String
      var params: Any
      var headers: Any
      var cache: Any
      var timeout: Number
      var withCredentials: Boolean
      var data: Any
      var transformRequest: Any
      var transformResponse: Any
    }

    trait IHttpPromiseCallback[T] {
      def apply(data: T, status: Number, headers: Function1[String, String], config: IRequestConfig): Unit
    }

    trait IHttpPromiseCallbackArg[T] {
      var data: T
      var status: Number
      var headers: Function1[String, String]
      var config: IRequestConfig
    }

    trait IHttpPromise[T] extends IPromise[T] {
      def success(callback: IHttpPromiseCallback[T]): IHttpPromise[T]
      def error(callback: IHttpPromiseCallback[T]): IHttpPromise[T]
      //def then[TResult](successCallback: Function1[IHttpPromiseCallbackArg[T], TResult], errorCallback: Function1[IHttpPromiseCallbackArg[T], Any]): IPromise[TResult]
      //def then[TResult](successCallback: Function1[IHttpPromiseCallbackArg[T], TResult]): IPromise[TResult]
      //def then[TResult](successCallback: Function1[IHttpPromiseCallbackArg[T], IPromise[TResult]], errorCallback: Function1[IHttpPromiseCallbackArg[T], Any]): IPromise[TResult]
      //def then[TResult](successCallback: Function1[IHttpPromiseCallbackArg[T], IPromise[TResult]]): IPromise[TResult]
    }

    trait IHttpProvider extends IServiceProvider {
      var defaults: IRequestConfig
      var interceptors: Array[Any]
      var responseInterceptors: Array[Any]
    }

    trait IHttpBackendService {
      def apply(method: String, url: String, post: Any, callback: Function, headers: Any, timeout: Number, withCredentials: Boolean): Unit
      def apply(method: String, url: String, post: Any, callback: Function, headers: Any, timeout: Number): Unit
      def apply(method: String, url: String, post: Any, callback: Function, headers: Any): Unit
      def apply(method: String, url: String, post: Any, callback: Function): Unit
      def apply(method: String, url: String, post: Any): Unit
      def apply(method: String, url: String): Unit
    }

    trait IInterpolateService {
      def apply(text: String, mustHaveExpression: Boolean): IInterpolationFunction
      def apply(text: String): IInterpolationFunction
      def endSymbol(): String
      def startSymbol(): String
    }

    trait IInterpolationFunction {
      def apply(context: Any): String
    }

    trait IInterpolateProvider extends IServiceProvider {
      def startSymbol(): String
      def startSymbol(value: String): IInterpolateProvider
      def endSymbol(): String
      def endSymbol(value: String): IInterpolateProvider
    }

    trait IRouteParamsService {
    }

    trait ITemplateCacheService extends ICacheobj {
    }

    trait IRootScopeService extends IScope {
    }

    trait IRouteService {
      def reload(): Unit
      var routes: Any
      var current: ICurrentRoute
    }

    trait IRoute {
      var controller: Any
      var name: String
      var template: String
      var templateUrl: Any
      var resolve: Any
      var redirectTo: Any
      var reloadOnSearch: Boolean
    }

    trait ICurrentRoute extends IRoute {
      var locals: Any
      var params: Any
    }

    trait IRouteProvider extends IServiceProvider {
      def otherwise(params: Any): IRouteProvider
      def when(path: String, route: IRoute): IRouteProvider
    }

    trait IDirective {
      var priority: Number
      var template: String
      var templateUrl: String
      var replace: Boolean
      var transclude: Any
      var restrict: String
      var scope: Any
      var link: Function
      var compile: Function
      var controller: Function
    }

    trait IAugmentedJQueryStatic extends JQueryStatic {
      def apply(selector: String, context: Any): IAugmentedJQuery
      def apply(selector: String): IAugmentedJQuery
      def apply(element: Element): IAugmentedJQuery
      def apply(obj: Any): IAugmentedJQuery
      def apply(elementArray: Array[Element]): IAugmentedJQuery
      def apply(obj: JQuery): IAugmentedJQuery
      def apply(func: Function): IAugmentedJQuery
      def apply(array: Array[Any]): IAugmentedJQuery
      def apply(): IAugmentedJQuery
    }

    trait IAugmentedJQuery extends JQuery {
      def find(selector: String): IAugmentedJQuery
      def find(element: Any): IAugmentedJQuery
      def find(obj: JQuery): IAugmentedJQuery
      def controller(name: String): Dynamic
      def injector(): Dynamic
      def scope(): IScope
      def inheritedData(key: String, value: Any): JQuery
      def inheritedData(obj: Any): JQuery
      def inheritedData(key: String): Dynamic
      def inheritedData(): Dynamic
    }

    package auto {

      trait IInjectorService {
        def annotate(fn: Function): Array[String]
        def annotate(inlineAnnotadedFunction: Array[Any]): Array[String]
        def get(name: String): Dynamic
        def instantiate(typeConstructor: Function, locals: Any): Dynamic
        def instantiate(typeConstructor: Function): Dynamic
        def invoke(func: Function, context: Any, locals: Any): Dynamic
        def invoke(func: Function, context: Any): Dynamic
        def invoke(func: Function): Dynamic
      }

      trait IProvideService {
        def constant(name: String, value: Any): Unit
        def decorator(name: String, decorator: Function): Unit
        def decorator(name: String, decoratorInline: Array[Any]): Unit
        def factory(name: String, serviceFactoryFunction: Function): ng.IServiceProvider
        def provider(name: String, provider: ng.IServiceProvider): ng.IServiceProvider
        def provider(name: String, serviceProviderConstructor: Function): ng.IServiceProvider
        def service(name: String, constructor: Function): ng.IServiceProvider
        def value(name: String, value: Any): ng.IServiceProvider
      }

    }

  }

}

/*
package object angularJS {
  var angular: ng.IAngularStatic
}
* /
*/
