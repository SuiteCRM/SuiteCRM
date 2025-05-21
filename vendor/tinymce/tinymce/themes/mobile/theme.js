/**
 * Copyright (c) Tiny Technologies, Inc. All rights reserved.
 * Licensed under the LGPL or a commercial license.
 * For LGPL see License.txt in the project root for license information.
 * For commercial licenses see https://www.tiny.cloud/
 *
 * Version: 5.10.9 (2023-11-15)
 */
(function () {
    'use strict';

    var __assign = function () {
      __assign = Object.assign || function __assign(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
          s = arguments[i];
          for (var p in s)
            if (Object.prototype.hasOwnProperty.call(s, p))
              t[p] = s[p];
        }
        return t;
      };
      return __assign.apply(this, arguments);
    };
    function __rest(s, e) {
      var t = {};
      for (var p in s)
        if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
          t[p] = s[p];
      if (s != null && typeof Object.getOwnPropertySymbols === 'function')
        for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
          if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
            t[p[i]] = s[p[i]];
        }
      return t;
    }
    function __spreadArray(to, from, pack) {
      if (pack || arguments.length === 2)
        for (var i = 0, l = from.length, ar; i < l; i++) {
          if (ar || !(i in from)) {
            if (!ar)
              ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
          }
        }
      return to.concat(ar || Array.prototype.slice.call(from));
    }

    var typeOf = function (x) {
      var t = typeof x;
      if (x === null) {
        return 'null';
      } else if (t === 'object' && (Array.prototype.isPrototypeOf(x) || x.constructor && x.constructor.name === 'Array')) {
        return 'array';
      } else if (t === 'object' && (String.prototype.isPrototypeOf(x) || x.constructor && x.constructor.name === 'String')) {
        return 'string';
      } else {
        return t;
      }
    };
    var isType$1 = function (type) {
      return function (value) {
        return typeOf(value) === type;
      };
    };
    var isSimpleType = function (type) {
      return function (value) {
        return typeof value === type;
      };
    };
    var eq$1 = function (t) {
      return function (a) {
        return t === a;
      };
    };
    var isString = isType$1('string');
    var isObject = isType$1('object');
    var isArray = isType$1('array');
    var isNull = eq$1(null);
    var isBoolean = isSimpleType('boolean');
    var isUndefined = eq$1(undefined);
    var isNullable = function (a) {
      return a === null || a === undefined;
    };
    var isNonNullable = function (a) {
      return !isNullable(a);
    };
    var isFunction = isSimpleType('function');
    var isNumber = isSimpleType('number');

    var noop = function () {
    };
    var compose = function (fa, fb) {
      return function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        return fa(fb.apply(null, args));
      };
    };
    var compose1 = function (fbc, fab) {
      return function (a) {
        return fbc(fab(a));
      };
    };
    var constant$1 = function (value) {
      return function () {
        return value;
      };
    };
    var identity = function (x) {
      return x;
    };
    var tripleEquals = function (a, b) {
      return a === b;
    };
    function curry(fn) {
      var initialArgs = [];
      for (var _i = 1; _i < arguments.length; _i++) {
        initialArgs[_i - 1] = arguments[_i];
      }
      return function () {
        var restArgs = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          restArgs[_i] = arguments[_i];
        }
        var all = initialArgs.concat(restArgs);
        return fn.apply(null, all);
      };
    }
    var not = function (f) {
      return function (t) {
        return !f(t);
      };
    };
    var die = function (msg) {
      return function () {
        throw new Error(msg);
      };
    };
    var apply$1 = function (f) {
      return f();
    };
    var never = constant$1(false);
    var always = constant$1(true);

    var none = function () {
      return NONE;
    };
    var NONE = function () {
      var call = function (thunk) {
        return thunk();
      };
      var id = identity;
      var me = {
        fold: function (n, _s) {
          return n();
        },
        isSome: never,
        isNone: always,
        getOr: id,
        getOrThunk: call,
        getOrDie: function (msg) {
          throw new Error(msg || 'error: getOrDie called on none.');
        },
        getOrNull: constant$1(null),
        getOrUndefined: constant$1(undefined),
        or: id,
        orThunk: call,
        map: none,
        each: noop,
        bind: none,
        exists: never,
        forall: always,
        filter: function () {
          return none();
        },
        toArray: function () {
          return [];
        },
        toString: constant$1('none()')
      };
      return me;
    }();
    var some = function (a) {
      var constant_a = constant$1(a);
      var self = function () {
        return me;
      };
      var bind = function (f) {
        return f(a);
      };
      var me = {
        fold: function (n, s) {
          return s(a);
        },
        isSome: always,
        isNone: never,
        getOr: constant_a,
        getOrThunk: constant_a,
        getOrDie: constant_a,
        getOrNull: constant_a,
        getOrUndefined: constant_a,
        or: self,
        orThunk: self,
        map: function (f) {
          return some(f(a));
        },
        each: function (f) {
          f(a);
        },
        bind: bind,
        exists: bind,
        forall: bind,
        filter: function (f) {
          return f(a) ? me : NONE;
        },
        toArray: function () {
          return [a];
        },
        toString: function () {
          return 'some(' + a + ')';
        }
      };
      return me;
    };
    var from = function (value) {
      return value === null || value === undefined ? NONE : some(value);
    };
    var Optional = {
      some: some,
      none: none,
      from: from
    };

    var cached = function (f) {
      var called = false;
      var r;
      return function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        if (!called) {
          called = true;
          r = f.apply(null, args);
        }
        return r;
      };
    };

    var DeviceType = function (os, browser, userAgent, mediaMatch) {
      var isiPad = os.isiOS() && /ipad/i.test(userAgent) === true;
      var isiPhone = os.isiOS() && !isiPad;
      var isMobile = os.isiOS() || os.isAndroid();
      var isTouch = isMobile || mediaMatch('(pointer:coarse)');
      var isTablet = isiPad || !isiPhone && isMobile && mediaMatch('(min-device-width:768px)');
      var isPhone = isiPhone || isMobile && !isTablet;
      var iOSwebview = browser.isSafari() && os.isiOS() && /safari/i.test(userAgent) === false;
      var isDesktop = !isPhone && !isTablet && !iOSwebview;
      return {
        isiPad: constant$1(isiPad),
        isiPhone: constant$1(isiPhone),
        isTablet: constant$1(isTablet),
        isPhone: constant$1(isPhone),
        isTouch: constant$1(isTouch),
        isAndroid: os.isAndroid,
        isiOS: os.isiOS,
        isWebView: constant$1(iOSwebview),
        isDesktop: constant$1(isDesktop)
      };
    };

    var nativeSlice = Array.prototype.slice;
    var nativeIndexOf = Array.prototype.indexOf;
    var nativePush = Array.prototype.push;
    var rawIndexOf = function (ts, t) {
      return nativeIndexOf.call(ts, t);
    };
    var contains$1 = function (xs, x) {
      return rawIndexOf(xs, x) > -1;
    };
    var exists = function (xs, pred) {
      for (var i = 0, len = xs.length; i < len; i++) {
        var x = xs[i];
        if (pred(x, i)) {
          return true;
        }
      }
      return false;
    };
    var map$2 = function (xs, f) {
      var len = xs.length;
      var r = new Array(len);
      for (var i = 0; i < len; i++) {
        var x = xs[i];
        r[i] = f(x, i);
      }
      return r;
    };
    var each$1 = function (xs, f) {
      for (var i = 0, len = xs.length; i < len; i++) {
        var x = xs[i];
        f(x, i);
      }
    };
    var eachr = function (xs, f) {
      for (var i = xs.length - 1; i >= 0; i--) {
        var x = xs[i];
        f(x, i);
      }
    };
    var filter$2 = function (xs, pred) {
      var r = [];
      for (var i = 0, len = xs.length; i < len; i++) {
        var x = xs[i];
        if (pred(x, i)) {
          r.push(x);
        }
      }
      return r;
    };
    var foldr = function (xs, f, acc) {
      eachr(xs, function (x, i) {
        acc = f(acc, x, i);
      });
      return acc;
    };
    var foldl = function (xs, f, acc) {
      each$1(xs, function (x, i) {
        acc = f(acc, x, i);
      });
      return acc;
    };
    var findUntil = function (xs, pred, until) {
      for (var i = 0, len = xs.length; i < len; i++) {
        var x = xs[i];
        if (pred(x, i)) {
          return Optional.some(x);
        } else if (until(x, i)) {
          break;
        }
      }
      return Optional.none();
    };
    var find$2 = function (xs, pred) {
      return findUntil(xs, pred, never);
    };
    var findIndex$1 = function (xs, pred) {
      for (var i = 0, len = xs.length; i < len; i++) {
        var x = xs[i];
        if (pred(x, i)) {
          return Optional.some(i);
        }
      }
      return Optional.none();
    };
    var flatten = function (xs) {
      var r = [];
      for (var i = 0, len = xs.length; i < len; ++i) {
        if (!isArray(xs[i])) {
          throw new Error('Arr.flatten item ' + i + ' was not an array, input: ' + xs);
        }
        nativePush.apply(r, xs[i]);
      }
      return r;
    };
    var bind$3 = function (xs, f) {
      return flatten(map$2(xs, f));
    };
    var forall = function (xs, pred) {
      for (var i = 0, len = xs.length; i < len; ++i) {
        var x = xs[i];
        if (pred(x, i) !== true) {
          return false;
        }
      }
      return true;
    };
    var reverse = function (xs) {
      var r = nativeSlice.call(xs, 0);
      r.reverse();
      return r;
    };
    var difference = function (a1, a2) {
      return filter$2(a1, function (x) {
        return !contains$1(a2, x);
      });
    };
    var pure$2 = function (x) {
      return [x];
    };
    var sort = function (xs, comparator) {
      var copy = nativeSlice.call(xs, 0);
      copy.sort(comparator);
      return copy;
    };
    var get$d = function (xs, i) {
      return i >= 0 && i < xs.length ? Optional.some(xs[i]) : Optional.none();
    };
    var head = function (xs) {
      return get$d(xs, 0);
    };
    var findMap = function (arr, f) {
      for (var i = 0; i < arr.length; i++) {
        var r = f(arr[i], i);
        if (r.isSome()) {
          return r;
        }
      }
      return Optional.none();
    };

    var firstMatch = function (regexes, s) {
      for (var i = 0; i < regexes.length; i++) {
        var x = regexes[i];
        if (x.test(s)) {
          return x;
        }
      }
      return undefined;
    };
    var find$1 = function (regexes, agent) {
      var r = firstMatch(regexes, agent);
      if (!r) {
        return {
          major: 0,
          minor: 0
        };
      }
      var group = function (i) {
        return Number(agent.replace(r, '$' + i));
      };
      return nu$8(group(1), group(2));
    };
    var detect$4 = function (versionRegexes, agent) {
      var cleanedAgent = String(agent).toLowerCase();
      if (versionRegexes.length === 0) {
        return unknown$3();
      }
      return find$1(versionRegexes, cleanedAgent);
    };
    var unknown$3 = function () {
      return nu$8(0, 0);
    };
    var nu$8 = function (major, minor) {
      return {
        major: major,
        minor: minor
      };
    };
    var Version = {
      nu: nu$8,
      detect: detect$4,
      unknown: unknown$3
    };

    var detectBrowser$1 = function (browsers, userAgentData) {
      return findMap(userAgentData.brands, function (uaBrand) {
        var lcBrand = uaBrand.brand.toLowerCase();
        return find$2(browsers, function (browser) {
          var _a;
          return lcBrand === ((_a = browser.brand) === null || _a === void 0 ? void 0 : _a.toLowerCase());
        }).map(function (info) {
          return {
            current: info.name,
            version: Version.nu(parseInt(uaBrand.version, 10), 0)
          };
        });
      });
    };

    var detect$3 = function (candidates, userAgent) {
      var agent = String(userAgent).toLowerCase();
      return find$2(candidates, function (candidate) {
        return candidate.search(agent);
      });
    };
    var detectBrowser = function (browsers, userAgent) {
      return detect$3(browsers, userAgent).map(function (browser) {
        var version = Version.detect(browser.versionRegexes, userAgent);
        return {
          current: browser.name,
          version: version
        };
      });
    };
    var detectOs = function (oses, userAgent) {
      return detect$3(oses, userAgent).map(function (os) {
        var version = Version.detect(os.versionRegexes, userAgent);
        return {
          current: os.name,
          version: version
        };
      });
    };

    var checkRange = function (str, substr, start) {
      return substr === '' || str.length >= substr.length && str.substr(start, start + substr.length) === substr;
    };
    var supplant = function (str, obj) {
      var isStringOrNumber = function (a) {
        var t = typeof a;
        return t === 'string' || t === 'number';
      };
      return str.replace(/\$\{([^{}]*)\}/g, function (fullMatch, key) {
        var value = obj[key];
        return isStringOrNumber(value) ? value.toString() : fullMatch;
      });
    };
    var contains = function (str, substr) {
      return str.indexOf(substr) !== -1;
    };
    var endsWith = function (str, suffix) {
      return checkRange(str, suffix, str.length - suffix.length);
    };
    var blank = function (r) {
      return function (s) {
        return s.replace(r, '');
      };
    };
    var trim = blank(/^\s+|\s+$/g);

    var normalVersionRegex = /.*?version\/\ ?([0-9]+)\.([0-9]+).*/;
    var checkContains = function (target) {
      return function (uastring) {
        return contains(uastring, target);
      };
    };
    var browsers = [
      {
        name: 'Edge',
        versionRegexes: [/.*?edge\/ ?([0-9]+)\.([0-9]+)$/],
        search: function (uastring) {
          return contains(uastring, 'edge/') && contains(uastring, 'chrome') && contains(uastring, 'safari') && contains(uastring, 'applewebkit');
        }
      },
      {
        name: 'Chrome',
        brand: 'Chromium',
        versionRegexes: [
          /.*?chrome\/([0-9]+)\.([0-9]+).*/,
          normalVersionRegex
        ],
        search: function (uastring) {
          return contains(uastring, 'chrome') && !contains(uastring, 'chromeframe');
        }
      },
      {
        name: 'IE',
        versionRegexes: [
          /.*?msie\ ?([0-9]+)\.([0-9]+).*/,
          /.*?rv:([0-9]+)\.([0-9]+).*/
        ],
        search: function (uastring) {
          return contains(uastring, 'msie') || contains(uastring, 'trident');
        }
      },
      {
        name: 'Opera',
        versionRegexes: [
          normalVersionRegex,
          /.*?opera\/([0-9]+)\.([0-9]+).*/
        ],
        search: checkContains('opera')
      },
      {
        name: 'Firefox',
        versionRegexes: [/.*?firefox\/\ ?([0-9]+)\.([0-9]+).*/],
        search: checkContains('firefox')
      },
      {
        name: 'Safari',
        versionRegexes: [
          normalVersionRegex,
          /.*?cpu os ([0-9]+)_([0-9]+).*/
        ],
        search: function (uastring) {
          return (contains(uastring, 'safari') || contains(uastring, 'mobile/')) && contains(uastring, 'applewebkit');
        }
      }
    ];
    var oses = [
      {
        name: 'Windows',
        search: checkContains('win'),
        versionRegexes: [/.*?windows\ nt\ ?([0-9]+)\.([0-9]+).*/]
      },
      {
        name: 'iOS',
        search: function (uastring) {
          return contains(uastring, 'iphone') || contains(uastring, 'ipad');
        },
        versionRegexes: [
          /.*?version\/\ ?([0-9]+)\.([0-9]+).*/,
          /.*cpu os ([0-9]+)_([0-9]+).*/,
          /.*cpu iphone os ([0-9]+)_([0-9]+).*/
        ]
      },
      {
        name: 'Android',
        search: checkContains('android'),
        versionRegexes: [/.*?android\ ?([0-9]+)\.([0-9]+).*/]
      },
      {
        name: 'OSX',
        search: checkContains('mac os x'),
        versionRegexes: [/.*?mac\ os\ x\ ?([0-9]+)_([0-9]+).*/]
      },
      {
        name: 'Linux',
        search: checkContains('linux'),
        versionRegexes: []
      },
      {
        name: 'Solaris',
        search: checkContains('sunos'),
        versionRegexes: []
      },
      {
        name: 'FreeBSD',
        search: checkContains('freebsd'),
        versionRegexes: []
      },
      {
        name: 'ChromeOS',
        search: checkContains('cros'),
        versionRegexes: [/.*?chrome\/([0-9]+)\.([0-9]+).*/]
      }
    ];
    var PlatformInfo = {
      browsers: constant$1(browsers),
      oses: constant$1(oses)
    };

    var edge = 'Edge';
    var chrome = 'Chrome';
    var ie = 'IE';
    var opera = 'Opera';
    var firefox = 'Firefox';
    var safari = 'Safari';
    var unknown$2 = function () {
      return nu$7({
        current: undefined,
        version: Version.unknown()
      });
    };
    var nu$7 = function (info) {
      var current = info.current;
      var version = info.version;
      var isBrowser = function (name) {
        return function () {
          return current === name;
        };
      };
      return {
        current: current,
        version: version,
        isEdge: isBrowser(edge),
        isChrome: isBrowser(chrome),
        isIE: isBrowser(ie),
        isOpera: isBrowser(opera),
        isFirefox: isBrowser(firefox),
        isSafari: isBrowser(safari)
      };
    };
    var Browser = {
      unknown: unknown$2,
      nu: nu$7,
      edge: constant$1(edge),
      chrome: constant$1(chrome),
      ie: constant$1(ie),
      opera: constant$1(opera),
      firefox: constant$1(firefox),
      safari: constant$1(safari)
    };

    var windows = 'Windows';
    var ios = 'iOS';
    var android = 'Android';
    var linux = 'Linux';
    var osx = 'OSX';
    var solaris = 'Solaris';
    var freebsd = 'FreeBSD';
    var chromeos = 'ChromeOS';
    var unknown$1 = function () {
      return nu$6({
        current: undefined,
        version: Version.unknown()
      });
    };
    var nu$6 = function (info) {
      var current = info.current;
      var version = info.version;
      var isOS = function (name) {
        return function () {
          return current === name;
        };
      };
      return {
        current: current,
        version: version,
        isWindows: isOS(windows),
        isiOS: isOS(ios),
        isAndroid: isOS(android),
        isOSX: isOS(osx),
        isLinux: isOS(linux),
        isSolaris: isOS(solaris),
        isFreeBSD: isOS(freebsd),
        isChromeOS: isOS(chromeos)
      };
    };
    var OperatingSystem = {
      unknown: unknown$1,
      nu: nu$6,
      windows: constant$1(windows),
      ios: constant$1(ios),
      android: constant$1(android),
      linux: constant$1(linux),
      osx: constant$1(osx),
      solaris: constant$1(solaris),
      freebsd: constant$1(freebsd),
      chromeos: constant$1(chromeos)
    };

    var detect$2 = function (userAgent, userAgentDataOpt, mediaMatch) {
      var browsers = PlatformInfo.browsers();
      var oses = PlatformInfo.oses();
      var browser = userAgentDataOpt.bind(function (userAgentData) {
        return detectBrowser$1(browsers, userAgentData);
      }).orThunk(function () {
        return detectBrowser(browsers, userAgent);
      }).fold(Browser.unknown, Browser.nu);
      var os = detectOs(oses, userAgent).fold(OperatingSystem.unknown, OperatingSystem.nu);
      var deviceType = DeviceType(os, browser, userAgent, mediaMatch);
      return {
        browser: browser,
        os: os,
        deviceType: deviceType
      };
    };
    var PlatformDetection = { detect: detect$2 };

    var mediaMatch = function (query) {
      return window.matchMedia(query).matches;
    };
    var platform$1 = cached(function () {
      return PlatformDetection.detect(navigator.userAgent, Optional.from(navigator.userAgentData), mediaMatch);
    });
    var detect$1 = function () {
      return platform$1();
    };

    var constant = constant$1;
    var touchstart = constant('touchstart');
    var touchmove = constant('touchmove');
    var touchend = constant('touchend');
    var mousedown = constant('mousedown');
    var mousemove = constant('mousemove');
    var mouseup = constant('mouseup');
    var mouseover = constant('mouseover');
    var keydown = constant('keydown');
    var keyup = constant('keyup');
    var input$1 = constant('input');
    var change = constant('change');
    var click = constant('click');
    var transitionend = constant('transitionend');
    var selectstart = constant('selectstart');

    var prefixName = function (name) {
      return constant$1('alloy.' + name);
    };
    var alloy = { tap: prefixName('tap') };
    var focus$4 = prefixName('focus');
    var postBlur = prefixName('blur.post');
    var postPaste = prefixName('paste.post');
    var receive$1 = prefixName('receive');
    var execute$5 = prefixName('execute');
    var focusItem = prefixName('focus.item');
    var tap = alloy.tap;
    var longpress = prefixName('longpress');
    var systemInit = prefixName('system.init');
    var attachedToDom = prefixName('system.attached');
    var detachedFromDom = prefixName('system.detached');
    var focusShifted = prefixName('focusmanager.shifted');
    var highlight$1 = prefixName('highlight');
    var dehighlight$1 = prefixName('dehighlight');

    var emit = function (component, event) {
      dispatchWith(component, component.element, event, {});
    };
    var emitWith = function (component, event, properties) {
      dispatchWith(component, component.element, event, properties);
    };
    var emitExecute = function (component) {
      emit(component, execute$5());
    };
    var dispatch = function (component, target, event) {
      dispatchWith(component, target, event, {});
    };
    var dispatchWith = function (component, target, event, properties) {
      var data = __assign({ target: target }, properties);
      component.getSystem().triggerEvent(event, target, data);
    };
    var dispatchEvent = function (component, target, event, simulatedEvent) {
      component.getSystem().triggerEvent(event, target, simulatedEvent.event);
    };
    var dispatchFocus = function (component, target) {
      component.getSystem().triggerFocus(target, component.element);
    };

    var DOCUMENT = 9;
    var DOCUMENT_FRAGMENT = 11;
    var ELEMENT = 1;
    var TEXT = 3;

    var fromHtml$2 = function (html, scope) {
      var doc = scope || document;
      var div = doc.createElement('div');
      div.innerHTML = html;
      if (!div.hasChildNodes() || div.childNodes.length > 1) {
        console.error('HTML does not have a single root node', html);
        throw new Error('HTML must have a single root node');
      }
      return fromDom(div.childNodes[0]);
    };
    var fromTag = function (tag, scope) {
      var doc = scope || document;
      var node = doc.createElement(tag);
      return fromDom(node);
    };
    var fromText = function (text, scope) {
      var doc = scope || document;
      var node = doc.createTextNode(text);
      return fromDom(node);
    };
    var fromDom = function (node) {
      if (node === null || node === undefined) {
        throw new Error('Node cannot be null or undefined');
      }
      return { dom: node };
    };
    var fromPoint = function (docElm, x, y) {
      return Optional.from(docElm.dom.elementFromPoint(x, y)).map(fromDom);
    };
    var SugarElement = {
      fromHtml: fromHtml$2,
      fromTag: fromTag,
      fromText: fromText,
      fromDom: fromDom,
      fromPoint: fromPoint
    };

    var is$1 = function (element, selector) {
      var dom = element.dom;
      if (dom.nodeType !== ELEMENT) {
        return false;
      } else {
        var elem = dom;
        if (elem.matches !== undefined) {
          return elem.matches(selector);
        } else if (elem.msMatchesSelector !== undefined) {
          return elem.msMatchesSelector(selector);
        } else if (elem.webkitMatchesSelector !== undefined) {
          return elem.webkitMatchesSelector(selector);
        } else if (elem.mozMatchesSelector !== undefined) {
          return elem.mozMatchesSelector(selector);
        } else {
          throw new Error('Browser lacks native selectors');
        }
      }
    };
    var bypassSelector = function (dom) {
      return dom.nodeType !== ELEMENT && dom.nodeType !== DOCUMENT && dom.nodeType !== DOCUMENT_FRAGMENT || dom.childElementCount === 0;
    };
    var all$2 = function (selector, scope) {
      var base = scope === undefined ? document : scope.dom;
      return bypassSelector(base) ? [] : map$2(base.querySelectorAll(selector), SugarElement.fromDom);
    };
    var one = function (selector, scope) {
      var base = scope === undefined ? document : scope.dom;
      return bypassSelector(base) ? Optional.none() : Optional.from(base.querySelector(selector)).map(SugarElement.fromDom);
    };

    var eq = function (e1, e2) {
      return e1.dom === e2.dom;
    };

    typeof window !== 'undefined' ? window : Function('return this;')();

    var name$1 = function (element) {
      var r = element.dom.nodeName;
      return r.toLowerCase();
    };
    var type = function (element) {
      return element.dom.nodeType;
    };
    var isType = function (t) {
      return function (element) {
        return type(element) === t;
      };
    };
    var isElement = isType(ELEMENT);
    var isText = isType(TEXT);
    var isDocument = isType(DOCUMENT);
    var isDocumentFragment = isType(DOCUMENT_FRAGMENT);

    var owner$2 = function (element) {
      return SugarElement.fromDom(element.dom.ownerDocument);
    };
    var documentOrOwner = function (dos) {
      return isDocument(dos) ? dos : owner$2(dos);
    };
    var defaultView = function (element) {
      return SugarElement.fromDom(documentOrOwner(element).dom.defaultView);
    };
    var parent = function (element) {
      return Optional.from(element.dom.parentNode).map(SugarElement.fromDom);
    };
    var parents = function (element, isRoot) {
      var stop = isFunction(isRoot) ? isRoot : never;
      var dom = element.dom;
      var ret = [];
      while (dom.parentNode !== null && dom.parentNode !== undefined) {
        var rawParent = dom.parentNode;
        var p = SugarElement.fromDom(rawParent);
        ret.push(p);
        if (stop(p) === true) {
          break;
        } else {
          dom = rawParent;
        }
      }
      return ret;
    };
    var siblings$2 = function (element) {
      var filterSelf = function (elements) {
        return filter$2(elements, function (x) {
          return !eq(element, x);
        });
      };
      return parent(element).map(children).map(filterSelf).getOr([]);
    };
    var nextSibling = function (element) {
      return Optional.from(element.dom.nextSibling).map(SugarElement.fromDom);
    };
    var children = function (element) {
      return map$2(element.dom.childNodes, SugarElement.fromDom);
    };
    var child = function (element, index) {
      var cs = element.dom.childNodes;
      return Optional.from(cs[index]).map(SugarElement.fromDom);
    };
    var firstChild = function (element) {
      return child(element, 0);
    };

    var before$1 = function (marker, element) {
      var parent$1 = parent(marker);
      parent$1.each(function (v) {
        v.dom.insertBefore(element.dom, marker.dom);
      });
    };
    var after$2 = function (marker, element) {
      var sibling = nextSibling(marker);
      sibling.fold(function () {
        var parent$1 = parent(marker);
        parent$1.each(function (v) {
          append$2(v, element);
        });
      }, function (v) {
        before$1(v, element);
      });
    };
    var prepend$1 = function (parent, element) {
      var firstChild$1 = firstChild(parent);
      firstChild$1.fold(function () {
        append$2(parent, element);
      }, function (v) {
        parent.dom.insertBefore(element.dom, v.dom);
      });
    };
    var append$2 = function (parent, element) {
      parent.dom.appendChild(element.dom);
    };
    var appendAt = function (parent, element, index) {
      child(parent, index).fold(function () {
        append$2(parent, element);
      }, function (v) {
        before$1(v, element);
      });
    };

    var append$1 = function (parent, elements) {
      each$1(elements, function (x) {
        append$2(parent, x);
      });
    };

    var empty = function (element) {
      element.dom.textContent = '';
      each$1(children(element), function (rogue) {
        remove$7(rogue);
      });
    };
    var remove$7 = function (element) {
      var dom = element.dom;
      if (dom.parentNode !== null) {
        dom.parentNode.removeChild(dom);
      }
    };

    var isShadowRoot = function (dos) {
      return isDocumentFragment(dos) && isNonNullable(dos.dom.host);
    };
    var supported = isFunction(Element.prototype.attachShadow) && isFunction(Node.prototype.getRootNode);
    var isSupported$1 = constant$1(supported);
    var getRootNode = supported ? function (e) {
      return SugarElement.fromDom(e.dom.getRootNode());
    } : documentOrOwner;
    var getShadowRoot = function (e) {
      var r = getRootNode(e);
      return isShadowRoot(r) ? Optional.some(r) : Optional.none();
    };
    var getShadowHost = function (e) {
      return SugarElement.fromDom(e.dom.host);
    };
    var getOriginalEventTarget = function (event) {
      if (isSupported$1() && isNonNullable(event.target)) {
        var el = SugarElement.fromDom(event.target);
        if (isElement(el) && isOpenShadowHost(el)) {
          if (event.composed && event.composedPath) {
            var composedPath = event.composedPath();
            if (composedPath) {
              return head(composedPath);
            }
          }
        }
      }
      return Optional.from(event.target);
    };
    var isOpenShadowHost = function (element) {
      return isNonNullable(element.dom.shadowRoot);
    };

    var inBody = function (element) {
      var dom = isText(element) ? element.dom.parentNode : element.dom;
      if (dom === undefined || dom === null || dom.ownerDocument === null) {
        return false;
      }
      var doc = dom.ownerDocument;
      return getShadowRoot(SugarElement.fromDom(dom)).fold(function () {
        return doc.body.contains(dom);
      }, compose1(inBody, getShadowHost));
    };
    var body = function () {
      return getBody(SugarElement.fromDom(document));
    };
    var getBody = function (doc) {
      var b = doc.dom.body;
      if (b === null || b === undefined) {
        throw new Error('Body is not available yet');
      }
      return SugarElement.fromDom(b);
    };

    var fireDetaching = function (component) {
      emit(component, detachedFromDom());
      var children = component.components();
      each$1(children, fireDetaching);
    };
    var fireAttaching = function (component) {
      var children = component.components();
      each$1(children, fireAttaching);
      emit(component, attachedToDom());
    };
    var attach$1 = function (parent, child) {
      append$2(parent.element, child.element);
    };
    var detachChildren = function (component) {
      each$1(component.components(), function (childComp) {
        return remove$7(childComp.element);
      });
      empty(component.element);
      component.syncComponents();
    };
    var replaceChildren = function (component, newChildren) {
      var subs = component.components();
      detachChildren(component);
      var deleted = difference(subs, newChildren);
      each$1(deleted, function (comp) {
        fireDetaching(comp);
        component.getSystem().removeFromWorld(comp);
      });
      each$1(newChildren, function (childComp) {
        if (!childComp.getSystem().isConnected()) {
          component.getSystem().addToWorld(childComp);
          attach$1(component, childComp);
          if (inBody(component.element)) {
            fireAttaching(childComp);
          }
        } else {
          attach$1(component, childComp);
        }
        component.syncComponents();
      });
    };

    var attach = function (parent, child) {
      attachWith(parent, child, append$2);
    };
    var attachWith = function (parent, child, insertion) {
      parent.getSystem().addToWorld(child);
      insertion(parent.element, child.element);
      if (inBody(parent.element)) {
        fireAttaching(child);
      }
      parent.syncComponents();
    };
    var doDetach = function (component) {
      fireDetaching(component);
      remove$7(component.element);
      component.getSystem().removeFromWorld(component);
    };
    var detach = function (component) {
      var parent$1 = parent(component.element).bind(function (p) {
        return component.getSystem().getByDom(p).toOptional();
      });
      doDetach(component);
      parent$1.each(function (p) {
        p.syncComponents();
      });
    };
    var attachSystemAfter = function (element, guiSystem) {
      attachSystemWith(element, guiSystem, after$2);
    };
    var attachSystemWith = function (element, guiSystem, inserter) {
      inserter(element, guiSystem.element);
      var children$1 = children(guiSystem.element);
      each$1(children$1, function (child) {
        guiSystem.getByDom(child).each(fireAttaching);
      });
    };
    var detachSystem = function (guiSystem) {
      var children$1 = children(guiSystem.element);
      each$1(children$1, function (child) {
        guiSystem.getByDom(child).each(fireDetaching);
      });
      remove$7(guiSystem.element);
    };

    var keys = Object.keys;
    var hasOwnProperty = Object.hasOwnProperty;
    var each = function (obj, f) {
      var props = keys(obj);
      for (var k = 0, len = props.length; k < len; k++) {
        var i = props[k];
        var x = obj[i];
        f(x, i);
      }
    };
    var map$1 = function (obj, f) {
      return tupleMap(obj, function (x, i) {
        return {
          k: i,
          v: f(x, i)
        };
      });
    };
    var tupleMap = function (obj, f) {
      var r = {};
      each(obj, function (x, i) {
        var tuple = f(x, i);
        r[tuple.k] = tuple.v;
      });
      return r;
    };
    var objAcc = function (r) {
      return function (x, i) {
        r[i] = x;
      };
    };
    var internalFilter = function (obj, pred, onTrue, onFalse) {
      var r = {};
      each(obj, function (x, i) {
        (pred(x, i) ? onTrue : onFalse)(x, i);
      });
      return r;
    };
    var filter$1 = function (obj, pred) {
      var t = {};
      internalFilter(obj, pred, objAcc(t), noop);
      return t;
    };
    var mapToArray = function (obj, f) {
      var r = [];
      each(obj, function (value, name) {
        r.push(f(value, name));
      });
      return r;
    };
    var find = function (obj, pred) {
      var props = keys(obj);
      for (var k = 0, len = props.length; k < len; k++) {
        var i = props[k];
        var x = obj[i];
        if (pred(x, i, obj)) {
          return Optional.some(x);
        }
      }
      return Optional.none();
    };
    var values = function (obj) {
      return mapToArray(obj, identity);
    };
    var get$c = function (obj, key) {
      return has$2(obj, key) ? Optional.from(obj[key]) : Optional.none();
    };
    var has$2 = function (obj, key) {
      return hasOwnProperty.call(obj, key);
    };
    var hasNonNullableKey = function (obj, key) {
      return has$2(obj, key) && obj[key] !== undefined && obj[key] !== null;
    };

    var rawSet = function (dom, key, value) {
      if (isString(value) || isBoolean(value) || isNumber(value)) {
        dom.setAttribute(key, value + '');
      } else {
        console.error('Invalid call to Attribute.set. Key ', key, ':: Value ', value, ':: Element ', dom);
        throw new Error('Attribute value was not simple');
      }
    };
    var set$8 = function (element, key, value) {
      rawSet(element.dom, key, value);
    };
    var setAll$1 = function (element, attrs) {
      var dom = element.dom;
      each(attrs, function (v, k) {
        rawSet(dom, k, v);
      });
    };
    var get$b = function (element, key) {
      var v = element.dom.getAttribute(key);
      return v === null ? undefined : v;
    };
    var getOpt = function (element, key) {
      return Optional.from(get$b(element, key));
    };
    var has$1 = function (element, key) {
      var dom = element.dom;
      return dom && dom.hasAttribute ? dom.hasAttribute(key) : false;
    };
    var remove$6 = function (element, key) {
      element.dom.removeAttribute(key);
    };

    var read$2 = function (element, attr) {
      var value = get$b(element, attr);
      return value === undefined || value === '' ? [] : value.split(' ');
    };
    var add$3 = function (element, attr, id) {
      var old = read$2(element, attr);
      var nu = old.concat([id]);
      set$8(element, attr, nu.join(' '));
      return true;
    };
    var remove$5 = function (element, attr, id) {
      var nu = filter$2(read$2(element, attr), function (v) {
        return v !== id;
      });
      if (nu.length > 0) {
        set$8(element, attr, nu.join(' '));
      } else {
        remove$6(element, attr);
      }
      return false;
    };

    var supports = function (element) {
      return element.dom.classList !== undefined;
    };
    var get$a = function (element) {
      return read$2(element, 'class');
    };
    var add$2 = function (element, clazz) {
      return add$3(element, 'class', clazz);
    };
    var remove$4 = function (element, clazz) {
      return remove$5(element, 'class', clazz);
    };

    var add$1 = function (element, clazz) {
      if (supports(element)) {
        element.dom.classList.add(clazz);
      } else {
        add$2(element, clazz);
      }
    };
    var cleanClass = function (element) {
      var classList = supports(element) ? element.dom.classList : get$a(element);
      if (classList.length === 0) {
        remove$6(element, 'class');
      }
    };
    var remove$3 = function (element, clazz) {
      if (supports(element)) {
        var classList = element.dom.classList;
        classList.remove(clazz);
      } else {
        remove$4(element, clazz);
      }
      cleanClass(element);
    };
    var has = function (element, clazz) {
      return supports(element) && element.dom.classList.contains(clazz);
    };

    var swap = function (element, addCls, removeCls) {
      remove$3(element, removeCls);
      add$1(element, addCls);
    };
    var toAlpha = function (component, swapConfig, _swapState) {
      swap(component.element, swapConfig.alpha, swapConfig.omega);
    };
    var toOmega = function (component, swapConfig, _swapState) {
      swap(component.element, swapConfig.omega, swapConfig.alpha);
    };
    var clear$1 = function (component, swapConfig, _swapState) {
      remove$3(component.element, swapConfig.alpha);
      remove$3(component.element, swapConfig.omega);
    };
    var isAlpha = function (component, swapConfig, _swapState) {
      return has(component.element, swapConfig.alpha);
    };
    var isOmega = function (component, swapConfig, _swapState) {
      return has(component.element, swapConfig.omega);
    };

    var SwapApis = /*#__PURE__*/Object.freeze({
        __proto__: null,
        toAlpha: toAlpha,
        toOmega: toOmega,
        isAlpha: isAlpha,
        isOmega: isOmega,
        clear: clear$1
    });

    var value$2 = function (o) {
      var or = function (_opt) {
        return value$2(o);
      };
      var orThunk = function (_f) {
        return value$2(o);
      };
      var map = function (f) {
        return value$2(f(o));
      };
      var mapError = function (_f) {
        return value$2(o);
      };
      var each = function (f) {
        f(o);
      };
      var bind = function (f) {
        return f(o);
      };
      var fold = function (_, onValue) {
        return onValue(o);
      };
      var exists = function (f) {
        return f(o);
      };
      var forall = function (f) {
        return f(o);
      };
      var toOptional = function () {
        return Optional.some(o);
      };
      return {
        isValue: always,
        isError: never,
        getOr: constant$1(o),
        getOrThunk: constant$1(o),
        getOrDie: constant$1(o),
        or: or,
        orThunk: orThunk,
        fold: fold,
        map: map,
        mapError: mapError,
        each: each,
        bind: bind,
        exists: exists,
        forall: forall,
        toOptional: toOptional
      };
    };
    var error = function (message) {
      var getOrThunk = function (f) {
        return f();
      };
      var getOrDie = function () {
        return die(String(message))();
      };
      var or = identity;
      var orThunk = function (f) {
        return f();
      };
      var map = function (_f) {
        return error(message);
      };
      var mapError = function (f) {
        return error(f(message));
      };
      var bind = function (_f) {
        return error(message);
      };
      var fold = function (onError, _) {
        return onError(message);
      };
      return {
        isValue: never,
        isError: always,
        getOr: identity,
        getOrThunk: getOrThunk,
        getOrDie: getOrDie,
        or: or,
        orThunk: orThunk,
        fold: fold,
        map: map,
        mapError: mapError,
        each: noop,
        bind: bind,
        exists: never,
        forall: always,
        toOptional: Optional.none
      };
    };
    var fromOption = function (opt, err) {
      return opt.fold(function () {
        return error(err);
      }, value$2);
    };
    var Result = {
      value: value$2,
      error: error,
      fromOption: fromOption
    };

    var SimpleResultType;
    (function (SimpleResultType) {
      SimpleResultType[SimpleResultType['Error'] = 0] = 'Error';
      SimpleResultType[SimpleResultType['Value'] = 1] = 'Value';
    }(SimpleResultType || (SimpleResultType = {})));
    var fold$1 = function (res, onError, onValue) {
      return res.stype === SimpleResultType.Error ? onError(res.serror) : onValue(res.svalue);
    };
    var partition$1 = function (results) {
      var values = [];
      var errors = [];
      each$1(results, function (obj) {
        fold$1(obj, function (err) {
          return errors.push(err);
        }, function (val) {
          return values.push(val);
        });
      });
      return {
        values: values,
        errors: errors
      };
    };
    var mapError = function (res, f) {
      if (res.stype === SimpleResultType.Error) {
        return {
          stype: SimpleResultType.Error,
          serror: f(res.serror)
        };
      } else {
        return res;
      }
    };
    var map = function (res, f) {
      if (res.stype === SimpleResultType.Value) {
        return {
          stype: SimpleResultType.Value,
          svalue: f(res.svalue)
        };
      } else {
        return res;
      }
    };
    var bind$2 = function (res, f) {
      if (res.stype === SimpleResultType.Value) {
        return f(res.svalue);
      } else {
        return res;
      }
    };
    var bindError = function (res, f) {
      if (res.stype === SimpleResultType.Error) {
        return f(res.serror);
      } else {
        return res;
      }
    };
    var svalue = function (v) {
      return {
        stype: SimpleResultType.Value,
        svalue: v
      };
    };
    var serror = function (e) {
      return {
        stype: SimpleResultType.Error,
        serror: e
      };
    };
    var toResult$1 = function (res) {
      return fold$1(res, Result.error, Result.value);
    };
    var fromResult = function (res) {
      return res.fold(serror, svalue);
    };
    var SimpleResult = {
      fromResult: fromResult,
      toResult: toResult$1,
      svalue: svalue,
      partition: partition$1,
      serror: serror,
      bind: bind$2,
      bindError: bindError,
      map: map,
      mapError: mapError,
      fold: fold$1
    };

    var field$3 = function (key, newKey, presence, prop) {
      return {
        tag: 'field',
        key: key,
        newKey: newKey,
        presence: presence,
        prop: prop
      };
    };
    var customField$1 = function (newKey, instantiator) {
      return {
        tag: 'custom',
        newKey: newKey,
        instantiator: instantiator
      };
    };
    var fold = function (value, ifField, ifCustom) {
      switch (value.tag) {
      case 'field':
        return ifField(value.key, value.newKey, value.presence, value.prop);
      case 'custom':
        return ifCustom(value.newKey, value.instantiator);
      }
    };

    var shallow$1 = function (old, nu) {
      return nu;
    };
    var deep = function (old, nu) {
      var bothObjects = isObject(old) && isObject(nu);
      return bothObjects ? deepMerge(old, nu) : nu;
    };
    var baseMerge = function (merger) {
      return function () {
        var objects = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          objects[_i] = arguments[_i];
        }
        if (objects.length === 0) {
          throw new Error('Can\'t merge zero objects');
        }
        var ret = {};
        for (var j = 0; j < objects.length; j++) {
          var curObject = objects[j];
          for (var key in curObject) {
            if (has$2(curObject, key)) {
              ret[key] = merger(ret[key], curObject[key]);
            }
          }
        }
        return ret;
      };
    };
    var deepMerge = baseMerge(deep);
    var merge$1 = baseMerge(shallow$1);

    var required$2 = function () {
      return {
        tag: 'required',
        process: {}
      };
    };
    var defaultedThunk = function (fallbackThunk) {
      return {
        tag: 'defaultedThunk',
        process: fallbackThunk
      };
    };
    var defaulted$1 = function (fallback) {
      return defaultedThunk(constant$1(fallback));
    };
    var asOption = function () {
      return {
        tag: 'option',
        process: {}
      };
    };
    var mergeWithThunk = function (baseThunk) {
      return {
        tag: 'mergeWithThunk',
        process: baseThunk
      };
    };
    var mergeWith = function (base) {
      return mergeWithThunk(constant$1(base));
    };

    var mergeValues$1 = function (values, base) {
      return values.length > 0 ? SimpleResult.svalue(deepMerge(base, merge$1.apply(undefined, values))) : SimpleResult.svalue(base);
    };
    var mergeErrors$1 = function (errors) {
      return compose(SimpleResult.serror, flatten)(errors);
    };
    var consolidateObj = function (objects, base) {
      var partition = SimpleResult.partition(objects);
      return partition.errors.length > 0 ? mergeErrors$1(partition.errors) : mergeValues$1(partition.values, base);
    };
    var consolidateArr = function (objects) {
      var partitions = SimpleResult.partition(objects);
      return partitions.errors.length > 0 ? mergeErrors$1(partitions.errors) : SimpleResult.svalue(partitions.values);
    };
    var ResultCombine = {
      consolidateObj: consolidateObj,
      consolidateArr: consolidateArr
    };

    var formatObj = function (input) {
      return isObject(input) && keys(input).length > 100 ? ' removed due to size' : JSON.stringify(input, null, 2);
    };
    var formatErrors = function (errors) {
      var es = errors.length > 10 ? errors.slice(0, 10).concat([{
          path: [],
          getErrorInfo: constant$1('... (only showing first ten failures)')
        }]) : errors;
      return map$2(es, function (e) {
        return 'Failed path: (' + e.path.join(' > ') + ')\n' + e.getErrorInfo();
      });
    };

    var nu$5 = function (path, getErrorInfo) {
      return SimpleResult.serror([{
          path: path,
          getErrorInfo: getErrorInfo
        }]);
    };
    var missingRequired = function (path, key, obj) {
      return nu$5(path, function () {
        return 'Could not find valid *required* value for "' + key + '" in ' + formatObj(obj);
      });
    };
    var missingKey = function (path, key) {
      return nu$5(path, function () {
        return 'Choice schema did not contain choice key: "' + key + '"';
      });
    };
    var missingBranch = function (path, branches, branch) {
      return nu$5(path, function () {
        return 'The chosen schema: "' + branch + '" did not exist in branches: ' + formatObj(branches);
      });
    };
    var unsupportedFields = function (path, unsupported) {
      return nu$5(path, function () {
        return 'There are unsupported fields: [' + unsupported.join(', ') + '] specified';
      });
    };
    var custom = function (path, err) {
      return nu$5(path, constant$1(err));
    };

    var value$1 = function (validator) {
      var extract = function (path, val) {
        return SimpleResult.bindError(validator(val), function (err) {
          return custom(path, err);
        });
      };
      var toString = constant$1('val');
      return {
        extract: extract,
        toString: toString
      };
    };
    var anyValue$1 = value$1(SimpleResult.svalue);

    var requiredAccess = function (path, obj, key, bundle) {
      return get$c(obj, key).fold(function () {
        return missingRequired(path, key, obj);
      }, bundle);
    };
    var fallbackAccess = function (obj, key, fallback, bundle) {
      var v = get$c(obj, key).getOrThunk(function () {
        return fallback(obj);
      });
      return bundle(v);
    };
    var optionAccess = function (obj, key, bundle) {
      return bundle(get$c(obj, key));
    };
    var optionDefaultedAccess = function (obj, key, fallback, bundle) {
      var opt = get$c(obj, key).map(function (val) {
        return val === true ? fallback(obj) : val;
      });
      return bundle(opt);
    };
    var extractField = function (field, path, obj, key, prop) {
      var bundle = function (av) {
        return prop.extract(path.concat([key]), av);
      };
      var bundleAsOption = function (optValue) {
        return optValue.fold(function () {
          return SimpleResult.svalue(Optional.none());
        }, function (ov) {
          var result = prop.extract(path.concat([key]), ov);
          return SimpleResult.map(result, Optional.some);
        });
      };
      switch (field.tag) {
      case 'required':
        return requiredAccess(path, obj, key, bundle);
      case 'defaultedThunk':
        return fallbackAccess(obj, key, field.process, bundle);
      case 'option':
        return optionAccess(obj, key, bundleAsOption);
      case 'defaultedOptionThunk':
        return optionDefaultedAccess(obj, key, field.process, bundleAsOption);
      case 'mergeWithThunk': {
          return fallbackAccess(obj, key, constant$1({}), function (v) {
            var result = deepMerge(field.process(obj), v);
            return bundle(result);
          });
        }
      }
    };
    var extractFields = function (path, obj, fields) {
      var success = {};
      var errors = [];
      for (var _i = 0, fields_1 = fields; _i < fields_1.length; _i++) {
        var field = fields_1[_i];
        fold(field, function (key, newKey, presence, prop) {
          var result = extractField(presence, path, obj, key, prop);
          SimpleResult.fold(result, function (err) {
            errors.push.apply(errors, err);
          }, function (res) {
            success[newKey] = res;
          });
        }, function (newKey, instantiator) {
          success[newKey] = instantiator(obj);
        });
      }
      return errors.length > 0 ? SimpleResult.serror(errors) : SimpleResult.svalue(success);
    };
    var getSetKeys = function (obj) {
      return keys(filter$1(obj, isNonNullable));
    };
    var objOfOnly = function (fields) {
      var delegate = objOf(fields);
      var fieldNames = foldr(fields, function (acc, value) {
        return fold(value, function (key) {
          var _a;
          return deepMerge(acc, (_a = {}, _a[key] = true, _a));
        }, constant$1(acc));
      }, {});
      var extract = function (path, o) {
        var keys = isBoolean(o) ? [] : getSetKeys(o);
        var extra = filter$2(keys, function (k) {
          return !hasNonNullableKey(fieldNames, k);
        });
        return extra.length === 0 ? delegate.extract(path, o) : unsupportedFields(path, extra);
      };
      return {
        extract: extract,
        toString: delegate.toString
      };
    };
    var objOf = function (values) {
      var extract = function (path, o) {
        return extractFields(path, o, values);
      };
      var toString = function () {
        var fieldStrings = map$2(values, function (value) {
          return fold(value, function (key, _okey, _presence, prop) {
            return key + ' -> ' + prop.toString();
          }, function (newKey, _instantiator) {
            return 'state(' + newKey + ')';
          });
        });
        return 'obj{\n' + fieldStrings.join('\n') + '}';
      };
      return {
        extract: extract,
        toString: toString
      };
    };
    var arrOf = function (prop) {
      var extract = function (path, array) {
        var results = map$2(array, function (a, i) {
          return prop.extract(path.concat(['[' + i + ']']), a);
        });
        return ResultCombine.consolidateArr(results);
      };
      var toString = function () {
        return 'array(' + prop.toString() + ')';
      };
      return {
        extract: extract,
        toString: toString
      };
    };
    var setOf$1 = function (validator, prop) {
      var validateKeys = function (path, keys) {
        return arrOf(value$1(validator)).extract(path, keys);
      };
      var extract = function (path, o) {
        var keys$1 = keys(o);
        var validatedKeys = validateKeys(path, keys$1);
        return SimpleResult.bind(validatedKeys, function (validKeys) {
          var schema = map$2(validKeys, function (vk) {
            return field$3(vk, vk, required$2(), prop);
          });
          return objOf(schema).extract(path, o);
        });
      };
      var toString = function () {
        return 'setOf(' + prop.toString() + ')';
      };
      return {
        extract: extract,
        toString: toString
      };
    };

    var anyValue = constant$1(anyValue$1);
    var typedValue = function (validator, expectedType) {
      return value$1(function (a) {
        var actualType = typeof a;
        return validator(a) ? SimpleResult.svalue(a) : SimpleResult.serror('Expected type: ' + expectedType + ' but got: ' + actualType);
      });
    };
    var functionProcessor = typedValue(isFunction, 'function');

    var chooseFrom = function (path, input, branches, ch) {
      var fields = get$c(branches, ch);
      return fields.fold(function () {
        return missingBranch(path, branches, ch);
      }, function (vp) {
        return vp.extract(path.concat(['branch: ' + ch]), input);
      });
    };
    var choose$2 = function (key, branches) {
      var extract = function (path, input) {
        var choice = get$c(input, key);
        return choice.fold(function () {
          return missingKey(path, key);
        }, function (chosen) {
          return chooseFrom(path, input, branches, chosen);
        });
      };
      var toString = function () {
        return 'chooseOn(' + key + '). Possible values: ' + keys(branches);
      };
      return {
        extract: extract,
        toString: toString
      };
    };

    var valueOf = function (validator) {
      return value$1(function (v) {
        return validator(v).fold(SimpleResult.serror, SimpleResult.svalue);
      });
    };
    var setOf = function (validator, prop) {
      return setOf$1(function (v) {
        return SimpleResult.fromResult(validator(v));
      }, prop);
    };
    var extractValue = function (label, prop, obj) {
      var res = prop.extract([label], obj);
      return SimpleResult.mapError(res, function (errs) {
        return {
          input: obj,
          errors: errs
        };
      });
    };
    var asRaw = function (label, prop, obj) {
      return SimpleResult.toResult(extractValue(label, prop, obj));
    };
    var getOrDie = function (extraction) {
      return extraction.fold(function (errInfo) {
        throw new Error(formatError(errInfo));
      }, identity);
    };
    var asRawOrDie$1 = function (label, prop, obj) {
      return getOrDie(asRaw(label, prop, obj));
    };
    var formatError = function (errInfo) {
      return 'Errors: \n' + formatErrors(errInfo.errors).join('\n') + '\n\nInput object: ' + formatObj(errInfo.input);
    };
    var choose$1 = function (key, branches) {
      return choose$2(key, map$1(branches, objOf));
    };

    var field$2 = field$3;
    var customField = customField$1;
    var required$1 = function (key) {
      return field$2(key, key, required$2(), anyValue());
    };
    var requiredOf = function (key, schema) {
      return field$2(key, key, required$2(), schema);
    };
    var forbid = function (key, message) {
      return field$2(key, key, asOption(), value$1(function (_v) {
        return SimpleResult.serror('The field: ' + key + ' is forbidden. ' + message);
      }));
    };
    var requiredObjOf = function (key, objSchema) {
      return field$2(key, key, required$2(), objOf(objSchema));
    };
    var option = function (key) {
      return field$2(key, key, asOption(), anyValue());
    };
    var optionOf = function (key, schema) {
      return field$2(key, key, asOption(), schema);
    };
    var optionObjOf = function (key, objSchema) {
      return optionOf(key, objOf(objSchema));
    };
    var optionObjOfOnly = function (key, objSchema) {
      return optionOf(key, objOfOnly(objSchema));
    };
    var defaulted = function (key, fallback) {
      return field$2(key, key, defaulted$1(fallback), anyValue());
    };
    var defaultedOf = function (key, fallback, schema) {
      return field$2(key, key, defaulted$1(fallback), schema);
    };
    var defaultedFunction = function (key, fallback) {
      return defaultedOf(key, fallback, functionProcessor);
    };
    var defaultedObjOf = function (key, fallback, objSchema) {
      return defaultedOf(key, fallback, objOf(objSchema));
    };

    var SwapSchema = [
      required$1('alpha'),
      required$1('omega')
    ];

    var generate$5 = function (cases) {
      if (!isArray(cases)) {
        throw new Error('cases must be an array');
      }
      if (cases.length === 0) {
        throw new Error('there must be at least one case');
      }
      var constructors = [];
      var adt = {};
      each$1(cases, function (acase, count) {
        var keys$1 = keys(acase);
        if (keys$1.length !== 1) {
          throw new Error('one and only one name per case');
        }
        var key = keys$1[0];
        var value = acase[key];
        if (adt[key] !== undefined) {
          throw new Error('duplicate key detected:' + key);
        } else if (key === 'cata') {
          throw new Error('cannot have a case named cata (sorry)');
        } else if (!isArray(value)) {
          throw new Error('case arguments must be an array');
        }
        constructors.push(key);
        adt[key] = function () {
          var args = [];
          for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
          }
          var argLength = args.length;
          if (argLength !== value.length) {
            throw new Error('Wrong number of arguments to case ' + key + '. Expected ' + value.length + ' (' + value + '), got ' + argLength);
          }
          var match = function (branches) {
            var branchKeys = keys(branches);
            if (constructors.length !== branchKeys.length) {
              throw new Error('Wrong number of arguments to match. Expected: ' + constructors.join(',') + '\nActual: ' + branchKeys.join(','));
            }
            var allReqd = forall(constructors, function (reqKey) {
              return contains$1(branchKeys, reqKey);
            });
            if (!allReqd) {
              throw new Error('Not all branches were specified when using match. Specified: ' + branchKeys.join(', ') + '\nRequired: ' + constructors.join(', '));
            }
            return branches[key].apply(null, args);
          };
          return {
            fold: function () {
              var foldArgs = [];
              for (var _i = 0; _i < arguments.length; _i++) {
                foldArgs[_i] = arguments[_i];
              }
              if (foldArgs.length !== cases.length) {
                throw new Error('Wrong number of arguments to fold. Expected ' + cases.length + ', got ' + foldArgs.length);
              }
              var target = foldArgs[count];
              return target.apply(null, args);
            },
            match: match,
            log: function (label) {
              console.log(label, {
                constructors: constructors,
                constructor: key,
                params: args
              });
            }
          };
        };
      });
      return adt;
    };
    var Adt = { generate: generate$5 };

    Adt.generate([
      {
        bothErrors: [
          'error1',
          'error2'
        ]
      },
      {
        firstError: [
          'error1',
          'value2'
        ]
      },
      {
        secondError: [
          'value1',
          'error2'
        ]
      },
      {
        bothValues: [
          'value1',
          'value2'
        ]
      }
    ]);
    var partition = function (results) {
      var errors = [];
      var values = [];
      each$1(results, function (result) {
        result.fold(function (err) {
          errors.push(err);
        }, function (value) {
          values.push(value);
        });
      });
      return {
        errors: errors,
        values: values
      };
    };

    var exclude$1 = function (obj, fields) {
      var r = {};
      each(obj, function (v, k) {
        if (!contains$1(fields, k)) {
          r[k] = v;
        }
      });
      return r;
    };

    var wrap$1 = function (key, value) {
      var _a;
      return _a = {}, _a[key] = value, _a;
    };
    var wrapAll$1 = function (keyvalues) {
      var r = {};
      each$1(keyvalues, function (kv) {
        r[kv.key] = kv.value;
      });
      return r;
    };

    var exclude = function (obj, fields) {
      return exclude$1(obj, fields);
    };
    var wrap = function (key, value) {
      return wrap$1(key, value);
    };
    var wrapAll = function (keyvalues) {
      return wrapAll$1(keyvalues);
    };
    var mergeValues = function (values, base) {
      return values.length === 0 ? Result.value(base) : Result.value(deepMerge(base, merge$1.apply(undefined, values)));
    };
    var mergeErrors = function (errors) {
      return Result.error(flatten(errors));
    };
    var consolidate = function (objs, base) {
      var partitions = partition(objs);
      return partitions.errors.length > 0 ? mergeErrors(partitions.errors) : mergeValues(partitions.values, base);
    };

    var is = function (lhs, rhs, comparator) {
      if (comparator === void 0) {
        comparator = tripleEquals;
      }
      return lhs.exists(function (left) {
        return comparator(left, rhs);
      });
    };
    var cat = function (arr) {
      var r = [];
      var push = function (x) {
        r.push(x);
      };
      for (var i = 0; i < arr.length; i++) {
        arr[i].each(push);
      }
      return r;
    };
    var sequence = function (arr) {
      var r = [];
      for (var i = 0; i < arr.length; i++) {
        var x = arr[i];
        if (x.isSome()) {
          r.push(x.getOrDie());
        } else {
          return Optional.none();
        }
      }
      return Optional.some(r);
    };
    var lift2 = function (oa, ob, f) {
      return oa.isSome() && ob.isSome() ? Optional.some(f(oa.getOrDie(), ob.getOrDie())) : Optional.none();
    };
    var someIf = function (b, a) {
      return b ? Optional.some(a) : Optional.none();
    };

    var ensureIsRoot = function (isRoot) {
      return isFunction(isRoot) ? isRoot : never;
    };
    var ancestor$2 = function (scope, transform, isRoot) {
      var element = scope.dom;
      var stop = ensureIsRoot(isRoot);
      while (element.parentNode) {
        element = element.parentNode;
        var el = SugarElement.fromDom(element);
        var transformed = transform(el);
        if (transformed.isSome()) {
          return transformed;
        } else if (stop(el)) {
          break;
        }
      }
      return Optional.none();
    };
    var closest$3 = function (scope, transform, isRoot) {
      var current = transform(scope);
      var stop = ensureIsRoot(isRoot);
      return current.orThunk(function () {
        return stop(scope) ? Optional.none() : ancestor$2(scope, transform, stop);
      });
    };

    var isSource = function (component, simulatedEvent) {
      return eq(component.element, simulatedEvent.event.target);
    };

    var defaultEventHandler = {
      can: always,
      abort: never,
      run: noop
    };
    var nu$4 = function (parts) {
      if (!hasNonNullableKey(parts, 'can') && !hasNonNullableKey(parts, 'abort') && !hasNonNullableKey(parts, 'run')) {
        throw new Error('EventHandler defined by: ' + JSON.stringify(parts, null, 2) + ' does not have can, abort, or run!');
      }
      return __assign(__assign({}, defaultEventHandler), parts);
    };
    var all$1 = function (handlers, f) {
      return function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        return foldl(handlers, function (acc, handler) {
          return acc && f(handler).apply(undefined, args);
        }, true);
      };
    };
    var any = function (handlers, f) {
      return function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        return foldl(handlers, function (acc, handler) {
          return acc || f(handler).apply(undefined, args);
        }, false);
      };
    };
    var read$1 = function (handler) {
      return isFunction(handler) ? {
        can: always,
        abort: never,
        run: handler
      } : handler;
    };
    var fuse$1 = function (handlers) {
      var can = all$1(handlers, function (handler) {
        return handler.can;
      });
      var abort = any(handlers, function (handler) {
        return handler.abort;
      });
      var run = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        each$1(handlers, function (handler) {
          handler.run.apply(undefined, args);
        });
      };
      return {
        can: can,
        abort: abort,
        run: run
      };
    };

    var derive$3 = function (configs) {
      return wrapAll(configs);
    };
    var abort = function (name, predicate) {
      return {
        key: name,
        value: nu$4({ abort: predicate })
      };
    };
    var can = function (name, predicate) {
      return {
        key: name,
        value: nu$4({ can: predicate })
      };
    };
    var run = function (name, handler) {
      return {
        key: name,
        value: nu$4({ run: handler })
      };
    };
    var runActionExtra = function (name, action, extra) {
      return {
        key: name,
        value: nu$4({
          run: function (component, simulatedEvent) {
            action.apply(undefined, [
              component,
              simulatedEvent
            ].concat(extra));
          }
        })
      };
    };
    var runOnName = function (name) {
      return function (handler) {
        return run(name, handler);
      };
    };
    var runOnSourceName = function (name) {
      return function (handler) {
        return {
          key: name,
          value: nu$4({
            run: function (component, simulatedEvent) {
              if (isSource(component, simulatedEvent)) {
                handler(component, simulatedEvent);
              }
            }
          })
        };
      };
    };
    var redirectToUid = function (name, uid) {
      return run(name, function (component, simulatedEvent) {
        component.getSystem().getByUid(uid).each(function (redirectee) {
          dispatchEvent(redirectee, redirectee.element, name, simulatedEvent);
        });
      });
    };
    var redirectToPart = function (name, detail, partName) {
      var uid = detail.partUids[partName];
      return redirectToUid(name, uid);
    };
    var cutter = function (name) {
      return run(name, function (component, simulatedEvent) {
        simulatedEvent.cut();
      });
    };
    var stopper = function (name) {
      return run(name, function (component, simulatedEvent) {
        simulatedEvent.stop();
      });
    };
    var runOnSource = function (name, f) {
      return runOnSourceName(name)(f);
    };
    var runOnAttached = runOnSourceName(attachedToDom());
    var runOnDetached = runOnSourceName(detachedFromDom());
    var runOnInit = runOnSourceName(systemInit());
    var runOnExecute = runOnName(execute$5());

    var markAsBehaviourApi = function (f, apiName, apiFunction) {
      var delegate = apiFunction.toString();
      var endIndex = delegate.indexOf(')') + 1;
      var openBracketIndex = delegate.indexOf('(');
      var parameters = delegate.substring(openBracketIndex + 1, endIndex - 1).split(/,\s*/);
      f.toFunctionAnnotation = function () {
        return {
          name: apiName,
          parameters: cleanParameters(parameters.slice(0, 1).concat(parameters.slice(3)))
        };
      };
      return f;
    };
    var cleanParameters = function (parameters) {
      return map$2(parameters, function (p) {
        return endsWith(p, '/*') ? p.substring(0, p.length - '/*'.length) : p;
      });
    };
    var markAsExtraApi = function (f, extraName) {
      var delegate = f.toString();
      var endIndex = delegate.indexOf(')') + 1;
      var openBracketIndex = delegate.indexOf('(');
      var parameters = delegate.substring(openBracketIndex + 1, endIndex - 1).split(/,\s*/);
      f.toFunctionAnnotation = function () {
        return {
          name: extraName,
          parameters: cleanParameters(parameters)
        };
      };
      return f;
    };
    var markAsSketchApi = function (f, apiFunction) {
      var delegate = apiFunction.toString();
      var endIndex = delegate.indexOf(')') + 1;
      var openBracketIndex = delegate.indexOf('(');
      var parameters = delegate.substring(openBracketIndex + 1, endIndex - 1).split(/,\s*/);
      f.toFunctionAnnotation = function () {
        return {
          name: 'OVERRIDE',
          parameters: cleanParameters(parameters.slice(1))
        };
      };
      return f;
    };

    var nu$3 = function (s) {
      return {
        classes: isUndefined(s.classes) ? [] : s.classes,
        attributes: isUndefined(s.attributes) ? {} : s.attributes,
        styles: isUndefined(s.styles) ? {} : s.styles
      };
    };
    var merge = function (defnA, mod) {
      return __assign(__assign({}, defnA), {
        attributes: __assign(__assign({}, defnA.attributes), mod.attributes),
        styles: __assign(__assign({}, defnA.styles), mod.styles),
        classes: defnA.classes.concat(mod.classes)
      });
    };

    var executeEvent = function (bConfig, bState, executor) {
      return runOnExecute(function (component) {
        executor(component, bConfig, bState);
      });
    };
    var loadEvent = function (bConfig, bState, f) {
      return runOnInit(function (component, _simulatedEvent) {
        f(component, bConfig, bState);
      });
    };
    var create$6 = function (schema, name, active, apis, extra, state) {
      var configSchema = objOfOnly(schema);
      var schemaSchema = optionObjOf(name, [optionObjOfOnly('config', schema)]);
      return doCreate(configSchema, schemaSchema, name, active, apis, extra, state);
    };
    var createModes$1 = function (modes, name, active, apis, extra, state) {
      var configSchema = modes;
      var schemaSchema = optionObjOf(name, [optionOf('config', modes)]);
      return doCreate(configSchema, schemaSchema, name, active, apis, extra, state);
    };
    var wrapApi = function (bName, apiFunction, apiName) {
      var f = function (component) {
        var rest = [];
        for (var _i = 1; _i < arguments.length; _i++) {
          rest[_i - 1] = arguments[_i];
        }
        var args = [component].concat(rest);
        return component.config({ name: constant$1(bName) }).fold(function () {
          throw new Error('We could not find any behaviour configuration for: ' + bName + '. Using API: ' + apiName);
        }, function (info) {
          var rest = Array.prototype.slice.call(args, 1);
          return apiFunction.apply(undefined, [
            component,
            info.config,
            info.state
          ].concat(rest));
        });
      };
      return markAsBehaviourApi(f, apiName, apiFunction);
    };
    var revokeBehaviour = function (name) {
      return {
        key: name,
        value: undefined
      };
    };
    var doCreate = function (configSchema, schemaSchema, name, active, apis, extra, state) {
      var getConfig = function (info) {
        return hasNonNullableKey(info, name) ? info[name]() : Optional.none();
      };
      var wrappedApis = map$1(apis, function (apiF, apiName) {
        return wrapApi(name, apiF, apiName);
      });
      var wrappedExtra = map$1(extra, function (extraF, extraName) {
        return markAsExtraApi(extraF, extraName);
      });
      var me = __assign(__assign(__assign({}, wrappedExtra), wrappedApis), {
        revoke: curry(revokeBehaviour, name),
        config: function (spec) {
          var prepared = asRawOrDie$1(name + '-config', configSchema, spec);
          return {
            key: name,
            value: {
              config: prepared,
              me: me,
              configAsRaw: cached(function () {
                return asRawOrDie$1(name + '-config', configSchema, spec);
              }),
              initialConfig: spec,
              state: state
            }
          };
        },
        schema: constant$1(schemaSchema),
        exhibit: function (info, base) {
          return lift2(getConfig(info), get$c(active, 'exhibit'), function (behaviourInfo, exhibitor) {
            return exhibitor(base, behaviourInfo.config, behaviourInfo.state);
          }).getOrThunk(function () {
            return nu$3({});
          });
        },
        name: constant$1(name),
        handlers: function (info) {
          return getConfig(info).map(function (behaviourInfo) {
            var getEvents = get$c(active, 'events').getOr(function () {
              return {};
            });
            return getEvents(behaviourInfo.config, behaviourInfo.state);
          }).getOr({});
        }
      });
      return me;
    };

    var NoState = {
      init: function () {
        return nu$2({ readState: constant$1('No State required') });
      }
    };
    var nu$2 = function (spec) {
      return spec;
    };

    var derive$2 = function (capabilities) {
      return wrapAll(capabilities);
    };
    var simpleSchema = objOfOnly([
      required$1('fields'),
      required$1('name'),
      defaulted('active', {}),
      defaulted('apis', {}),
      defaulted('state', NoState),
      defaulted('extra', {})
    ]);
    var create$5 = function (data) {
      var value = asRawOrDie$1('Creating behaviour: ' + data.name, simpleSchema, data);
      return create$6(value.fields, value.name, value.active, value.apis, value.extra, value.state);
    };
    var modeSchema = objOfOnly([
      required$1('branchKey'),
      required$1('branches'),
      required$1('name'),
      defaulted('active', {}),
      defaulted('apis', {}),
      defaulted('state', NoState),
      defaulted('extra', {})
    ]);
    var createModes = function (data) {
      var value = asRawOrDie$1('Creating behaviour: ' + data.name, modeSchema, data);
      return createModes$1(choose$1(value.branchKey, value.branches), value.name, value.active, value.apis, value.extra, value.state);
    };
    var revoke = constant$1(undefined);

    var Swapping = create$5({
      fields: SwapSchema,
      name: 'swapping',
      apis: SwapApis
    });

    var Cell = function (initial) {
      var value = initial;
      var get = function () {
        return value;
      };
      var set = function (v) {
        value = v;
      };
      return {
        get: get,
        set: set
      };
    };

    var getDocument = function () {
      return SugarElement.fromDom(document);
    };

    var focus$3 = function (element) {
      return element.dom.focus();
    };
    var blur$1 = function (element) {
      return element.dom.blur();
    };
    var hasFocus = function (element) {
      var root = getRootNode(element).dom;
      return element.dom === root.activeElement;
    };
    var active = function (root) {
      if (root === void 0) {
        root = getDocument();
      }
      return Optional.from(root.dom.activeElement).map(SugarElement.fromDom);
    };
    var search = function (element) {
      return active(getRootNode(element)).filter(function (e) {
        return element.dom.contains(e.dom);
      });
    };

    var global$5 = tinymce.util.Tools.resolve('tinymce.dom.DOMUtils');

    var global$4 = tinymce.util.Tools.resolve('tinymce.ThemeManager');

    var openLink = function (target) {
      var link = document.createElement('a');
      link.target = '_blank';
      link.href = target.href;
      link.rel = 'noreferrer noopener';
      var nuEvt = document.createEvent('MouseEvents');
      nuEvt.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
      document.body.appendChild(link);
      link.dispatchEvent(nuEvt);
      document.body.removeChild(link);
    };

    var DefaultStyleFormats = [
      {
        title: 'Headings',
        items: [
          {
            title: 'Heading 1',
            format: 'h1'
          },
          {
            title: 'Heading 2',
            format: 'h2'
          },
          {
            title: 'Heading 3',
            format: 'h3'
          },
          {
            title: 'Heading 4',
            format: 'h4'
          },
          {
            title: 'Heading 5',
            format: 'h5'
          },
          {
            title: 'Heading 6',
            format: 'h6'
          }
        ]
      },
      {
        title: 'Inline',
        items: [
          {
            title: 'Bold',
            icon: 'bold',
            format: 'bold'
          },
          {
            title: 'Italic',
            icon: 'italic',
            format: 'italic'
          },
          {
            title: 'Underline',
            icon: 'underline',
            format: 'underline'
          },
          {
            title: 'Strikethrough',
            icon: 'strikethrough',
            format: 'strikethrough'
          },
          {
            title: 'Superscript',
            icon: 'superscript',
            format: 'superscript'
          },
          {
            title: 'Subscript',
            icon: 'subscript',
            format: 'subscript'
          },
          {
            title: 'Code',
            icon: 'code',
            format: 'code'
          }
        ]
      },
      {
        title: 'Blocks',
        items: [
          {
            title: 'Paragraph',
            format: 'p'
          },
          {
            title: 'Blockquote',
            format: 'blockquote'
          },
          {
            title: 'Div',
            format: 'div'
          },
          {
            title: 'Pre',
            format: 'pre'
          }
        ]
      },
      {
        title: 'Alignment',
        items: [
          {
            title: 'Left',
            icon: 'alignleft',
            format: 'alignleft'
          },
          {
            title: 'Center',
            icon: 'aligncenter',
            format: 'aligncenter'
          },
          {
            title: 'Right',
            icon: 'alignright',
            format: 'alignright'
          },
          {
            title: 'Justify',
            icon: 'alignjustify',
            format: 'alignjustify'
          }
        ]
      }
    ];

    var defaults = [
      'undo',
      'bold',
      'italic',
      'link',
      'image',
      'bullist',
      'styleselect'
    ];
    var isSkinDisabled = function (editor) {
      return editor.getParam('skin') === false;
    };
    var readOnlyOnInit = function (_editor) {
      return false;
    };
    var getToolbar = function (editor) {
      return editor.getParam('toolbar', defaults, 'array');
    };
    var getStyleFormats = function (editor) {
      return editor.getParam('style_formats', DefaultStyleFormats, 'array');
    };
    var getSkinUrl = function (editor) {
      return editor.getParam('skin_url');
    };

    var formatChanged = 'formatChanged';
    var orientationChanged = 'orientationChanged';
    var dropupDismissed = 'dropupDismissed';

    var fromHtml$1 = function (html, scope) {
      var doc = scope || document;
      var div = doc.createElement('div');
      div.innerHTML = html;
      return children(SugarElement.fromDom(div));
    };

    var get$9 = function (element) {
      return element.dom.innerHTML;
    };
    var set$7 = function (element, content) {
      var owner = owner$2(element);
      var docDom = owner.dom;
      var fragment = SugarElement.fromDom(docDom.createDocumentFragment());
      var contentElements = fromHtml$1(content, docDom);
      append$1(fragment, contentElements);
      empty(element);
      append$2(element, fragment);
    };
    var getOuter = function (element) {
      var container = SugarElement.fromTag('div');
      var clone = SugarElement.fromDom(element.dom.cloneNode(true));
      append$2(container, clone);
      return get$9(container);
    };

    var clone = function (original, isDeep) {
      return SugarElement.fromDom(original.dom.cloneNode(isDeep));
    };
    var shallow = function (original) {
      return clone(original, false);
    };

    var getHtml = function (element) {
      if (isShadowRoot(element)) {
        return '#shadow-root';
      } else {
        var clone = shallow(element);
        return getOuter(clone);
      }
    };

    var element = function (elem) {
      return getHtml(elem);
    };

    var chooseChannels = function (channels, message) {
      return message.universal ? channels : filter$2(channels, function (ch) {
        return contains$1(message.channels, ch);
      });
    };
    var events$a = function (receiveConfig) {
      return derive$3([run(receive$1(), function (component, message) {
          var channelMap = receiveConfig.channels;
          var channels = keys(channelMap);
          var receivingData = message;
          var targetChannels = chooseChannels(channels, receivingData);
          each$1(targetChannels, function (ch) {
            var channelInfo = channelMap[ch];
            var channelSchema = channelInfo.schema;
            var data = asRawOrDie$1('channel[' + ch + '] data\nReceiver: ' + element(component.element), channelSchema, receivingData.data);
            channelInfo.onReceive(component, data);
          });
        })]);
    };

    var ActiveReceiving = /*#__PURE__*/Object.freeze({
        __proto__: null,
        events: events$a
    });

    var unknown = 'unknown';
    var EventConfiguration;
    (function (EventConfiguration) {
      EventConfiguration[EventConfiguration['STOP'] = 0] = 'STOP';
      EventConfiguration[EventConfiguration['NORMAL'] = 1] = 'NORMAL';
      EventConfiguration[EventConfiguration['LOGGING'] = 2] = 'LOGGING';
    }(EventConfiguration || (EventConfiguration = {})));
    var eventConfig = Cell({});
    var makeEventLogger = function (eventName, initialTarget) {
      var sequence = [];
      var startTime = new Date().getTime();
      return {
        logEventCut: function (_name, target, purpose) {
          sequence.push({
            outcome: 'cut',
            target: target,
            purpose: purpose
          });
        },
        logEventStopped: function (_name, target, purpose) {
          sequence.push({
            outcome: 'stopped',
            target: target,
            purpose: purpose
          });
        },
        logNoParent: function (_name, target, purpose) {
          sequence.push({
            outcome: 'no-parent',
            target: target,
            purpose: purpose
          });
        },
        logEventNoHandlers: function (_name, target) {
          sequence.push({
            outcome: 'no-handlers-left',
            target: target
          });
        },
        logEventResponse: function (_name, target, purpose) {
          sequence.push({
            outcome: 'response',
            purpose: purpose,
            target: target
          });
        },
        write: function () {
          var finishTime = new Date().getTime();
          if (contains$1([
              'mousemove',
              'mouseover',
              'mouseout',
              systemInit()
            ], eventName)) {
            return;
          }
          console.log(eventName, {
            event: eventName,
            time: finishTime - startTime,
            target: initialTarget.dom,
            sequence: map$2(sequence, function (s) {
              if (!contains$1([
                  'cut',
                  'stopped',
                  'response'
                ], s.outcome)) {
                return s.outcome;
              } else {
                return '{' + s.purpose + '} ' + s.outcome + ' at (' + element(s.target) + ')';
              }
            })
          });
        }
      };
    };
    var processEvent = function (eventName, initialTarget, f) {
      var status = get$c(eventConfig.get(), eventName).orThunk(function () {
        var patterns = keys(eventConfig.get());
        return findMap(patterns, function (p) {
          return eventName.indexOf(p) > -1 ? Optional.some(eventConfig.get()[p]) : Optional.none();
        });
      }).getOr(EventConfiguration.NORMAL);
      switch (status) {
      case EventConfiguration.NORMAL:
        return f(noLogger());
      case EventConfiguration.LOGGING: {
          var logger = makeEventLogger(eventName, initialTarget);
          var output = f(logger);
          logger.write();
          return output;
        }
      case EventConfiguration.STOP:
        return true;
      }
    };
    var path = [
      'alloy/data/Fields',
      'alloy/debugging/Debugging'
    ];
    var getTrace = function () {
      var err = new Error();
      if (err.stack !== undefined) {
        var lines = err.stack.split('\n');
        return find$2(lines, function (line) {
          return line.indexOf('alloy') > 0 && !exists(path, function (p) {
            return line.indexOf(p) > -1;
          });
        }).getOr(unknown);
      } else {
        return unknown;
      }
    };
    var ignoreEvent = {
      logEventCut: noop,
      logEventStopped: noop,
      logNoParent: noop,
      logEventNoHandlers: noop,
      logEventResponse: noop,
      write: noop
    };
    var monitorEvent = function (eventName, initialTarget, f) {
      return processEvent(eventName, initialTarget, f);
    };
    var noLogger = constant$1(ignoreEvent);

    var menuFields = constant$1([
      required$1('menu'),
      required$1('selectedMenu')
    ]);
    var itemFields = constant$1([
      required$1('item'),
      required$1('selectedItem')
    ]);
    constant$1(objOf(itemFields().concat(menuFields())));
    var itemSchema$1 = constant$1(objOf(itemFields()));

    var _initSize = requiredObjOf('initSize', [
      required$1('numColumns'),
      required$1('numRows')
    ]);
    var itemMarkers = function () {
      return requiredOf('markers', itemSchema$1());
    };
    var tieredMenuMarkers = function () {
      return requiredObjOf('markers', [required$1('backgroundMenu')].concat(menuFields()).concat(itemFields()));
    };
    var markers = function (required) {
      return requiredObjOf('markers', map$2(required, required$1));
    };
    var onPresenceHandler = function (label, fieldName, presence) {
      getTrace();
      return field$2(fieldName, fieldName, presence, valueOf(function (f) {
        return Result.value(function () {
          var args = [];
          for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
          }
          return f.apply(undefined, args);
        });
      }));
    };
    var onHandler = function (fieldName) {
      return onPresenceHandler('onHandler', fieldName, defaulted$1(noop));
    };
    var onKeyboardHandler = function (fieldName) {
      return onPresenceHandler('onKeyboardHandler', fieldName, defaulted$1(Optional.none));
    };
    var onStrictHandler = function (fieldName) {
      return onPresenceHandler('onHandler', fieldName, required$2());
    };
    var onStrictKeyboardHandler = function (fieldName) {
      return onPresenceHandler('onKeyboardHandler', fieldName, required$2());
    };
    var output = function (name, value) {
      return customField(name, constant$1(value));
    };
    var snapshot = function (name) {
      return customField(name, identity);
    };
    var initSize = constant$1(_initSize);

    var ReceivingSchema = [requiredOf('channels', setOf(Result.value, objOfOnly([
        onStrictHandler('onReceive'),
        defaulted('schema', anyValue())
      ])))];

    var Receiving = create$5({
      fields: ReceivingSchema,
      name: 'receiving',
      active: ActiveReceiving
    });

    var SetupBehaviourCellState = function (initialState) {
      var init = function () {
        var cell = Cell(initialState);
        var get = function () {
          return cell.get();
        };
        var set = function (newState) {
          return cell.set(newState);
        };
        var clear = function () {
          return cell.set(initialState);
        };
        var readState = function () {
          return cell.get();
        };
        return {
          get: get,
          set: set,
          clear: clear,
          readState: readState
        };
      };
      return { init: init };
    };

    var updateAriaState = function (component, toggleConfig, toggleState) {
      var ariaInfo = toggleConfig.aria;
      ariaInfo.update(component, ariaInfo, toggleState.get());
    };
    var updateClass = function (component, toggleConfig, toggleState) {
      toggleConfig.toggleClass.each(function (toggleClass) {
        if (toggleState.get()) {
          add$1(component.element, toggleClass);
        } else {
          remove$3(component.element, toggleClass);
        }
      });
    };
    var toggle = function (component, toggleConfig, toggleState) {
      set$6(component, toggleConfig, toggleState, !toggleState.get());
    };
    var on$1 = function (component, toggleConfig, toggleState) {
      toggleState.set(true);
      updateClass(component, toggleConfig, toggleState);
      updateAriaState(component, toggleConfig, toggleState);
    };
    var off = function (component, toggleConfig, toggleState) {
      toggleState.set(false);
      updateClass(component, toggleConfig, toggleState);
      updateAriaState(component, toggleConfig, toggleState);
    };
    var set$6 = function (component, toggleConfig, toggleState, state) {
      var action = state ? on$1 : off;
      action(component, toggleConfig, toggleState);
    };
    var isOn = function (component, toggleConfig, toggleState) {
      return toggleState.get();
    };
    var onLoad$5 = function (component, toggleConfig, toggleState) {
      set$6(component, toggleConfig, toggleState, toggleConfig.selected);
    };

    var ToggleApis = /*#__PURE__*/Object.freeze({
        __proto__: null,
        onLoad: onLoad$5,
        toggle: toggle,
        isOn: isOn,
        on: on$1,
        off: off,
        set: set$6
    });

    var exhibit$5 = function () {
      return nu$3({});
    };
    var events$9 = function (toggleConfig, toggleState) {
      var execute = executeEvent(toggleConfig, toggleState, toggle);
      var load = loadEvent(toggleConfig, toggleState, onLoad$5);
      return derive$3(flatten([
        toggleConfig.toggleOnExecute ? [execute] : [],
        [load]
      ]));
    };

    var ActiveToggle = /*#__PURE__*/Object.freeze({
        __proto__: null,
        exhibit: exhibit$5,
        events: events$9
    });

    var updatePressed = function (component, ariaInfo, status) {
      set$8(component.element, 'aria-pressed', status);
      if (ariaInfo.syncWithExpanded) {
        updateExpanded(component, ariaInfo, status);
      }
    };
    var updateSelected = function (component, ariaInfo, status) {
      set$8(component.element, 'aria-selected', status);
    };
    var updateChecked = function (component, ariaInfo, status) {
      set$8(component.element, 'aria-checked', status);
    };
    var updateExpanded = function (component, ariaInfo, status) {
      set$8(component.element, 'aria-expanded', status);
    };

    var ToggleSchema = [
      defaulted('selected', false),
      option('toggleClass'),
      defaulted('toggleOnExecute', true),
      defaultedOf('aria', { mode: 'none' }, choose$1('mode', {
        pressed: [
          defaulted('syncWithExpanded', false),
          output('update', updatePressed)
        ],
        checked: [output('update', updateChecked)],
        expanded: [output('update', updateExpanded)],
        selected: [output('update', updateSelected)],
        none: [output('update', noop)]
      }))
    ];

    var Toggling = create$5({
      fields: ToggleSchema,
      name: 'toggling',
      active: ActiveToggle,
      apis: ToggleApis,
      state: SetupBehaviourCellState(false)
    });

    var format = function (command, update) {
      return Receiving.config({
        channels: wrap(formatChanged, {
          onReceive: function (button, data) {
            if (data.command === command) {
              update(button, data.state);
            }
          }
        })
      });
    };
    var orientation = function (onReceive) {
      return Receiving.config({ channels: wrap(orientationChanged, { onReceive: onReceive }) });
    };
    var receive = function (channel, onReceive) {
      return {
        key: channel,
        value: { onReceive: onReceive }
      };
    };

    var prefix$2 = 'tinymce-mobile';
    var resolve = function (p) {
      return prefix$2 + '-' + p;
    };

    var pointerEvents = function () {
      var onClick = function (component, simulatedEvent) {
        simulatedEvent.stop();
        emitExecute(component);
      };
      return [
        run(click(), onClick),
        run(tap(), onClick),
        cutter(touchstart()),
        cutter(mousedown())
      ];
    };
    var events$8 = function (optAction) {
      var executeHandler = function (action) {
        return runOnExecute(function (component, simulatedEvent) {
          action(component);
          simulatedEvent.stop();
        });
      };
      return derive$3(flatten([
        optAction.map(executeHandler).toArray(),
        pointerEvents()
      ]));
    };

    var focus$2 = function (component, focusConfig) {
      if (!focusConfig.ignore) {
        focus$3(component.element);
        focusConfig.onFocus(component);
      }
    };
    var blur = function (component, focusConfig) {
      if (!focusConfig.ignore) {
        blur$1(component.element);
      }
    };
    var isFocused = function (component) {
      return hasFocus(component.element);
    };

    var FocusApis = /*#__PURE__*/Object.freeze({
        __proto__: null,
        focus: focus$2,
        blur: blur,
        isFocused: isFocused
    });

    var exhibit$4 = function (base, focusConfig) {
      var mod = focusConfig.ignore ? {} : { attributes: { tabindex: '-1' } };
      return nu$3(mod);
    };
    var events$7 = function (focusConfig) {
      return derive$3([run(focus$4(), function (component, simulatedEvent) {
          focus$2(component, focusConfig);
          simulatedEvent.stop();
        })].concat(focusConfig.stopMousedown ? [run(mousedown(), function (_, simulatedEvent) {
          simulatedEvent.event.prevent();
        })] : []));
    };

    var ActiveFocus = /*#__PURE__*/Object.freeze({
        __proto__: null,
        exhibit: exhibit$4,
        events: events$7
    });

    var FocusSchema = [
      onHandler('onFocus'),
      defaulted('stopMousedown', false),
      defaulted('ignore', false)
    ];

    var Focusing = create$5({
      fields: FocusSchema,
      name: 'focusing',
      active: ActiveFocus,
      apis: FocusApis
    });

    var isSupported = function (dom) {
      return dom.style !== undefined && isFunction(dom.style.getPropertyValue);
    };

    var internalSet = function (dom, property, value) {
      if (!isString(value)) {
        console.error('Invalid call to CSS.set. Property ', property, ':: Value ', value, ':: Element ', dom);
        throw new Error('CSS value must be a string: ' + value);
      }
      if (isSupported(dom)) {
        dom.style.setProperty(property, value);
      }
    };
    var internalRemove = function (dom, property) {
      if (isSupported(dom)) {
        dom.style.removeProperty(property);
      }
    };
    var set$5 = function (element, property, value) {
      var dom = element.dom;
      internalSet(dom, property, value);
    };
    var setAll = function (element, css) {
      var dom = element.dom;
      each(css, function (v, k) {
        internalSet(dom, k, v);
      });
    };
    var get$8 = function (element, property) {
      var dom = element.dom;
      var styles = window.getComputedStyle(dom);
      var r = styles.getPropertyValue(property);
      return r === '' && !inBody(element) ? getUnsafeProperty(dom, property) : r;
    };
    var getUnsafeProperty = function (dom, property) {
      return isSupported(dom) ? dom.style.getPropertyValue(property) : '';
    };
    var getRaw = function (element, property) {
      var dom = element.dom;
      var raw = getUnsafeProperty(dom, property);
      return Optional.from(raw).filter(function (r) {
        return r.length > 0;
      });
    };
    var remove$2 = function (element, property) {
      var dom = element.dom;
      internalRemove(dom, property);
      if (is(getOpt(element, 'style').map(trim), '')) {
        remove$6(element, 'style');
      }
    };
    var reflow = function (e) {
      return e.dom.offsetWidth;
    };

    var Dimension = function (name, getOffset) {
      var set = function (element, h) {
        if (!isNumber(h) && !h.match(/^[0-9]+$/)) {
          throw new Error(name + '.set accepts only positive integer values. Value was ' + h);
        }
        var dom = element.dom;
        if (isSupported(dom)) {
          dom.style[name] = h + 'px';
        }
      };
      var get = function (element) {
        var r = getOffset(element);
        if (r <= 0 || r === null) {
          var css = get$8(element, name);
          return parseFloat(css) || 0;
        }
        return r;
      };
      var getOuter = get;
      var aggregate = function (element, properties) {
        return foldl(properties, function (acc, property) {
          var val = get$8(element, property);
          var value = val === undefined ? 0 : parseInt(val, 10);
          return isNaN(value) ? acc : acc + value;
        }, 0);
      };
      var max = function (element, value, properties) {
        var cumulativeInclusions = aggregate(element, properties);
        var absoluteMax = value > cumulativeInclusions ? value - cumulativeInclusions : 0;
        return absoluteMax;
      };
      return {
        set: set,
        get: get,
        getOuter: getOuter,
        aggregate: aggregate,
        max: max
      };
    };

    var api$3 = Dimension('height', function (element) {
      var dom = element.dom;
      return inBody(element) ? dom.getBoundingClientRect().height : dom.offsetHeight;
    });
    var get$7 = function (element) {
      return api$3.get(element);
    };

    var ancestors$1 = function (scope, predicate, isRoot) {
      return filter$2(parents(scope, isRoot), predicate);
    };
    var siblings$1 = function (scope, predicate) {
      return filter$2(siblings$2(scope), predicate);
    };

    var all = function (selector) {
      return all$2(selector);
    };
    var ancestors = function (scope, selector, isRoot) {
      return ancestors$1(scope, function (e) {
        return is$1(e, selector);
      }, isRoot);
    };
    var siblings = function (scope, selector) {
      return siblings$1(scope, function (e) {
        return is$1(e, selector);
      });
    };
    var descendants = function (scope, selector) {
      return all$2(selector, scope);
    };

    function ClosestOrAncestor (is, ancestor, scope, a, isRoot) {
      if (is(scope, a)) {
        return Optional.some(scope);
      } else if (isFunction(isRoot) && isRoot(scope)) {
        return Optional.none();
      } else {
        return ancestor(scope, a, isRoot);
      }
    }

    var ancestor$1 = function (scope, predicate, isRoot) {
      var element = scope.dom;
      var stop = isFunction(isRoot) ? isRoot : never;
      while (element.parentNode) {
        element = element.parentNode;
        var el = SugarElement.fromDom(element);
        if (predicate(el)) {
          return Optional.some(el);
        } else if (stop(el)) {
          break;
        }
      }
      return Optional.none();
    };
    var closest$2 = function (scope, predicate, isRoot) {
      var is = function (s, test) {
        return test(s);
      };
      return ClosestOrAncestor(is, ancestor$1, scope, predicate, isRoot);
    };
    var descendant$1 = function (scope, predicate) {
      var descend = function (node) {
        for (var i = 0; i < node.childNodes.length; i++) {
          var child_1 = SugarElement.fromDom(node.childNodes[i]);
          if (predicate(child_1)) {
            return Optional.some(child_1);
          }
          var res = descend(node.childNodes[i]);
          if (res.isSome()) {
            return res;
          }
        }
        return Optional.none();
      };
      return descend(scope.dom);
    };

    var first$1 = function (selector) {
      return one(selector);
    };
    var ancestor = function (scope, selector, isRoot) {
      return ancestor$1(scope, function (e) {
        return is$1(e, selector);
      }, isRoot);
    };
    var descendant = function (scope, selector) {
      return one(selector, scope);
    };
    var closest$1 = function (scope, selector, isRoot) {
      var is = function (element, selector) {
        return is$1(element, selector);
      };
      return ClosestOrAncestor(is, ancestor, scope, selector, isRoot);
    };

    var BACKSPACE = [8];
    var TAB = [9];
    var ENTER = [13];
    var ESCAPE = [27];
    var SPACE = [32];
    var LEFT = [37];
    var UP = [38];
    var RIGHT = [39];
    var DOWN = [40];

    var cyclePrev = function (values, index, predicate) {
      var before = reverse(values.slice(0, index));
      var after = reverse(values.slice(index + 1));
      return find$2(before.concat(after), predicate);
    };
    var tryPrev = function (values, index, predicate) {
      var before = reverse(values.slice(0, index));
      return find$2(before, predicate);
    };
    var cycleNext = function (values, index, predicate) {
      var before = values.slice(0, index);
      var after = values.slice(index + 1);
      return find$2(after.concat(before), predicate);
    };
    var tryNext = function (values, index, predicate) {
      var after = values.slice(index + 1);
      return find$2(after, predicate);
    };

    var inSet = function (keys) {
      return function (event) {
        var raw = event.raw;
        return contains$1(keys, raw.which);
      };
    };
    var and = function (preds) {
      return function (event) {
        return forall(preds, function (pred) {
          return pred(event);
        });
      };
    };
    var isShift = function (event) {
      var raw = event.raw;
      return raw.shiftKey === true;
    };
    var isControl = function (event) {
      var raw = event.raw;
      return raw.ctrlKey === true;
    };
    var isNotShift = not(isShift);

    var rule = function (matches, action) {
      return {
        matches: matches,
        classification: action
      };
    };
    var choose = function (transitions, event) {
      var transition = find$2(transitions, function (t) {
        return t.matches(event);
      });
      return transition.map(function (t) {
        return t.classification;
      });
    };

    var cycleBy = function (value, delta, min, max) {
      var r = value + delta;
      if (r > max) {
        return min;
      } else if (r < min) {
        return max;
      } else {
        return r;
      }
    };
    var clamp = function (value, min, max) {
      return Math.min(Math.max(value, min), max);
    };

    var dehighlightAllExcept = function (component, hConfig, hState, skip) {
      var highlighted = descendants(component.element, '.' + hConfig.highlightClass);
      each$1(highlighted, function (h) {
        if (!exists(skip, function (skipComp) {
            return skipComp.element === h;
          })) {
          remove$3(h, hConfig.highlightClass);
          component.getSystem().getByDom(h).each(function (target) {
            hConfig.onDehighlight(component, target);
            emit(target, dehighlight$1());
          });
        }
      });
    };
    var dehighlightAll = function (component, hConfig, hState) {
      return dehighlightAllExcept(component, hConfig, hState, []);
    };
    var dehighlight = function (component, hConfig, hState, target) {
      if (isHighlighted(component, hConfig, hState, target)) {
        remove$3(target.element, hConfig.highlightClass);
        hConfig.onDehighlight(component, target);
        emit(target, dehighlight$1());
      }
    };
    var highlight = function (component, hConfig, hState, target) {
      dehighlightAllExcept(component, hConfig, hState, [target]);
      if (!isHighlighted(component, hConfig, hState, target)) {
        add$1(target.element, hConfig.highlightClass);
        hConfig.onHighlight(component, target);
        emit(target, highlight$1());
      }
    };
    var highlightFirst = function (component, hConfig, hState) {
      getFirst(component, hConfig).each(function (firstComp) {
        highlight(component, hConfig, hState, firstComp);
      });
    };
    var highlightLast = function (component, hConfig, hState) {
      getLast(component, hConfig).each(function (lastComp) {
        highlight(component, hConfig, hState, lastComp);
      });
    };
    var highlightAt = function (component, hConfig, hState, index) {
      getByIndex(component, hConfig, hState, index).fold(function (err) {
        throw err;
      }, function (firstComp) {
        highlight(component, hConfig, hState, firstComp);
      });
    };
    var highlightBy = function (component, hConfig, hState, predicate) {
      var candidates = getCandidates(component, hConfig);
      var targetComp = find$2(candidates, predicate);
      targetComp.each(function (c) {
        highlight(component, hConfig, hState, c);
      });
    };
    var isHighlighted = function (component, hConfig, hState, queryTarget) {
      return has(queryTarget.element, hConfig.highlightClass);
    };
    var getHighlighted = function (component, hConfig, _hState) {
      return descendant(component.element, '.' + hConfig.highlightClass).bind(function (e) {
        return component.getSystem().getByDom(e).toOptional();
      });
    };
    var getByIndex = function (component, hConfig, hState, index) {
      var items = descendants(component.element, '.' + hConfig.itemClass);
      return Optional.from(items[index]).fold(function () {
        return Result.error(new Error('No element found with index ' + index));
      }, component.getSystem().getByDom);
    };
    var getFirst = function (component, hConfig, _hState) {
      return descendant(component.element, '.' + hConfig.itemClass).bind(function (e) {
        return component.getSystem().getByDom(e).toOptional();
      });
    };
    var getLast = function (component, hConfig, _hState) {
      var items = descendants(component.element, '.' + hConfig.itemClass);
      var last = items.length > 0 ? Optional.some(items[items.length - 1]) : Optional.none();
      return last.bind(function (c) {
        return component.getSystem().getByDom(c).toOptional();
      });
    };
    var getDelta = function (component, hConfig, hState, delta) {
      var items = descendants(component.element, '.' + hConfig.itemClass);
      var current = findIndex$1(items, function (item) {
        return has(item, hConfig.highlightClass);
      });
      return current.bind(function (selected) {
        var dest = cycleBy(selected, delta, 0, items.length - 1);
        return component.getSystem().getByDom(items[dest]).toOptional();
      });
    };
    var getPrevious = function (component, hConfig, hState) {
      return getDelta(component, hConfig, hState, -1);
    };
    var getNext = function (component, hConfig, hState) {
      return getDelta(component, hConfig, hState, +1);
    };
    var getCandidates = function (component, hConfig, _hState) {
      var items = descendants(component.element, '.' + hConfig.itemClass);
      return cat(map$2(items, function (i) {
        return component.getSystem().getByDom(i).toOptional();
      }));
    };

    var HighlightApis = /*#__PURE__*/Object.freeze({
        __proto__: null,
        dehighlightAll: dehighlightAll,
        dehighlight: dehighlight,
        highlight: highlight,
        highlightFirst: highlightFirst,
        highlightLast: highlightLast,
        highlightAt: highlightAt,
        highlightBy: highlightBy,
        isHighlighted: isHighlighted,
        getHighlighted: getHighlighted,
        getFirst: getFirst,
        getLast: getLast,
        getPrevious: getPrevious,
        getNext: getNext,
        getCandidates: getCandidates
    });

    var HighlightSchema = [
      required$1('highlightClass'),
      required$1('itemClass'),
      onHandler('onHighlight'),
      onHandler('onDehighlight')
    ];

    var Highlighting = create$5({
      fields: HighlightSchema,
      name: 'highlighting',
      apis: HighlightApis
    });

    var reportFocusShifting = function (component, prevFocus, newFocus) {
      var noChange = prevFocus.exists(function (p) {
        return newFocus.exists(function (n) {
          return eq(n, p);
        });
      });
      if (!noChange) {
        emitWith(component, focusShifted(), {
          prevFocus: prevFocus,
          newFocus: newFocus
        });
      }
    };
    var dom$2 = function () {
      var get = function (component) {
        return search(component.element);
      };
      var set = function (component, focusee) {
        var prevFocus = get(component);
        component.getSystem().triggerFocus(focusee, component.element);
        var newFocus = get(component);
        reportFocusShifting(component, prevFocus, newFocus);
      };
      return {
        get: get,
        set: set
      };
    };
    var highlights = function () {
      var get = function (component) {
        return Highlighting.getHighlighted(component).map(function (item) {
          return item.element;
        });
      };
      var set = function (component, element) {
        var prevFocus = get(component);
        component.getSystem().getByDom(element).fold(noop, function (item) {
          Highlighting.highlight(component, item);
        });
        var newFocus = get(component);
        reportFocusShifting(component, prevFocus, newFocus);
      };
      return {
        get: get,
        set: set
      };
    };

    var FocusInsideModes;
    (function (FocusInsideModes) {
      FocusInsideModes['OnFocusMode'] = 'onFocus';
      FocusInsideModes['OnEnterOrSpaceMode'] = 'onEnterOrSpace';
      FocusInsideModes['OnApiMode'] = 'onApi';
    }(FocusInsideModes || (FocusInsideModes = {})));

    var typical = function (infoSchema, stateInit, getKeydownRules, getKeyupRules, optFocusIn) {
      var schema = function () {
        return infoSchema.concat([
          defaulted('focusManager', dom$2()),
          defaultedOf('focusInside', 'onFocus', valueOf(function (val) {
            return contains$1([
              'onFocus',
              'onEnterOrSpace',
              'onApi'
            ], val) ? Result.value(val) : Result.error('Invalid value for focusInside');
          })),
          output('handler', me),
          output('state', stateInit),
          output('sendFocusIn', optFocusIn)
        ]);
      };
      var processKey = function (component, simulatedEvent, getRules, keyingConfig, keyingState) {
        var rules = getRules(component, simulatedEvent, keyingConfig, keyingState);
        return choose(rules, simulatedEvent.event).bind(function (rule) {
          return rule(component, simulatedEvent, keyingConfig, keyingState);
        });
      };
      var toEvents = function (keyingConfig, keyingState) {
        var onFocusHandler = keyingConfig.focusInside !== FocusInsideModes.OnFocusMode ? Optional.none() : optFocusIn(keyingConfig).map(function (focusIn) {
          return run(focus$4(), function (component, simulatedEvent) {
            focusIn(component, keyingConfig, keyingState);
            simulatedEvent.stop();
          });
        });
        var tryGoInsideComponent = function (component, simulatedEvent) {
          var isEnterOrSpace = inSet(SPACE.concat(ENTER))(simulatedEvent.event);
          if (keyingConfig.focusInside === FocusInsideModes.OnEnterOrSpaceMode && isEnterOrSpace && isSource(component, simulatedEvent)) {
            optFocusIn(keyingConfig).each(function (focusIn) {
              focusIn(component, keyingConfig, keyingState);
              simulatedEvent.stop();
            });
          }
        };
        var keyboardEvents = [
          run(keydown(), function (component, simulatedEvent) {
            processKey(component, simulatedEvent, getKeydownRules, keyingConfig, keyingState).fold(function () {
              tryGoInsideComponent(component, simulatedEvent);
            }, function (_) {
              simulatedEvent.stop();
            });
          }),
          run(keyup(), function (component, simulatedEvent) {
            processKey(component, simulatedEvent, getKeyupRules, keyingConfig, keyingState).each(function (_) {
              simulatedEvent.stop();
            });
          })
        ];
        return derive$3(onFocusHandler.toArray().concat(keyboardEvents));
      };
      var me = {
        schema: schema,
        processKey: processKey,
        toEvents: toEvents
      };
      return me;
    };

    var create$4 = function (cyclicField) {
      var schema = [
        option('onEscape'),
        option('onEnter'),
        defaulted('selector', '[data-alloy-tabstop="true"]:not(:disabled)'),
        defaulted('firstTabstop', 0),
        defaulted('useTabstopAt', always),
        option('visibilitySelector')
      ].concat([cyclicField]);
      var isVisible = function (tabbingConfig, element) {
        var target = tabbingConfig.visibilitySelector.bind(function (sel) {
          return closest$1(element, sel);
        }).getOr(element);
        return get$7(target) > 0;
      };
      var findInitial = function (component, tabbingConfig) {
        var tabstops = descendants(component.element, tabbingConfig.selector);
        var visibles = filter$2(tabstops, function (elem) {
          return isVisible(tabbingConfig, elem);
        });
        return Optional.from(visibles[tabbingConfig.firstTabstop]);
      };
      var findCurrent = function (component, tabbingConfig) {
        return tabbingConfig.focusManager.get(component).bind(function (elem) {
          return closest$1(elem, tabbingConfig.selector);
        });
      };
      var isTabstop = function (tabbingConfig, element) {
        return isVisible(tabbingConfig, element) && tabbingConfig.useTabstopAt(element);
      };
      var focusIn = function (component, tabbingConfig, _tabbingState) {
        findInitial(component, tabbingConfig).each(function (target) {
          tabbingConfig.focusManager.set(component, target);
        });
      };
      var goFromTabstop = function (component, tabstops, stopIndex, tabbingConfig, cycle) {
        return cycle(tabstops, stopIndex, function (elem) {
          return isTabstop(tabbingConfig, elem);
        }).fold(function () {
          return tabbingConfig.cyclic ? Optional.some(true) : Optional.none();
        }, function (target) {
          tabbingConfig.focusManager.set(component, target);
          return Optional.some(true);
        });
      };
      var go = function (component, _simulatedEvent, tabbingConfig, cycle) {
        var tabstops = descendants(component.element, tabbingConfig.selector);
        return findCurrent(component, tabbingConfig).bind(function (tabstop) {
          var optStopIndex = findIndex$1(tabstops, curry(eq, tabstop));
          return optStopIndex.bind(function (stopIndex) {
            return goFromTabstop(component, tabstops, stopIndex, tabbingConfig, cycle);
          });
        });
      };
      var goBackwards = function (component, simulatedEvent, tabbingConfig) {
        var navigate = tabbingConfig.cyclic ? cyclePrev : tryPrev;
        return go(component, simulatedEvent, tabbingConfig, navigate);
      };
      var goForwards = function (component, simulatedEvent, tabbingConfig) {
        var navigate = tabbingConfig.cyclic ? cycleNext : tryNext;
        return go(component, simulatedEvent, tabbingConfig, navigate);
      };
      var execute = function (component, simulatedEvent, tabbingConfig) {
        return tabbingConfig.onEnter.bind(function (f) {
          return f(component, simulatedEvent);
        });
      };
      var exit = function (component, simulatedEvent, tabbingConfig) {
        return tabbingConfig.onEscape.bind(function (f) {
          return f(component, simulatedEvent);
        });
      };
      var getKeydownRules = constant$1([
        rule(and([
          isShift,
          inSet(TAB)
        ]), goBackwards),
        rule(inSet(TAB), goForwards),
        rule(inSet(ESCAPE), exit),
        rule(and([
          isNotShift,
          inSet(ENTER)
        ]), execute)
      ]);
      var getKeyupRules = constant$1([]);
      return typical(schema, NoState.init, getKeydownRules, getKeyupRules, function () {
        return Optional.some(focusIn);
      });
    };

    var AcyclicType = create$4(customField('cyclic', never));

    var CyclicType = create$4(customField('cyclic', always));

    var inside = function (target) {
      return name$1(target) === 'input' && get$b(target, 'type') !== 'radio' || name$1(target) === 'textarea';
    };

    var doDefaultExecute = function (component, _simulatedEvent, focused) {
      dispatch(component, focused, execute$5());
      return Optional.some(true);
    };
    var defaultExecute = function (component, simulatedEvent, focused) {
      var isComplex = inside(focused) && inSet(SPACE)(simulatedEvent.event);
      return isComplex ? Optional.none() : doDefaultExecute(component, simulatedEvent, focused);
    };
    var stopEventForFirefox = function (_component, _simulatedEvent) {
      return Optional.some(true);
    };

    var schema$f = [
      defaulted('execute', defaultExecute),
      defaulted('useSpace', false),
      defaulted('useEnter', true),
      defaulted('useControlEnter', false),
      defaulted('useDown', false)
    ];
    var execute$4 = function (component, simulatedEvent, executeConfig) {
      return executeConfig.execute(component, simulatedEvent, component.element);
    };
    var getKeydownRules$5 = function (component, _simulatedEvent, executeConfig, _executeState) {
      var spaceExec = executeConfig.useSpace && !inside(component.element) ? SPACE : [];
      var enterExec = executeConfig.useEnter ? ENTER : [];
      var downExec = executeConfig.useDown ? DOWN : [];
      var execKeys = spaceExec.concat(enterExec).concat(downExec);
      return [rule(inSet(execKeys), execute$4)].concat(executeConfig.useControlEnter ? [rule(and([
          isControl,
          inSet(ENTER)
        ]), execute$4)] : []);
    };
    var getKeyupRules$5 = function (component, _simulatedEvent, executeConfig, _executeState) {
      return executeConfig.useSpace && !inside(component.element) ? [rule(inSet(SPACE), stopEventForFirefox)] : [];
    };
    var ExecutionType = typical(schema$f, NoState.init, getKeydownRules$5, getKeyupRules$5, function () {
      return Optional.none();
    });

    var singleton$1 = function (doRevoke) {
      var subject = Cell(Optional.none());
      var revoke = function () {
        return subject.get().each(doRevoke);
      };
      var clear = function () {
        revoke();
        subject.set(Optional.none());
      };
      var isSet = function () {
        return subject.get().isSome();
      };
      var get = function () {
        return subject.get();
      };
      var set = function (s) {
        revoke();
        subject.set(Optional.some(s));
      };
      return {
        clear: clear,
        isSet: isSet,
        get: get,
        set: set
      };
    };
    var destroyable = function () {
      return singleton$1(function (s) {
        return s.destroy();
      });
    };
    var api$2 = function () {
      var subject = destroyable();
      var run = function (f) {
        return subject.get().each(f);
      };
      return __assign(__assign({}, subject), { run: run });
    };
    var value = function () {
      var subject = singleton$1(noop);
      var on = function (f) {
        return subject.get().each(f);
      };
      return __assign(__assign({}, subject), { on: on });
    };

    var flatgrid$1 = function () {
      var dimensions = value();
      var setGridSize = function (numRows, numColumns) {
        dimensions.set({
          numRows: numRows,
          numColumns: numColumns
        });
      };
      var getNumRows = function () {
        return dimensions.get().map(function (d) {
          return d.numRows;
        });
      };
      var getNumColumns = function () {
        return dimensions.get().map(function (d) {
          return d.numColumns;
        });
      };
      return nu$2({
        readState: function () {
          return dimensions.get().map(function (d) {
            return {
              numRows: String(d.numRows),
              numColumns: String(d.numColumns)
            };
          }).getOr({
            numRows: '?',
            numColumns: '?'
          });
        },
        setGridSize: setGridSize,
        getNumRows: getNumRows,
        getNumColumns: getNumColumns
      });
    };
    var init$5 = function (spec) {
      return spec.state(spec);
    };

    var KeyingState = /*#__PURE__*/Object.freeze({
        __proto__: null,
        flatgrid: flatgrid$1,
        init: init$5
    });

    var onDirection = function (isLtr, isRtl) {
      return function (element) {
        return getDirection(element) === 'rtl' ? isRtl : isLtr;
      };
    };
    var getDirection = function (element) {
      return get$8(element, 'direction') === 'rtl' ? 'rtl' : 'ltr';
    };

    var useH = function (movement) {
      return function (component, simulatedEvent, config, state) {
        var move = movement(component.element);
        return use(move, component, simulatedEvent, config, state);
      };
    };
    var west = function (moveLeft, moveRight) {
      var movement = onDirection(moveLeft, moveRight);
      return useH(movement);
    };
    var east = function (moveLeft, moveRight) {
      var movement = onDirection(moveRight, moveLeft);
      return useH(movement);
    };
    var useV = function (move) {
      return function (component, simulatedEvent, config, state) {
        return use(move, component, simulatedEvent, config, state);
      };
    };
    var use = function (move, component, simulatedEvent, config, state) {
      var outcome = config.focusManager.get(component).bind(function (focused) {
        return move(component.element, focused, config, state);
      });
      return outcome.map(function (newFocus) {
        config.focusManager.set(component, newFocus);
        return true;
      });
    };
    var north = useV;
    var south = useV;
    var move$1 = useV;

    var isHidden = function (dom) {
      return dom.offsetWidth <= 0 && dom.offsetHeight <= 0;
    };
    var isVisible = function (element) {
      return !isHidden(element.dom);
    };

    var locate = function (candidates, predicate) {
      return findIndex$1(candidates, predicate).map(function (index) {
        return {
          index: index,
          candidates: candidates
        };
      });
    };

    var locateVisible = function (container, current, selector) {
      var predicate = function (x) {
        return eq(x, current);
      };
      var candidates = descendants(container, selector);
      var visible = filter$2(candidates, isVisible);
      return locate(visible, predicate);
    };
    var findIndex = function (elements, target) {
      return findIndex$1(elements, function (elem) {
        return eq(target, elem);
      });
    };

    var withGrid = function (values, index, numCols, f) {
      var oldRow = Math.floor(index / numCols);
      var oldColumn = index % numCols;
      return f(oldRow, oldColumn).bind(function (address) {
        var newIndex = address.row * numCols + address.column;
        return newIndex >= 0 && newIndex < values.length ? Optional.some(values[newIndex]) : Optional.none();
      });
    };
    var cycleHorizontal$1 = function (values, index, numRows, numCols, delta) {
      return withGrid(values, index, numCols, function (oldRow, oldColumn) {
        var onLastRow = oldRow === numRows - 1;
        var colsInRow = onLastRow ? values.length - oldRow * numCols : numCols;
        var newColumn = cycleBy(oldColumn, delta, 0, colsInRow - 1);
        return Optional.some({
          row: oldRow,
          column: newColumn
        });
      });
    };
    var cycleVertical$1 = function (values, index, numRows, numCols, delta) {
      return withGrid(values, index, numCols, function (oldRow, oldColumn) {
        var newRow = cycleBy(oldRow, delta, 0, numRows - 1);
        var onLastRow = newRow === numRows - 1;
        var colsInRow = onLastRow ? values.length - newRow * numCols : numCols;
        var newCol = clamp(oldColumn, 0, colsInRow - 1);
        return Optional.some({
          row: newRow,
          column: newCol
        });
      });
    };
    var cycleRight$1 = function (values, index, numRows, numCols) {
      return cycleHorizontal$1(values, index, numRows, numCols, +1);
    };
    var cycleLeft$1 = function (values, index, numRows, numCols) {
      return cycleHorizontal$1(values, index, numRows, numCols, -1);
    };
    var cycleUp$1 = function (values, index, numRows, numCols) {
      return cycleVertical$1(values, index, numRows, numCols, -1);
    };
    var cycleDown$1 = function (values, index, numRows, numCols) {
      return cycleVertical$1(values, index, numRows, numCols, +1);
    };

    var schema$e = [
      required$1('selector'),
      defaulted('execute', defaultExecute),
      onKeyboardHandler('onEscape'),
      defaulted('captureTab', false),
      initSize()
    ];
    var focusIn$3 = function (component, gridConfig, _gridState) {
      descendant(component.element, gridConfig.selector).each(function (first) {
        gridConfig.focusManager.set(component, first);
      });
    };
    var findCurrent$1 = function (component, gridConfig) {
      return gridConfig.focusManager.get(component).bind(function (elem) {
        return closest$1(elem, gridConfig.selector);
      });
    };
    var execute$3 = function (component, simulatedEvent, gridConfig, _gridState) {
      return findCurrent$1(component, gridConfig).bind(function (focused) {
        return gridConfig.execute(component, simulatedEvent, focused);
      });
    };
    var doMove$2 = function (cycle) {
      return function (element, focused, gridConfig, gridState) {
        return locateVisible(element, focused, gridConfig.selector).bind(function (identified) {
          return cycle(identified.candidates, identified.index, gridState.getNumRows().getOr(gridConfig.initSize.numRows), gridState.getNumColumns().getOr(gridConfig.initSize.numColumns));
        });
      };
    };
    var handleTab = function (_component, _simulatedEvent, gridConfig) {
      return gridConfig.captureTab ? Optional.some(true) : Optional.none();
    };
    var doEscape$1 = function (component, simulatedEvent, gridConfig) {
      return gridConfig.onEscape(component, simulatedEvent);
    };
    var moveLeft$3 = doMove$2(cycleLeft$1);
    var moveRight$3 = doMove$2(cycleRight$1);
    var moveNorth$1 = doMove$2(cycleUp$1);
    var moveSouth$1 = doMove$2(cycleDown$1);
    var getKeydownRules$4 = constant$1([
      rule(inSet(LEFT), west(moveLeft$3, moveRight$3)),
      rule(inSet(RIGHT), east(moveLeft$3, moveRight$3)),
      rule(inSet(UP), north(moveNorth$1)),
      rule(inSet(DOWN), south(moveSouth$1)),
      rule(and([
        isShift,
        inSet(TAB)
      ]), handleTab),
      rule(and([
        isNotShift,
        inSet(TAB)
      ]), handleTab),
      rule(inSet(ESCAPE), doEscape$1),
      rule(inSet(SPACE.concat(ENTER)), execute$3)
    ]);
    var getKeyupRules$4 = constant$1([rule(inSet(SPACE), stopEventForFirefox)]);
    var FlatgridType = typical(schema$e, flatgrid$1, getKeydownRules$4, getKeyupRules$4, function () {
      return Optional.some(focusIn$3);
    });

    var horizontal = function (container, selector, current, delta) {
      var isDisabledButton = function (candidate) {
        return name$1(candidate) === 'button' && get$b(candidate, 'disabled') === 'disabled';
      };
      var tryCycle = function (initial, index, candidates) {
        var newIndex = cycleBy(index, delta, 0, candidates.length - 1);
        if (newIndex === initial) {
          return Optional.none();
        } else {
          return isDisabledButton(candidates[newIndex]) ? tryCycle(initial, newIndex, candidates) : Optional.from(candidates[newIndex]);
        }
      };
      return locateVisible(container, current, selector).bind(function (identified) {
        var index = identified.index;
        var candidates = identified.candidates;
        return tryCycle(index, index, candidates);
      });
    };

    var schema$d = [
      required$1('selector'),
      defaulted('getInitial', Optional.none),
      defaulted('execute', defaultExecute),
      onKeyboardHandler('onEscape'),
      defaulted('executeOnMove', false),
      defaulted('allowVertical', true)
    ];
    var findCurrent = function (component, flowConfig) {
      return flowConfig.focusManager.get(component).bind(function (elem) {
        return closest$1(elem, flowConfig.selector);
      });
    };
    var execute$2 = function (component, simulatedEvent, flowConfig) {
      return findCurrent(component, flowConfig).bind(function (focused) {
        return flowConfig.execute(component, simulatedEvent, focused);
      });
    };
    var focusIn$2 = function (component, flowConfig, _state) {
      flowConfig.getInitial(component).orThunk(function () {
        return descendant(component.element, flowConfig.selector);
      }).each(function (first) {
        flowConfig.focusManager.set(component, first);
      });
    };
    var moveLeft$2 = function (element, focused, info) {
      return horizontal(element, info.selector, focused, -1);
    };
    var moveRight$2 = function (element, focused, info) {
      return horizontal(element, info.selector, focused, +1);
    };
    var doMove$1 = function (movement) {
      return function (component, simulatedEvent, flowConfig, flowState) {
        return movement(component, simulatedEvent, flowConfig, flowState).bind(function () {
          return flowConfig.executeOnMove ? execute$2(component, simulatedEvent, flowConfig) : Optional.some(true);
        });
      };
    };
    var doEscape = function (component, simulatedEvent, flowConfig) {
      return flowConfig.onEscape(component, simulatedEvent);
    };
    var getKeydownRules$3 = function (_component, _se, flowConfig, _flowState) {
      var westMovers = LEFT.concat(flowConfig.allowVertical ? UP : []);
      var eastMovers = RIGHT.concat(flowConfig.allowVertical ? DOWN : []);
      return [
        rule(inSet(westMovers), doMove$1(west(moveLeft$2, moveRight$2))),
        rule(inSet(eastMovers), doMove$1(east(moveLeft$2, moveRight$2))),
        rule(inSet(ENTER), execute$2),
        rule(inSet(SPACE), execute$2),
        rule(inSet(ESCAPE), doEscape)
      ];
    };
    var getKeyupRules$3 = constant$1([rule(inSet(SPACE), stopEventForFirefox)]);
    var FlowType = typical(schema$d, NoState.init, getKeydownRules$3, getKeyupRules$3, function () {
      return Optional.some(focusIn$2);
    });

    var toCell = function (matrix, rowIndex, columnIndex) {
      return Optional.from(matrix[rowIndex]).bind(function (row) {
        return Optional.from(row[columnIndex]).map(function (cell) {
          return {
            rowIndex: rowIndex,
            columnIndex: columnIndex,
            cell: cell
          };
        });
      });
    };
    var cycleHorizontal = function (matrix, rowIndex, startCol, deltaCol) {
      var row = matrix[rowIndex];
      var colsInRow = row.length;
      var newColIndex = cycleBy(startCol, deltaCol, 0, colsInRow - 1);
      return toCell(matrix, rowIndex, newColIndex);
    };
    var cycleVertical = function (matrix, colIndex, startRow, deltaRow) {
      var nextRowIndex = cycleBy(startRow, deltaRow, 0, matrix.length - 1);
      var colsInNextRow = matrix[nextRowIndex].length;
      var nextColIndex = clamp(colIndex, 0, colsInNextRow - 1);
      return toCell(matrix, nextRowIndex, nextColIndex);
    };
    var moveHorizontal = function (matrix, rowIndex, startCol, deltaCol) {
      var row = matrix[rowIndex];
      var colsInRow = row.length;
      var newColIndex = clamp(startCol + deltaCol, 0, colsInRow - 1);
      return toCell(matrix, rowIndex, newColIndex);
    };
    var moveVertical = function (matrix, colIndex, startRow, deltaRow) {
      var nextRowIndex = clamp(startRow + deltaRow, 0, matrix.length - 1);
      var colsInNextRow = matrix[nextRowIndex].length;
      var nextColIndex = clamp(colIndex, 0, colsInNextRow - 1);
      return toCell(matrix, nextRowIndex, nextColIndex);
    };
    var cycleRight = function (matrix, startRow, startCol) {
      return cycleHorizontal(matrix, startRow, startCol, +1);
    };
    var cycleLeft = function (matrix, startRow, startCol) {
      return cycleHorizontal(matrix, startRow, startCol, -1);
    };
    var cycleUp = function (matrix, startRow, startCol) {
      return cycleVertical(matrix, startCol, startRow, -1);
    };
    var cycleDown = function (matrix, startRow, startCol) {
      return cycleVertical(matrix, startCol, startRow, +1);
    };
    var moveLeft$1 = function (matrix, startRow, startCol) {
      return moveHorizontal(matrix, startRow, startCol, -1);
    };
    var moveRight$1 = function (matrix, startRow, startCol) {
      return moveHorizontal(matrix, startRow, startCol, +1);
    };
    var moveUp$1 = function (matrix, startRow, startCol) {
      return moveVertical(matrix, startCol, startRow, -1);
    };
    var moveDown$1 = function (matrix, startRow, startCol) {
      return moveVertical(matrix, startCol, startRow, +1);
    };

    var schema$c = [
      requiredObjOf('selectors', [
        required$1('row'),
        required$1('cell')
      ]),
      defaulted('cycles', true),
      defaulted('previousSelector', Optional.none),
      defaulted('execute', defaultExecute)
    ];
    var focusIn$1 = function (component, matrixConfig, _state) {
      var focused = matrixConfig.previousSelector(component).orThunk(function () {
        var selectors = matrixConfig.selectors;
        return descendant(component.element, selectors.cell);
      });
      focused.each(function (cell) {
        matrixConfig.focusManager.set(component, cell);
      });
    };
    var execute$1 = function (component, simulatedEvent, matrixConfig) {
      return search(component.element).bind(function (focused) {
        return matrixConfig.execute(component, simulatedEvent, focused);
      });
    };
    var toMatrix = function (rows, matrixConfig) {
      return map$2(rows, function (row) {
        return descendants(row, matrixConfig.selectors.cell);
      });
    };
    var doMove = function (ifCycle, ifMove) {
      return function (element, focused, matrixConfig) {
        var move = matrixConfig.cycles ? ifCycle : ifMove;
        return closest$1(focused, matrixConfig.selectors.row).bind(function (inRow) {
          var cellsInRow = descendants(inRow, matrixConfig.selectors.cell);
          return findIndex(cellsInRow, focused).bind(function (colIndex) {
            var allRows = descendants(element, matrixConfig.selectors.row);
            return findIndex(allRows, inRow).bind(function (rowIndex) {
              var matrix = toMatrix(allRows, matrixConfig);
              return move(matrix, rowIndex, colIndex).map(function (next) {
                return next.cell;
              });
            });
          });
        });
      };
    };
    var moveLeft = doMove(cycleLeft, moveLeft$1);
    var moveRight = doMove(cycleRight, moveRight$1);
    var moveNorth = doMove(cycleUp, moveUp$1);
    var moveSouth = doMove(cycleDown, moveDown$1);
    var getKeydownRules$2 = constant$1([
      rule(inSet(LEFT), west(moveLeft, moveRight)),
      rule(inSet(RIGHT), east(moveLeft, moveRight)),
      rule(inSet(UP), north(moveNorth)),
      rule(inSet(DOWN), south(moveSouth)),
      rule(inSet(SPACE.concat(ENTER)), execute$1)
    ]);
    var getKeyupRules$2 = constant$1([rule(inSet(SPACE), stopEventForFirefox)]);
    var MatrixType = typical(schema$c, NoState.init, getKeydownRules$2, getKeyupRules$2, function () {
      return Optional.some(focusIn$1);
    });

    var schema$b = [
      required$1('selector'),
      defaulted('execute', defaultExecute),
      defaulted('moveOnTab', false)
    ];
    var execute = function (component, simulatedEvent, menuConfig) {
      return menuConfig.focusManager.get(component).bind(function (focused) {
        return menuConfig.execute(component, simulatedEvent, focused);
      });
    };
    var focusIn = function (component, menuConfig, _state) {
      descendant(component.element, menuConfig.selector).each(function (first) {
        menuConfig.focusManager.set(component, first);
      });
    };
    var moveUp = function (element, focused, info) {
      return horizontal(element, info.selector, focused, -1);
    };
    var moveDown = function (element, focused, info) {
      return horizontal(element, info.selector, focused, +1);
    };
    var fireShiftTab = function (component, simulatedEvent, menuConfig, menuState) {
      return menuConfig.moveOnTab ? move$1(moveUp)(component, simulatedEvent, menuConfig, menuState) : Optional.none();
    };
    var fireTab = function (component, simulatedEvent, menuConfig, menuState) {
      return menuConfig.moveOnTab ? move$1(moveDown)(component, simulatedEvent, menuConfig, menuState) : Optional.none();
    };
    var getKeydownRules$1 = constant$1([
      rule(inSet(UP), move$1(moveUp)),
      rule(inSet(DOWN), move$1(moveDown)),
      rule(and([
        isShift,
        inSet(TAB)
      ]), fireShiftTab),
      rule(and([
        isNotShift,
        inSet(TAB)
      ]), fireTab),
      rule(inSet(ENTER), execute),
      rule(inSet(SPACE), execute)
    ]);
    var getKeyupRules$1 = constant$1([rule(inSet(SPACE), stopEventForFirefox)]);
    var MenuType = typical(schema$b, NoState.init, getKeydownRules$1, getKeyupRules$1, function () {
      return Optional.some(focusIn);
    });

    var schema$a = [
      onKeyboardHandler('onSpace'),
      onKeyboardHandler('onEnter'),
      onKeyboardHandler('onShiftEnter'),
      onKeyboardHandler('onLeft'),
      onKeyboardHandler('onRight'),
      onKeyboardHandler('onTab'),
      onKeyboardHandler('onShiftTab'),
      onKeyboardHandler('onUp'),
      onKeyboardHandler('onDown'),
      onKeyboardHandler('onEscape'),
      defaulted('stopSpaceKeyup', false),
      option('focusIn')
    ];
    var getKeydownRules = function (component, simulatedEvent, specialInfo) {
      return [
        rule(inSet(SPACE), specialInfo.onSpace),
        rule(and([
          isNotShift,
          inSet(ENTER)
        ]), specialInfo.onEnter),
        rule(and([
          isShift,
          inSet(ENTER)
        ]), specialInfo.onShiftEnter),
        rule(and([
          isShift,
          inSet(TAB)
        ]), specialInfo.onShiftTab),
        rule(and([
          isNotShift,
          inSet(TAB)
        ]), specialInfo.onTab),
        rule(inSet(UP), specialInfo.onUp),
        rule(inSet(DOWN), specialInfo.onDown),
        rule(inSet(LEFT), specialInfo.onLeft),
        rule(inSet(RIGHT), specialInfo.onRight),
        rule(inSet(SPACE), specialInfo.onSpace),
        rule(inSet(ESCAPE), specialInfo.onEscape)
      ];
    };
    var getKeyupRules = function (component, simulatedEvent, specialInfo) {
      return specialInfo.stopSpaceKeyup ? [rule(inSet(SPACE), stopEventForFirefox)] : [];
    };
    var SpecialType = typical(schema$a, NoState.init, getKeydownRules, getKeyupRules, function (specialInfo) {
      return specialInfo.focusIn;
    });

    var acyclic = AcyclicType.schema();
    var cyclic = CyclicType.schema();
    var flow = FlowType.schema();
    var flatgrid = FlatgridType.schema();
    var matrix = MatrixType.schema();
    var execution = ExecutionType.schema();
    var menu = MenuType.schema();
    var special = SpecialType.schema();

    var KeyboardBranches = /*#__PURE__*/Object.freeze({
        __proto__: null,
        acyclic: acyclic,
        cyclic: cyclic,
        flow: flow,
        flatgrid: flatgrid,
        matrix: matrix,
        execution: execution,
        menu: menu,
        special: special
    });

    var isFlatgridState = function (keyState) {
      return hasNonNullableKey(keyState, 'setGridSize');
    };
    var Keying = createModes({
      branchKey: 'mode',
      branches: KeyboardBranches,
      name: 'keying',
      active: {
        events: function (keyingConfig, keyingState) {
          var handler = keyingConfig.handler;
          return handler.toEvents(keyingConfig, keyingState);
        }
      },
      apis: {
        focusIn: function (component, keyConfig, keyState) {
          keyConfig.sendFocusIn(keyConfig).fold(function () {
            component.getSystem().triggerFocus(component.element, component.element);
          }, function (sendFocusIn) {
            sendFocusIn(component, keyConfig, keyState);
          });
        },
        setGridSize: function (component, keyConfig, keyState, numRows, numColumns) {
          if (!isFlatgridState(keyState)) {
            console.error('Layout does not support setGridSize');
          } else {
            keyState.setGridSize(numRows, numColumns);
          }
        }
      },
      state: KeyingState
    });

    var field$1 = function (name, forbidden) {
      return defaultedObjOf(name, {}, map$2(forbidden, function (f) {
        return forbid(f.name(), 'Cannot configure ' + f.name() + ' for ' + name);
      }).concat([customField('dump', identity)]));
    };
    var get$6 = function (data) {
      return data.dump;
    };
    var augment = function (data, original) {
      return __assign(__assign({}, derive$2(original)), data.dump);
    };
    var SketchBehaviours = {
      field: field$1,
      augment: augment,
      get: get$6
    };

    var _placeholder = 'placeholder';
    var adt$5 = Adt.generate([
      {
        single: [
          'required',
          'valueThunk'
        ]
      },
      {
        multiple: [
          'required',
          'valueThunks'
        ]
      }
    ]);
    var isSubstituted = function (spec) {
      return has$2(spec, 'uiType');
    };
    var subPlaceholder = function (owner, detail, compSpec, placeholders) {
      if (owner.exists(function (o) {
          return o !== compSpec.owner;
        })) {
        return adt$5.single(true, constant$1(compSpec));
      }
      return get$c(placeholders, compSpec.name).fold(function () {
        throw new Error('Unknown placeholder component: ' + compSpec.name + '\nKnown: [' + keys(placeholders) + ']\nNamespace: ' + owner.getOr('none') + '\nSpec: ' + JSON.stringify(compSpec, null, 2));
      }, function (newSpec) {
        return newSpec.replace();
      });
    };
    var scan = function (owner, detail, compSpec, placeholders) {
      if (isSubstituted(compSpec) && compSpec.uiType === _placeholder) {
        return subPlaceholder(owner, detail, compSpec, placeholders);
      } else {
        return adt$5.single(false, constant$1(compSpec));
      }
    };
    var substitute = function (owner, detail, compSpec, placeholders) {
      var base = scan(owner, detail, compSpec, placeholders);
      return base.fold(function (req, valueThunk) {
        var value = isSubstituted(compSpec) ? valueThunk(detail, compSpec.config, compSpec.validated) : valueThunk(detail);
        var childSpecs = get$c(value, 'components').getOr([]);
        var substituted = bind$3(childSpecs, function (c) {
          return substitute(owner, detail, c, placeholders);
        });
        return [__assign(__assign({}, value), { components: substituted })];
      }, function (req, valuesThunk) {
        if (isSubstituted(compSpec)) {
          var values = valuesThunk(detail, compSpec.config, compSpec.validated);
          var preprocessor = compSpec.validated.preprocess.getOr(identity);
          return preprocessor(values);
        } else {
          return valuesThunk(detail);
        }
      });
    };
    var substituteAll = function (owner, detail, components, placeholders) {
      return bind$3(components, function (c) {
        return substitute(owner, detail, c, placeholders);
      });
    };
    var oneReplace = function (label, replacements) {
      var called = false;
      var used = function () {
        return called;
      };
      var replace = function () {
        if (called) {
          throw new Error('Trying to use the same placeholder more than once: ' + label);
        }
        called = true;
        return replacements;
      };
      var required = function () {
        return replacements.fold(function (req, _) {
          return req;
        }, function (req, _) {
          return req;
        });
      };
      return {
        name: constant$1(label),
        required: required,
        used: used,
        replace: replace
      };
    };
    var substitutePlaces = function (owner, detail, components, placeholders) {
      var ps = map$1(placeholders, function (ph, name) {
        return oneReplace(name, ph);
      });
      var outcome = substituteAll(owner, detail, components, ps);
      each(ps, function (p) {
        if (p.used() === false && p.required()) {
          throw new Error('Placeholder: ' + p.name() + ' was not found in components list\nNamespace: ' + owner.getOr('none') + '\nComponents: ' + JSON.stringify(detail.components, null, 2));
        }
      });
      return outcome;
    };
    var single$2 = adt$5.single;
    var multiple = adt$5.multiple;
    var placeholder = constant$1(_placeholder);

    var unique = 0;
    var generate$4 = function (prefix) {
      var date = new Date();
      var time = date.getTime();
      var random = Math.floor(Math.random() * 1000000000);
      unique++;
      return prefix + '_' + random + unique + String(time);
    };

    var adt$4 = Adt.generate([
      { required: ['data'] },
      { external: ['data'] },
      { optional: ['data'] },
      { group: ['data'] }
    ]);
    var fFactory = defaulted('factory', { sketch: identity });
    var fSchema = defaulted('schema', []);
    var fName = required$1('name');
    var fPname = field$2('pname', 'pname', defaultedThunk(function (typeSpec) {
      return '<alloy.' + generate$4(typeSpec.name) + '>';
    }), anyValue());
    var fGroupSchema = customField('schema', function () {
      return [option('preprocess')];
    });
    var fDefaults = defaulted('defaults', constant$1({}));
    var fOverrides = defaulted('overrides', constant$1({}));
    var requiredSpec = objOf([
      fFactory,
      fSchema,
      fName,
      fPname,
      fDefaults,
      fOverrides
    ]);
    var externalSpec = objOf([
      fFactory,
      fSchema,
      fName,
      fDefaults,
      fOverrides
    ]);
    var optionalSpec = objOf([
      fFactory,
      fSchema,
      fName,
      fPname,
      fDefaults,
      fOverrides
    ]);
    var groupSpec = objOf([
      fFactory,
      fGroupSchema,
      fName,
      required$1('unit'),
      fPname,
      fDefaults,
      fOverrides
    ]);
    var asNamedPart = function (part) {
      return part.fold(Optional.some, Optional.none, Optional.some, Optional.some);
    };
    var name = function (part) {
      var get = function (data) {
        return data.name;
      };
      return part.fold(get, get, get, get);
    };
    var convert$1 = function (adtConstructor, partSchema) {
      return function (spec) {
        var data = asRawOrDie$1('Converting part type', partSchema, spec);
        return adtConstructor(data);
      };
    };
    var required = convert$1(adt$4.required, requiredSpec);
    convert$1(adt$4.external, externalSpec);
    var optional = convert$1(adt$4.optional, optionalSpec);
    var group = convert$1(adt$4.group, groupSpec);
    var original = constant$1('entirety');

    var combine$2 = function (detail, data, partSpec, partValidated) {
      return deepMerge(data.defaults(detail, partSpec, partValidated), partSpec, { uid: detail.partUids[data.name] }, data.overrides(detail, partSpec, partValidated));
    };
    var subs = function (owner, detail, parts) {
      var internals = {};
      var externals = {};
      each$1(parts, function (part) {
        part.fold(function (data) {
          internals[data.pname] = single$2(true, function (detail, partSpec, partValidated) {
            return data.factory.sketch(combine$2(detail, data, partSpec, partValidated));
          });
        }, function (data) {
          var partSpec = detail.parts[data.name];
          externals[data.name] = constant$1(data.factory.sketch(combine$2(detail, data, partSpec[original()]), partSpec));
        }, function (data) {
          internals[data.pname] = single$2(false, function (detail, partSpec, partValidated) {
            return data.factory.sketch(combine$2(detail, data, partSpec, partValidated));
          });
        }, function (data) {
          internals[data.pname] = multiple(true, function (detail, _partSpec, _partValidated) {
            var units = detail[data.name];
            return map$2(units, function (u) {
              return data.factory.sketch(deepMerge(data.defaults(detail, u, _partValidated), u, data.overrides(detail, u)));
            });
          });
        });
      });
      return {
        internals: constant$1(internals),
        externals: constant$1(externals)
      };
    };

    var generate$3 = function (owner, parts) {
      var r = {};
      each$1(parts, function (part) {
        asNamedPart(part).each(function (np) {
          var g = doGenerateOne(owner, np.pname);
          r[np.name] = function (config) {
            var validated = asRawOrDie$1('Part: ' + np.name + ' in ' + owner, objOf(np.schema), config);
            return __assign(__assign({}, g), {
              config: config,
              validated: validated
            });
          };
        });
      });
      return r;
    };
    var doGenerateOne = function (owner, pname) {
      return {
        uiType: placeholder(),
        owner: owner,
        name: pname
      };
    };
    var generateOne = function (owner, pname, config) {
      return {
        uiType: placeholder(),
        owner: owner,
        name: pname,
        config: config,
        validated: {}
      };
    };
    var schemas = function (parts) {
      return bind$3(parts, function (part) {
        return part.fold(Optional.none, Optional.some, Optional.none, Optional.none).map(function (data) {
          return requiredObjOf(data.name, data.schema.concat([snapshot(original())]));
        }).toArray();
      });
    };
    var names = function (parts) {
      return map$2(parts, name);
    };
    var substitutes = function (owner, detail, parts) {
      return subs(owner, detail, parts);
    };
    var components = function (owner, detail, internals) {
      return substitutePlaces(Optional.some(owner), detail, detail.components, internals);
    };
    var getPart = function (component, detail, partKey) {
      var uid = detail.partUids[partKey];
      return component.getSystem().getByUid(uid).toOptional();
    };
    var getPartOrDie = function (component, detail, partKey) {
      return getPart(component, detail, partKey).getOrDie('Could not find part: ' + partKey);
    };
    var getAllParts = function (component, detail) {
      var system = component.getSystem();
      return map$1(detail.partUids, function (pUid, _k) {
        return constant$1(system.getByUid(pUid));
      });
    };
    var defaultUids = function (baseUid, partTypes) {
      var partNames = names(partTypes);
      return wrapAll(map$2(partNames, function (pn) {
        return {
          key: pn,
          value: baseUid + '-' + pn
        };
      }));
    };
    var defaultUidsSchema = function (partTypes) {
      return field$2('partUids', 'partUids', mergeWithThunk(function (spec) {
        return defaultUids(spec.uid, partTypes);
      }), anyValue());
    };

    var premadeTag = generate$4('alloy-premade');
    var premade$1 = function (comp) {
      return wrap(premadeTag, comp);
    };
    var getPremade = function (spec) {
      return get$c(spec, premadeTag);
    };
    var makeApi = function (f) {
      return markAsSketchApi(function (component) {
        var rest = [];
        for (var _i = 1; _i < arguments.length; _i++) {
          rest[_i - 1] = arguments[_i];
        }
        return f.apply(void 0, __spreadArray([
          component.getApis(),
          component
        ], rest, false));
      }, f);
    };

    var prefix$1 = constant$1('alloy-id-');
    var idAttr$1 = constant$1('data-alloy-id');

    var prefix = prefix$1();
    var idAttr = idAttr$1();
    var write = function (label, elem) {
      var id = generate$4(prefix + label);
      writeOnly(elem, id);
      return id;
    };
    var writeOnly = function (elem, uid) {
      Object.defineProperty(elem.dom, idAttr, {
        value: uid,
        writable: true
      });
    };
    var read = function (elem) {
      var id = isElement(elem) ? elem.dom[idAttr] : null;
      return Optional.from(id);
    };
    var generate$2 = function (prefix) {
      return generate$4(prefix);
    };

    var base = function (partSchemas, partUidsSchemas) {
      var ps = partSchemas.length > 0 ? [requiredObjOf('parts', partSchemas)] : [];
      return ps.concat([
        required$1('uid'),
        defaulted('dom', {}),
        defaulted('components', []),
        snapshot('originalSpec'),
        defaulted('debug.sketcher', {})
      ]).concat(partUidsSchemas);
    };
    var asRawOrDie = function (label, schema, spec, partSchemas, partUidsSchemas) {
      var baseS = base(partSchemas, partUidsSchemas);
      return asRawOrDie$1(label + ' [SpecSchema]', objOfOnly(baseS.concat(schema)), spec);
    };

    var single$1 = function (owner, schema, factory, spec) {
      var specWithUid = supplyUid(spec);
      var detail = asRawOrDie(owner, schema, specWithUid, [], []);
      return factory(detail, specWithUid);
    };
    var composite$1 = function (owner, schema, partTypes, factory, spec) {
      var specWithUid = supplyUid(spec);
      var partSchemas = schemas(partTypes);
      var partUidsSchema = defaultUidsSchema(partTypes);
      var detail = asRawOrDie(owner, schema, specWithUid, partSchemas, [partUidsSchema]);
      var subs = substitutes(owner, detail, partTypes);
      var components$1 = components(owner, detail, subs.internals());
      return factory(detail, components$1, specWithUid, subs.externals());
    };
    var hasUid = function (spec) {
      return has$2(spec, 'uid');
    };
    var supplyUid = function (spec) {
      return hasUid(spec) ? spec : __assign(__assign({}, spec), { uid: generate$2('uid') });
    };

    var isSketchSpec$1 = function (spec) {
      return spec.uid !== undefined;
    };
    var singleSchema = objOfOnly([
      required$1('name'),
      required$1('factory'),
      required$1('configFields'),
      defaulted('apis', {}),
      defaulted('extraApis', {})
    ]);
    var compositeSchema = objOfOnly([
      required$1('name'),
      required$1('factory'),
      required$1('configFields'),
      required$1('partFields'),
      defaulted('apis', {}),
      defaulted('extraApis', {})
    ]);
    var single = function (rawConfig) {
      var config = asRawOrDie$1('Sketcher for ' + rawConfig.name, singleSchema, rawConfig);
      var sketch = function (spec) {
        return single$1(config.name, config.configFields, config.factory, spec);
      };
      var apis = map$1(config.apis, makeApi);
      var extraApis = map$1(config.extraApis, function (f, k) {
        return markAsExtraApi(f, k);
      });
      return __assign(__assign({
        name: config.name,
        configFields: config.configFields,
        sketch: sketch
      }, apis), extraApis);
    };
    var composite = function (rawConfig) {
      var config = asRawOrDie$1('Sketcher for ' + rawConfig.name, compositeSchema, rawConfig);
      var sketch = function (spec) {
        return composite$1(config.name, config.configFields, config.partFields, config.factory, spec);
      };
      var parts = generate$3(config.name, config.partFields);
      var apis = map$1(config.apis, makeApi);
      var extraApis = map$1(config.extraApis, function (f, k) {
        return markAsExtraApi(f, k);
      });
      return __assign(__assign({
        name: config.name,
        partFields: config.partFields,
        configFields: config.configFields,
        sketch: sketch,
        parts: parts
      }, apis), extraApis);
    };

    var factory$5 = function (detail) {
      var events = events$8(detail.action);
      var tag = detail.dom.tag;
      var lookupAttr = function (attr) {
        return get$c(detail.dom, 'attributes').bind(function (attrs) {
          return get$c(attrs, attr);
        });
      };
      var getModAttributes = function () {
        if (tag === 'button') {
          var type = lookupAttr('type').getOr('button');
          var roleAttrs = lookupAttr('role').map(function (role) {
            return { role: role };
          }).getOr({});
          return __assign({ type: type }, roleAttrs);
        } else {
          var role = lookupAttr('role').getOr('button');
          return { role: role };
        }
      };
      return {
        uid: detail.uid,
        dom: detail.dom,
        components: detail.components,
        events: events,
        behaviours: SketchBehaviours.augment(detail.buttonBehaviours, [
          Focusing.config({}),
          Keying.config({
            mode: 'execution',
            useSpace: true,
            useEnter: true
          })
        ]),
        domModification: { attributes: getModAttributes() },
        eventOrder: detail.eventOrder
      };
    };
    var Button = single({
      name: 'Button',
      factory: factory$5,
      configFields: [
        defaulted('uid', undefined),
        required$1('dom'),
        defaulted('components', []),
        SketchBehaviours.field('buttonBehaviours', [
          Focusing,
          Keying
        ]),
        option('action'),
        option('role'),
        defaulted('eventOrder', {})
      ]
    });

    var exhibit$3 = function () {
      return nu$3({
        styles: {
          '-webkit-user-select': 'none',
          'user-select': 'none',
          '-ms-user-select': 'none',
          '-moz-user-select': '-moz-none'
        },
        attributes: { unselectable: 'on' }
      });
    };
    var events$6 = function () {
      return derive$3([abort(selectstart(), always)]);
    };

    var ActiveUnselecting = /*#__PURE__*/Object.freeze({
        __proto__: null,
        events: events$6,
        exhibit: exhibit$3
    });

    var Unselecting = create$5({
      fields: [],
      name: 'unselecting',
      active: ActiveUnselecting
    });

    var getAttrs$1 = function (elem) {
      var attributes = elem.dom.attributes !== undefined ? elem.dom.attributes : [];
      return foldl(attributes, function (b, attr) {
        var _a;
        if (attr.name === 'class') {
          return b;
        } else {
          return __assign(__assign({}, b), (_a = {}, _a[attr.name] = attr.value, _a));
        }
      }, {});
    };
    var getClasses = function (elem) {
      return Array.prototype.slice.call(elem.dom.classList, 0);
    };
    var fromHtml = function (html) {
      var elem = SugarElement.fromHtml(html);
      var children$1 = children(elem);
      var attrs = getAttrs$1(elem);
      var classes = getClasses(elem);
      var contents = children$1.length === 0 ? {} : { innerHtml: get$9(elem) };
      return __assign({
        tag: name$1(elem),
        classes: classes,
        attributes: attrs
      }, contents);
    };

    var dom$1 = function (rawHtml) {
      var html = supplant(rawHtml, { prefix: prefix$2 });
      return fromHtml(html);
    };
    var spec = function (rawHtml) {
      return { dom: dom$1(rawHtml) };
    };

    var forToolbarCommand = function (editor, command) {
      return forToolbar(command, function () {
        editor.execCommand(command);
      }, {}, editor);
    };
    var getToggleBehaviours = function (command) {
      return derive$2([
        Toggling.config({
          toggleClass: resolve('toolbar-button-selected'),
          toggleOnExecute: false,
          aria: { mode: 'pressed' }
        }),
        format(command, function (button, status) {
          var toggle = status ? Toggling.on : Toggling.off;
          toggle(button);
        })
      ]);
    };
    var forToolbarStateCommand = function (editor, command) {
      var extraBehaviours = getToggleBehaviours(command);
      return forToolbar(command, function () {
        editor.execCommand(command);
      }, extraBehaviours, editor);
    };
    var forToolbarStateAction = function (editor, clazz, command, action) {
      var extraBehaviours = getToggleBehaviours(command);
      return forToolbar(clazz, action, extraBehaviours, editor);
    };
    var getToolbarIconButton = function (clazz, editor) {
      var icons = editor.ui.registry.getAll().icons;
      var optOxideIcon = Optional.from(icons[clazz]);
      return optOxideIcon.fold(function () {
        return dom$1('<span class="${prefix}-toolbar-button ${prefix}-toolbar-group-item ${prefix}-icon-' + clazz + ' ${prefix}-icon"></span>');
      }, function (icon) {
        return dom$1('<span class="${prefix}-toolbar-button ${prefix}-toolbar-group-item">' + icon + '</span>');
      });
    };
    var forToolbar = function (clazz, action, extraBehaviours, editor) {
      return Button.sketch({
        dom: getToolbarIconButton(clazz, editor),
        action: action,
        buttonBehaviours: deepMerge(derive$2([Unselecting.config({})]), extraBehaviours)
      });
    };

    var labelPart = optional({
      schema: [required$1('dom')],
      name: 'label'
    });
    var edgePart = function (name) {
      return optional({
        name: '' + name + '-edge',
        overrides: function (detail) {
          var action = detail.model.manager.edgeActions[name];
          return action.fold(function () {
            return {};
          }, function (a) {
            return {
              events: derive$3([
                runActionExtra(touchstart(), function (comp, se, d) {
                  return a(comp, d);
                }, [detail]),
                runActionExtra(mousedown(), function (comp, se, d) {
                  return a(comp, d);
                }, [detail]),
                runActionExtra(mousemove(), function (comp, se, det) {
                  if (det.mouseIsDown.get()) {
                    a(comp, det);
                  }
                }, [detail])
              ])
            };
          });
        }
      });
    };
    var tlEdgePart = edgePart('top-left');
    var tedgePart = edgePart('top');
    var trEdgePart = edgePart('top-right');
    var redgePart = edgePart('right');
    var brEdgePart = edgePart('bottom-right');
    var bedgePart = edgePart('bottom');
    var blEdgePart = edgePart('bottom-left');
    var ledgePart = edgePart('left');
    var thumbPart = required({
      name: 'thumb',
      defaults: constant$1({ dom: { styles: { position: 'absolute' } } }),
      overrides: function (detail) {
        return {
          events: derive$3([
            redirectToPart(touchstart(), detail, 'spectrum'),
            redirectToPart(touchmove(), detail, 'spectrum'),
            redirectToPart(touchend(), detail, 'spectrum'),
            redirectToPart(mousedown(), detail, 'spectrum'),
            redirectToPart(mousemove(), detail, 'spectrum'),
            redirectToPart(mouseup(), detail, 'spectrum')
          ])
        };
      }
    });
    var spectrumPart = required({
      schema: [customField('mouseIsDown', function () {
          return Cell(false);
        })],
      name: 'spectrum',
      overrides: function (detail) {
        var modelDetail = detail.model;
        var model = modelDetail.manager;
        var setValueFrom = function (component, simulatedEvent) {
          return model.getValueFromEvent(simulatedEvent).map(function (value) {
            return model.setValueFrom(component, detail, value);
          });
        };
        return {
          behaviours: derive$2([
            Keying.config({
              mode: 'special',
              onLeft: function (spectrum) {
                return model.onLeft(spectrum, detail);
              },
              onRight: function (spectrum) {
                return model.onRight(spectrum, detail);
              },
              onUp: function (spectrum) {
                return model.onUp(spectrum, detail);
              },
              onDown: function (spectrum) {
                return model.onDown(spectrum, detail);
              }
            }),
            Focusing.config({})
          ]),
          events: derive$3([
            run(touchstart(), setValueFrom),
            run(touchmove(), setValueFrom),
            run(mousedown(), setValueFrom),
            run(mousemove(), function (spectrum, se) {
              if (detail.mouseIsDown.get()) {
                setValueFrom(spectrum, se);
              }
            })
          ])
        };
      }
    });
    var SliderParts = [
      labelPart,
      ledgePart,
      redgePart,
      tedgePart,
      bedgePart,
      tlEdgePart,
      trEdgePart,
      blEdgePart,
      brEdgePart,
      thumbPart,
      spectrumPart
    ];

    var onLoad$4 = function (component, repConfig, repState) {
      repConfig.store.manager.onLoad(component, repConfig, repState);
    };
    var onUnload$2 = function (component, repConfig, repState) {
      repConfig.store.manager.onUnload(component, repConfig, repState);
    };
    var setValue$3 = function (component, repConfig, repState, data) {
      repConfig.store.manager.setValue(component, repConfig, repState, data);
    };
    var getValue$4 = function (component, repConfig, repState) {
      return repConfig.store.manager.getValue(component, repConfig, repState);
    };
    var getState$1 = function (component, repConfig, repState) {
      return repState;
    };

    var RepresentApis = /*#__PURE__*/Object.freeze({
        __proto__: null,
        onLoad: onLoad$4,
        onUnload: onUnload$2,
        setValue: setValue$3,
        getValue: getValue$4,
        getState: getState$1
    });

    var events$5 = function (repConfig, repState) {
      var es = repConfig.resetOnDom ? [
        runOnAttached(function (comp, _se) {
          onLoad$4(comp, repConfig, repState);
        }),
        runOnDetached(function (comp, _se) {
          onUnload$2(comp, repConfig, repState);
        })
      ] : [loadEvent(repConfig, repState, onLoad$4)];
      return derive$3(es);
    };

    var ActiveRepresenting = /*#__PURE__*/Object.freeze({
        __proto__: null,
        events: events$5
    });

    var memory = function () {
      var data = Cell(null);
      var readState = function () {
        return {
          mode: 'memory',
          value: data.get()
        };
      };
      var isNotSet = function () {
        return data.get() === null;
      };
      var clear = function () {
        data.set(null);
      };
      return nu$2({
        set: data.set,
        get: data.get,
        isNotSet: isNotSet,
        clear: clear,
        readState: readState
      });
    };
    var manual = function () {
      var readState = noop;
      return nu$2({ readState: readState });
    };
    var dataset = function () {
      var dataByValue = Cell({});
      var dataByText = Cell({});
      var readState = function () {
        return {
          mode: 'dataset',
          dataByValue: dataByValue.get(),
          dataByText: dataByText.get()
        };
      };
      var clear = function () {
        dataByValue.set({});
        dataByText.set({});
      };
      var lookup = function (itemString) {
        return get$c(dataByValue.get(), itemString).orThunk(function () {
          return get$c(dataByText.get(), itemString);
        });
      };
      var update = function (items) {
        var currentDataByValue = dataByValue.get();
        var currentDataByText = dataByText.get();
        var newDataByValue = {};
        var newDataByText = {};
        each$1(items, function (item) {
          newDataByValue[item.value] = item;
          get$c(item, 'meta').each(function (meta) {
            get$c(meta, 'text').each(function (text) {
              newDataByText[text] = item;
            });
          });
        });
        dataByValue.set(__assign(__assign({}, currentDataByValue), newDataByValue));
        dataByText.set(__assign(__assign({}, currentDataByText), newDataByText));
      };
      return nu$2({
        readState: readState,
        lookup: lookup,
        update: update,
        clear: clear
      });
    };
    var init$4 = function (spec) {
      return spec.store.manager.state(spec);
    };

    var RepresentState = /*#__PURE__*/Object.freeze({
        __proto__: null,
        memory: memory,
        dataset: dataset,
        manual: manual,
        init: init$4
    });

    var setValue$2 = function (component, repConfig, repState, data) {
      var store = repConfig.store;
      repState.update([data]);
      store.setValue(component, data);
      repConfig.onSetValue(component, data);
    };
    var getValue$3 = function (component, repConfig, repState) {
      var store = repConfig.store;
      var key = store.getDataKey(component);
      return repState.lookup(key).getOrThunk(function () {
        return store.getFallbackEntry(key);
      });
    };
    var onLoad$3 = function (component, repConfig, repState) {
      var store = repConfig.store;
      store.initialValue.each(function (data) {
        setValue$2(component, repConfig, repState, data);
      });
    };
    var onUnload$1 = function (component, repConfig, repState) {
      repState.clear();
    };
    var DatasetStore = [
      option('initialValue'),
      required$1('getFallbackEntry'),
      required$1('getDataKey'),
      required$1('setValue'),
      output('manager', {
        setValue: setValue$2,
        getValue: getValue$3,
        onLoad: onLoad$3,
        onUnload: onUnload$1,
        state: dataset
      })
    ];

    var getValue$2 = function (component, repConfig, _repState) {
      return repConfig.store.getValue(component);
    };
    var setValue$1 = function (component, repConfig, _repState, data) {
      repConfig.store.setValue(component, data);
      repConfig.onSetValue(component, data);
    };
    var onLoad$2 = function (component, repConfig, _repState) {
      repConfig.store.initialValue.each(function (data) {
        repConfig.store.setValue(component, data);
      });
    };
    var ManualStore = [
      required$1('getValue'),
      defaulted('setValue', noop),
      option('initialValue'),
      output('manager', {
        setValue: setValue$1,
        getValue: getValue$2,
        onLoad: onLoad$2,
        onUnload: noop,
        state: NoState.init
      })
    ];

    var setValue = function (component, repConfig, repState, data) {
      repState.set(data);
      repConfig.onSetValue(component, data);
    };
    var getValue$1 = function (component, repConfig, repState) {
      return repState.get();
    };
    var onLoad$1 = function (component, repConfig, repState) {
      repConfig.store.initialValue.each(function (initVal) {
        if (repState.isNotSet()) {
          repState.set(initVal);
        }
      });
    };
    var onUnload = function (component, repConfig, repState) {
      repState.clear();
    };
    var MemoryStore = [
      option('initialValue'),
      output('manager', {
        setValue: setValue,
        getValue: getValue$1,
        onLoad: onLoad$1,
        onUnload: onUnload,
        state: memory
      })
    ];

    var RepresentSchema = [
      defaultedOf('store', { mode: 'memory' }, choose$1('mode', {
        memory: MemoryStore,
        manual: ManualStore,
        dataset: DatasetStore
      })),
      onHandler('onSetValue'),
      defaulted('resetOnDom', false)
    ];

    var Representing = create$5({
      fields: RepresentSchema,
      name: 'representing',
      active: ActiveRepresenting,
      apis: RepresentApis,
      extra: {
        setValueFrom: function (component, source) {
          var value = Representing.getValue(source);
          Representing.setValue(component, value);
        }
      },
      state: RepresentState
    });

    var api$1 = Dimension('width', function (element) {
      return element.dom.offsetWidth;
    });
    var set$4 = function (element, h) {
      return api$1.set(element, h);
    };
    var get$5 = function (element) {
      return api$1.get(element);
    };

    var r$1 = function (left, top) {
      var translate = function (x, y) {
        return r$1(left + x, top + y);
      };
      return {
        left: left,
        top: top,
        translate: translate
      };
    };
    var SugarPosition = r$1;

    var _sliderChangeEvent = 'slider.change.value';
    var sliderChangeEvent = constant$1(_sliderChangeEvent);
    var isTouchEvent = function (evt) {
      return evt.type.indexOf('touch') !== -1;
    };
    var getEventSource = function (simulatedEvent) {
      var evt = simulatedEvent.event.raw;
      if (isTouchEvent(evt)) {
        var touchEvent = evt;
        return touchEvent.touches !== undefined && touchEvent.touches.length === 1 ? Optional.some(touchEvent.touches[0]).map(function (t) {
          return SugarPosition(t.clientX, t.clientY);
        }) : Optional.none();
      } else {
        var mouseEvent = evt;
        return mouseEvent.clientX !== undefined ? Optional.some(mouseEvent).map(function (me) {
          return SugarPosition(me.clientX, me.clientY);
        }) : Optional.none();
      }
    };

    var t = 'top', r = 'right', b = 'bottom', l = 'left';
    var minX = function (detail) {
      return detail.model.minX;
    };
    var minY = function (detail) {
      return detail.model.minY;
    };
    var min1X = function (detail) {
      return detail.model.minX - 1;
    };
    var min1Y = function (detail) {
      return detail.model.minY - 1;
    };
    var maxX = function (detail) {
      return detail.model.maxX;
    };
    var maxY = function (detail) {
      return detail.model.maxY;
    };
    var max1X = function (detail) {
      return detail.model.maxX + 1;
    };
    var max1Y = function (detail) {
      return detail.model.maxY + 1;
    };
    var range$1 = function (detail, max, min) {
      return max(detail) - min(detail);
    };
    var xRange = function (detail) {
      return range$1(detail, maxX, minX);
    };
    var yRange = function (detail) {
      return range$1(detail, maxY, minY);
    };
    var halfX = function (detail) {
      return xRange(detail) / 2;
    };
    var halfY = function (detail) {
      return yRange(detail) / 2;
    };
    var step = function (detail) {
      return detail.stepSize;
    };
    var snap = function (detail) {
      return detail.snapToGrid;
    };
    var snapStart = function (detail) {
      return detail.snapStart;
    };
    var rounded = function (detail) {
      return detail.rounded;
    };
    var hasEdge = function (detail, edgeName) {
      return detail[edgeName + '-edge'] !== undefined;
    };
    var hasLEdge = function (detail) {
      return hasEdge(detail, l);
    };
    var hasREdge = function (detail) {
      return hasEdge(detail, r);
    };
    var hasTEdge = function (detail) {
      return hasEdge(detail, t);
    };
    var hasBEdge = function (detail) {
      return hasEdge(detail, b);
    };
    var currentValue = function (detail) {
      return detail.model.value.get();
    };

    var xValue = function (x) {
      return { x: x };
    };
    var yValue = function (y) {
      return { y: y };
    };
    var xyValue = function (x, y) {
      return {
        x: x,
        y: y
      };
    };
    var fireSliderChange$3 = function (component, value) {
      emitWith(component, sliderChangeEvent(), { value: value });
    };
    var setToTLEdgeXY = function (edge, detail) {
      fireSliderChange$3(edge, xyValue(min1X(detail), min1Y(detail)));
    };
    var setToTEdge = function (edge, detail) {
      fireSliderChange$3(edge, yValue(min1Y(detail)));
    };
    var setToTEdgeXY = function (edge, detail) {
      fireSliderChange$3(edge, xyValue(halfX(detail), min1Y(detail)));
    };
    var setToTREdgeXY = function (edge, detail) {
      fireSliderChange$3(edge, xyValue(max1X(detail), min1Y(detail)));
    };
    var setToREdge = function (edge, detail) {
      fireSliderChange$3(edge, xValue(max1X(detail)));
    };
    var setToREdgeXY = function (edge, detail) {
      fireSliderChange$3(edge, xyValue(max1X(detail), halfY(detail)));
    };
    var setToBREdgeXY = function (edge, detail) {
      fireSliderChange$3(edge, xyValue(max1X(detail), max1Y(detail)));
    };
    var setToBEdge = function (edge, detail) {
      fireSliderChange$3(edge, yValue(max1Y(detail)));
    };
    var setToBEdgeXY = function (edge, detail) {
      fireSliderChange$3(edge, xyValue(halfX(detail), max1Y(detail)));
    };
    var setToBLEdgeXY = function (edge, detail) {
      fireSliderChange$3(edge, xyValue(min1X(detail), max1Y(detail)));
    };
    var setToLEdge = function (edge, detail) {
      fireSliderChange$3(edge, xValue(min1X(detail)));
    };
    var setToLEdgeXY = function (edge, detail) {
      fireSliderChange$3(edge, xyValue(min1X(detail), halfY(detail)));
    };

    var reduceBy = function (value, min, max, step) {
      if (value < min) {
        return value;
      } else if (value > max) {
        return max;
      } else if (value === min) {
        return min - 1;
      } else {
        return Math.max(min, value - step);
      }
    };
    var increaseBy = function (value, min, max, step) {
      if (value > max) {
        return value;
      } else if (value < min) {
        return min;
      } else if (value === max) {
        return max + 1;
      } else {
        return Math.min(max, value + step);
      }
    };
    var capValue = function (value, min, max) {
      return Math.max(min, Math.min(max, value));
    };
    var snapValueOf = function (value, min, max, step, snapStart) {
      return snapStart.fold(function () {
        var initValue = value - min;
        var extraValue = Math.round(initValue / step) * step;
        return capValue(min + extraValue, min - 1, max + 1);
      }, function (start) {
        var remainder = (value - start) % step;
        var adjustment = Math.round(remainder / step);
        var rawSteps = Math.floor((value - start) / step);
        var maxSteps = Math.floor((max - start) / step);
        var numSteps = Math.min(maxSteps, rawSteps + adjustment);
        var r = start + numSteps * step;
        return Math.max(start, r);
      });
    };
    var findOffsetOf = function (value, min, max) {
      return Math.min(max, Math.max(value, min)) - min;
    };
    var findValueOf = function (args) {
      var min = args.min, max = args.max, range = args.range, value = args.value, step = args.step, snap = args.snap, snapStart = args.snapStart, rounded = args.rounded, hasMinEdge = args.hasMinEdge, hasMaxEdge = args.hasMaxEdge, minBound = args.minBound, maxBound = args.maxBound, screenRange = args.screenRange;
      var capMin = hasMinEdge ? min - 1 : min;
      var capMax = hasMaxEdge ? max + 1 : max;
      if (value < minBound) {
        return capMin;
      } else if (value > maxBound) {
        return capMax;
      } else {
        var offset = findOffsetOf(value, minBound, maxBound);
        var newValue = capValue(offset / screenRange * range + min, capMin, capMax);
        if (snap && newValue >= min && newValue <= max) {
          return snapValueOf(newValue, min, max, step, snapStart);
        } else if (rounded) {
          return Math.round(newValue);
        } else {
          return newValue;
        }
      }
    };
    var findOffsetOfValue$2 = function (args) {
      var min = args.min, max = args.max, range = args.range, value = args.value, hasMinEdge = args.hasMinEdge, hasMaxEdge = args.hasMaxEdge, maxBound = args.maxBound, maxOffset = args.maxOffset, centerMinEdge = args.centerMinEdge, centerMaxEdge = args.centerMaxEdge;
      if (value < min) {
        return hasMinEdge ? 0 : centerMinEdge;
      } else if (value > max) {
        return hasMaxEdge ? maxBound : centerMaxEdge;
      } else {
        return (value - min) / range * maxOffset;
      }
    };

    var top = 'top', right = 'right', bottom = 'bottom', left = 'left', width = 'width', height = 'height';
    var getBounds$1 = function (component) {
      return component.element.dom.getBoundingClientRect();
    };
    var getBoundsProperty = function (bounds, property) {
      return bounds[property];
    };
    var getMinXBounds = function (component) {
      var bounds = getBounds$1(component);
      return getBoundsProperty(bounds, left);
    };
    var getMaxXBounds = function (component) {
      var bounds = getBounds$1(component);
      return getBoundsProperty(bounds, right);
    };
    var getMinYBounds = function (component) {
      var bounds = getBounds$1(component);
      return getBoundsProperty(bounds, top);
    };
    var getMaxYBounds = function (component) {
      var bounds = getBounds$1(component);
      return getBoundsProperty(bounds, bottom);
    };
    var getXScreenRange = function (component) {
      var bounds = getBounds$1(component);
      return getBoundsProperty(bounds, width);
    };
    var getYScreenRange = function (component) {
      var bounds = getBounds$1(component);
      return getBoundsProperty(bounds, height);
    };
    var getCenterOffsetOf = function (componentMinEdge, componentMaxEdge, spectrumMinEdge) {
      return (componentMinEdge + componentMaxEdge) / 2 - spectrumMinEdge;
    };
    var getXCenterOffSetOf = function (component, spectrum) {
      var componentBounds = getBounds$1(component);
      var spectrumBounds = getBounds$1(spectrum);
      var componentMinEdge = getBoundsProperty(componentBounds, left);
      var componentMaxEdge = getBoundsProperty(componentBounds, right);
      var spectrumMinEdge = getBoundsProperty(spectrumBounds, left);
      return getCenterOffsetOf(componentMinEdge, componentMaxEdge, spectrumMinEdge);
    };
    var getYCenterOffSetOf = function (component, spectrum) {
      var componentBounds = getBounds$1(component);
      var spectrumBounds = getBounds$1(spectrum);
      var componentMinEdge = getBoundsProperty(componentBounds, top);
      var componentMaxEdge = getBoundsProperty(componentBounds, bottom);
      var spectrumMinEdge = getBoundsProperty(spectrumBounds, top);
      return getCenterOffsetOf(componentMinEdge, componentMaxEdge, spectrumMinEdge);
    };

    var fireSliderChange$2 = function (spectrum, value) {
      emitWith(spectrum, sliderChangeEvent(), { value: value });
    };
    var sliderValue$2 = function (x) {
      return { x: x };
    };
    var findValueOfOffset$1 = function (spectrum, detail, left) {
      var args = {
        min: minX(detail),
        max: maxX(detail),
        range: xRange(detail),
        value: left,
        step: step(detail),
        snap: snap(detail),
        snapStart: snapStart(detail),
        rounded: rounded(detail),
        hasMinEdge: hasLEdge(detail),
        hasMaxEdge: hasREdge(detail),
        minBound: getMinXBounds(spectrum),
        maxBound: getMaxXBounds(spectrum),
        screenRange: getXScreenRange(spectrum)
      };
      return findValueOf(args);
    };
    var setValueFrom$2 = function (spectrum, detail, value) {
      var xValue = findValueOfOffset$1(spectrum, detail, value);
      var sliderVal = sliderValue$2(xValue);
      fireSliderChange$2(spectrum, sliderVal);
      return xValue;
    };
    var setToMin$2 = function (spectrum, detail) {
      var min = minX(detail);
      fireSliderChange$2(spectrum, sliderValue$2(min));
    };
    var setToMax$2 = function (spectrum, detail) {
      var max = maxX(detail);
      fireSliderChange$2(spectrum, sliderValue$2(max));
    };
    var moveBy$2 = function (direction, spectrum, detail) {
      var f = direction > 0 ? increaseBy : reduceBy;
      var xValue = f(currentValue(detail).x, minX(detail), maxX(detail), step(detail));
      fireSliderChange$2(spectrum, sliderValue$2(xValue));
      return Optional.some(xValue);
    };
    var handleMovement$2 = function (direction) {
      return function (spectrum, detail) {
        return moveBy$2(direction, spectrum, detail).map(always);
      };
    };
    var getValueFromEvent$2 = function (simulatedEvent) {
      var pos = getEventSource(simulatedEvent);
      return pos.map(function (p) {
        return p.left;
      });
    };
    var findOffsetOfValue$1 = function (spectrum, detail, value, minEdge, maxEdge) {
      var minOffset = 0;
      var maxOffset = getXScreenRange(spectrum);
      var centerMinEdge = minEdge.bind(function (edge) {
        return Optional.some(getXCenterOffSetOf(edge, spectrum));
      }).getOr(minOffset);
      var centerMaxEdge = maxEdge.bind(function (edge) {
        return Optional.some(getXCenterOffSetOf(edge, spectrum));
      }).getOr(maxOffset);
      var args = {
        min: minX(detail),
        max: maxX(detail),
        range: xRange(detail),
        value: value,
        hasMinEdge: hasLEdge(detail),
        hasMaxEdge: hasREdge(detail),
        minBound: getMinXBounds(spectrum),
        minOffset: minOffset,
        maxBound: getMaxXBounds(spectrum),
        maxOffset: maxOffset,
        centerMinEdge: centerMinEdge,
        centerMaxEdge: centerMaxEdge
      };
      return findOffsetOfValue$2(args);
    };
    var findPositionOfValue$1 = function (slider, spectrum, value, minEdge, maxEdge, detail) {
      var offset = findOffsetOfValue$1(spectrum, detail, value, minEdge, maxEdge);
      return getMinXBounds(spectrum) - getMinXBounds(slider) + offset;
    };
    var setPositionFromValue$2 = function (slider, thumb, detail, edges) {
      var value = currentValue(detail);
      var pos = findPositionOfValue$1(slider, edges.getSpectrum(slider), value.x, edges.getLeftEdge(slider), edges.getRightEdge(slider), detail);
      var thumbRadius = get$5(thumb.element) / 2;
      set$5(thumb.element, 'left', pos - thumbRadius + 'px');
    };
    var onLeft$2 = handleMovement$2(-1);
    var onRight$2 = handleMovement$2(1);
    var onUp$2 = Optional.none;
    var onDown$2 = Optional.none;
    var edgeActions$2 = {
      'top-left': Optional.none(),
      'top': Optional.none(),
      'top-right': Optional.none(),
      'right': Optional.some(setToREdge),
      'bottom-right': Optional.none(),
      'bottom': Optional.none(),
      'bottom-left': Optional.none(),
      'left': Optional.some(setToLEdge)
    };

    var HorizontalModel = /*#__PURE__*/Object.freeze({
        __proto__: null,
        setValueFrom: setValueFrom$2,
        setToMin: setToMin$2,
        setToMax: setToMax$2,
        findValueOfOffset: findValueOfOffset$1,
        getValueFromEvent: getValueFromEvent$2,
        findPositionOfValue: findPositionOfValue$1,
        setPositionFromValue: setPositionFromValue$2,
        onLeft: onLeft$2,
        onRight: onRight$2,
        onUp: onUp$2,
        onDown: onDown$2,
        edgeActions: edgeActions$2
    });

    var fireSliderChange$1 = function (spectrum, value) {
      emitWith(spectrum, sliderChangeEvent(), { value: value });
    };
    var sliderValue$1 = function (y) {
      return { y: y };
    };
    var findValueOfOffset = function (spectrum, detail, top) {
      var args = {
        min: minY(detail),
        max: maxY(detail),
        range: yRange(detail),
        value: top,
        step: step(detail),
        snap: snap(detail),
        snapStart: snapStart(detail),
        rounded: rounded(detail),
        hasMinEdge: hasTEdge(detail),
        hasMaxEdge: hasBEdge(detail),
        minBound: getMinYBounds(spectrum),
        maxBound: getMaxYBounds(spectrum),
        screenRange: getYScreenRange(spectrum)
      };
      return findValueOf(args);
    };
    var setValueFrom$1 = function (spectrum, detail, value) {
      var yValue = findValueOfOffset(spectrum, detail, value);
      var sliderVal = sliderValue$1(yValue);
      fireSliderChange$1(spectrum, sliderVal);
      return yValue;
    };
    var setToMin$1 = function (spectrum, detail) {
      var min = minY(detail);
      fireSliderChange$1(spectrum, sliderValue$1(min));
    };
    var setToMax$1 = function (spectrum, detail) {
      var max = maxY(detail);
      fireSliderChange$1(spectrum, sliderValue$1(max));
    };
    var moveBy$1 = function (direction, spectrum, detail) {
      var f = direction > 0 ? increaseBy : reduceBy;
      var yValue = f(currentValue(detail).y, minY(detail), maxY(detail), step(detail));
      fireSliderChange$1(spectrum, sliderValue$1(yValue));
      return Optional.some(yValue);
    };
    var handleMovement$1 = function (direction) {
      return function (spectrum, detail) {
        return moveBy$1(direction, spectrum, detail).map(always);
      };
    };
    var getValueFromEvent$1 = function (simulatedEvent) {
      var pos = getEventSource(simulatedEvent);
      return pos.map(function (p) {
        return p.top;
      });
    };
    var findOffsetOfValue = function (spectrum, detail, value, minEdge, maxEdge) {
      var minOffset = 0;
      var maxOffset = getYScreenRange(spectrum);
      var centerMinEdge = minEdge.bind(function (edge) {
        return Optional.some(getYCenterOffSetOf(edge, spectrum));
      }).getOr(minOffset);
      var centerMaxEdge = maxEdge.bind(function (edge) {
        return Optional.some(getYCenterOffSetOf(edge, spectrum));
      }).getOr(maxOffset);
      var args = {
        min: minY(detail),
        max: maxY(detail),
        range: yRange(detail),
        value: value,
        hasMinEdge: hasTEdge(detail),
        hasMaxEdge: hasBEdge(detail),
        minBound: getMinYBounds(spectrum),
        minOffset: minOffset,
        maxBound: getMaxYBounds(spectrum),
        maxOffset: maxOffset,
        centerMinEdge: centerMinEdge,
        centerMaxEdge: centerMaxEdge
      };
      return findOffsetOfValue$2(args);
    };
    var findPositionOfValue = function (slider, spectrum, value, minEdge, maxEdge, detail) {
      var offset = findOffsetOfValue(spectrum, detail, value, minEdge, maxEdge);
      return getMinYBounds(spectrum) - getMinYBounds(slider) + offset;
    };
    var setPositionFromValue$1 = function (slider, thumb, detail, edges) {
      var value = currentValue(detail);
      var pos = findPositionOfValue(slider, edges.getSpectrum(slider), value.y, edges.getTopEdge(slider), edges.getBottomEdge(slider), detail);
      var thumbRadius = get$7(thumb.element) / 2;
      set$5(thumb.element, 'top', pos - thumbRadius + 'px');
    };
    var onLeft$1 = Optional.none;
    var onRight$1 = Optional.none;
    var onUp$1 = handleMovement$1(-1);
    var onDown$1 = handleMovement$1(1);
    var edgeActions$1 = {
      'top-left': Optional.none(),
      'top': Optional.some(setToTEdge),
      'top-right': Optional.none(),
      'right': Optional.none(),
      'bottom-right': Optional.none(),
      'bottom': Optional.some(setToBEdge),
      'bottom-left': Optional.none(),
      'left': Optional.none()
    };

    var VerticalModel = /*#__PURE__*/Object.freeze({
        __proto__: null,
        setValueFrom: setValueFrom$1,
        setToMin: setToMin$1,
        setToMax: setToMax$1,
        findValueOfOffset: findValueOfOffset,
        getValueFromEvent: getValueFromEvent$1,
        findPositionOfValue: findPositionOfValue,
        setPositionFromValue: setPositionFromValue$1,
        onLeft: onLeft$1,
        onRight: onRight$1,
        onUp: onUp$1,
        onDown: onDown$1,
        edgeActions: edgeActions$1
    });

    var fireSliderChange = function (spectrum, value) {
      emitWith(spectrum, sliderChangeEvent(), { value: value });
    };
    var sliderValue = function (x, y) {
      return {
        x: x,
        y: y
      };
    };
    var setValueFrom = function (spectrum, detail, value) {
      var xValue = findValueOfOffset$1(spectrum, detail, value.left);
      var yValue = findValueOfOffset(spectrum, detail, value.top);
      var val = sliderValue(xValue, yValue);
      fireSliderChange(spectrum, val);
      return val;
    };
    var moveBy = function (direction, isVerticalMovement, spectrum, detail) {
      var f = direction > 0 ? increaseBy : reduceBy;
      var xValue = isVerticalMovement ? currentValue(detail).x : f(currentValue(detail).x, minX(detail), maxX(detail), step(detail));
      var yValue = !isVerticalMovement ? currentValue(detail).y : f(currentValue(detail).y, minY(detail), maxY(detail), step(detail));
      fireSliderChange(spectrum, sliderValue(xValue, yValue));
      return Optional.some(xValue);
    };
    var handleMovement = function (direction, isVerticalMovement) {
      return function (spectrum, detail) {
        return moveBy(direction, isVerticalMovement, spectrum, detail).map(always);
      };
    };
    var setToMin = function (spectrum, detail) {
      var mX = minX(detail);
      var mY = minY(detail);
      fireSliderChange(spectrum, sliderValue(mX, mY));
    };
    var setToMax = function (spectrum, detail) {
      var mX = maxX(detail);
      var mY = maxY(detail);
      fireSliderChange(spectrum, sliderValue(mX, mY));
    };
    var getValueFromEvent = function (simulatedEvent) {
      return getEventSource(simulatedEvent);
    };
    var setPositionFromValue = function (slider, thumb, detail, edges) {
      var value = currentValue(detail);
      var xPos = findPositionOfValue$1(slider, edges.getSpectrum(slider), value.x, edges.getLeftEdge(slider), edges.getRightEdge(slider), detail);
      var yPos = findPositionOfValue(slider, edges.getSpectrum(slider), value.y, edges.getTopEdge(slider), edges.getBottomEdge(slider), detail);
      var thumbXRadius = get$5(thumb.element) / 2;
      var thumbYRadius = get$7(thumb.element) / 2;
      set$5(thumb.element, 'left', xPos - thumbXRadius + 'px');
      set$5(thumb.element, 'top', yPos - thumbYRadius + 'px');
    };
    var onLeft = handleMovement(-1, false);
    var onRight = handleMovement(1, false);
    var onUp = handleMovement(-1, true);
    var onDown = handleMovement(1, true);
    var edgeActions = {
      'top-left': Optional.some(setToTLEdgeXY),
      'top': Optional.some(setToTEdgeXY),
      'top-right': Optional.some(setToTREdgeXY),
      'right': Optional.some(setToREdgeXY),
      'bottom-right': Optional.some(setToBREdgeXY),
      'bottom': Optional.some(setToBEdgeXY),
      'bottom-left': Optional.some(setToBLEdgeXY),
      'left': Optional.some(setToLEdgeXY)
    };

    var TwoDModel = /*#__PURE__*/Object.freeze({
        __proto__: null,
        setValueFrom: setValueFrom,
        setToMin: setToMin,
        setToMax: setToMax,
        getValueFromEvent: getValueFromEvent,
        setPositionFromValue: setPositionFromValue,
        onLeft: onLeft,
        onRight: onRight,
        onUp: onUp,
        onDown: onDown,
        edgeActions: edgeActions
    });

    var SliderSchema = [
      defaulted('stepSize', 1),
      defaulted('onChange', noop),
      defaulted('onChoose', noop),
      defaulted('onInit', noop),
      defaulted('onDragStart', noop),
      defaulted('onDragEnd', noop),
      defaulted('snapToGrid', false),
      defaulted('rounded', true),
      option('snapStart'),
      requiredOf('model', choose$1('mode', {
        x: [
          defaulted('minX', 0),
          defaulted('maxX', 100),
          customField('value', function (spec) {
            return Cell(spec.mode.minX);
          }),
          required$1('getInitialValue'),
          output('manager', HorizontalModel)
        ],
        y: [
          defaulted('minY', 0),
          defaulted('maxY', 100),
          customField('value', function (spec) {
            return Cell(spec.mode.minY);
          }),
          required$1('getInitialValue'),
          output('manager', VerticalModel)
        ],
        xy: [
          defaulted('minX', 0),
          defaulted('maxX', 100),
          defaulted('minY', 0),
          defaulted('maxY', 100),
          customField('value', function (spec) {
            return Cell({
              x: spec.mode.minX,
              y: spec.mode.minY
            });
          }),
          required$1('getInitialValue'),
          output('manager', TwoDModel)
        ]
      })),
      field$1('sliderBehaviours', [
        Keying,
        Representing
      ]),
      customField('mouseIsDown', function () {
        return Cell(false);
      })
    ];

    var mouseReleased = constant$1('mouse.released');

    var sketch$9 = function (detail, components, _spec, _externals) {
      var _a;
      var getThumb = function (component) {
        return getPartOrDie(component, detail, 'thumb');
      };
      var getSpectrum = function (component) {
        return getPartOrDie(component, detail, 'spectrum');
      };
      var getLeftEdge = function (component) {
        return getPart(component, detail, 'left-edge');
      };
      var getRightEdge = function (component) {
        return getPart(component, detail, 'right-edge');
      };
      var getTopEdge = function (component) {
        return getPart(component, detail, 'top-edge');
      };
      var getBottomEdge = function (component) {
        return getPart(component, detail, 'bottom-edge');
      };
      var modelDetail = detail.model;
      var model = modelDetail.manager;
      var refresh = function (slider, thumb) {
        model.setPositionFromValue(slider, thumb, detail, {
          getLeftEdge: getLeftEdge,
          getRightEdge: getRightEdge,
          getTopEdge: getTopEdge,
          getBottomEdge: getBottomEdge,
          getSpectrum: getSpectrum
        });
      };
      var setValue = function (slider, newValue) {
        modelDetail.value.set(newValue);
        var thumb = getThumb(slider);
        refresh(slider, thumb);
      };
      var changeValue = function (slider, newValue) {
        setValue(slider, newValue);
        var thumb = getThumb(slider);
        detail.onChange(slider, thumb, newValue);
        return Optional.some(true);
      };
      var resetToMin = function (slider) {
        model.setToMin(slider, detail);
      };
      var resetToMax = function (slider) {
        model.setToMax(slider, detail);
      };
      var choose = function (slider) {
        var fireOnChoose = function () {
          getPart(slider, detail, 'thumb').each(function (thumb) {
            var value = modelDetail.value.get();
            detail.onChoose(slider, thumb, value);
          });
        };
        var wasDown = detail.mouseIsDown.get();
        detail.mouseIsDown.set(false);
        if (wasDown) {
          fireOnChoose();
        }
      };
      var onDragStart = function (slider, simulatedEvent) {
        simulatedEvent.stop();
        detail.mouseIsDown.set(true);
        detail.onDragStart(slider, getThumb(slider));
      };
      var onDragEnd = function (slider, simulatedEvent) {
        simulatedEvent.stop();
        detail.onDragEnd(slider, getThumb(slider));
        choose(slider);
      };
      return {
        uid: detail.uid,
        dom: detail.dom,
        components: components,
        behaviours: augment(detail.sliderBehaviours, [
          Keying.config({
            mode: 'special',
            focusIn: function (slider) {
              return getPart(slider, detail, 'spectrum').map(Keying.focusIn).map(always);
            }
          }),
          Representing.config({
            store: {
              mode: 'manual',
              getValue: function (_) {
                return modelDetail.value.get();
              }
            }
          }),
          Receiving.config({ channels: (_a = {}, _a[mouseReleased()] = { onReceive: choose }, _a) })
        ]),
        events: derive$3([
          run(sliderChangeEvent(), function (slider, simulatedEvent) {
            changeValue(slider, simulatedEvent.event.value);
          }),
          runOnAttached(function (slider, _simulatedEvent) {
            var getInitial = modelDetail.getInitialValue();
            modelDetail.value.set(getInitial);
            var thumb = getThumb(slider);
            refresh(slider, thumb);
            var spectrum = getSpectrum(slider);
            detail.onInit(slider, thumb, spectrum, modelDetail.value.get());
          }),
          run(touchstart(), onDragStart),
          run(touchend(), onDragEnd),
          run(mousedown(), onDragStart),
          run(mouseup(), onDragEnd)
        ]),
        apis: {
          resetToMin: resetToMin,
          resetToMax: resetToMax,
          setValue: setValue,
          refresh: refresh
        },
        domModification: { styles: { position: 'relative' } }
      };
    };

    var Slider = composite({
      name: 'Slider',
      configFields: SliderSchema,
      partFields: SliderParts,
      factory: sketch$9,
      apis: {
        setValue: function (apis, slider, value) {
          apis.setValue(slider, value);
        },
        resetToMin: function (apis, slider) {
          apis.resetToMin(slider);
        },
        resetToMax: function (apis, slider) {
          apis.resetToMax(slider);
        },
        refresh: function (apis, slider) {
          apis.refresh(slider);
        }
      }
    });

    var button = function (realm, clazz, makeItems, editor) {
      return forToolbar(clazz, function () {
        var items = makeItems();
        realm.setContextToolbar([{
            label: clazz + ' group',
            items: items
          }]);
      }, {}, editor);
    };

    var BLACK = -1;
    var makeSlider$1 = function (spec$1) {
      var getColor = function (hue) {
        if (hue < 0) {
          return 'black';
        } else if (hue > 360) {
          return 'white';
        } else {
          return 'hsl(' + hue + ', 100%, 50%)';
        }
      };
      var onInit = function (slider, thumb, spectrum, value) {
        var color = getColor(value.x());
        set$5(thumb.element, 'background-color', color);
      };
      var onChange = function (slider, thumb, value) {
        var color = getColor(value.x());
        set$5(thumb.element, 'background-color', color);
        spec$1.onChange(slider, thumb, color);
      };
      return Slider.sketch({
        dom: dom$1('<div class="${prefix}-slider ${prefix}-hue-slider-container"></div>'),
        components: [
          Slider.parts['left-edge'](spec('<div class="${prefix}-hue-slider-black"></div>')),
          Slider.parts.spectrum({
            dom: dom$1('<div class="${prefix}-slider-gradient-container"></div>'),
            components: [spec('<div class="${prefix}-slider-gradient"></div>')],
            behaviours: derive$2([Toggling.config({ toggleClass: resolve('thumb-active') })])
          }),
          Slider.parts['right-edge'](spec('<div class="${prefix}-hue-slider-white"></div>')),
          Slider.parts.thumb({
            dom: dom$1('<div class="${prefix}-slider-thumb"></div>'),
            behaviours: derive$2([Toggling.config({ toggleClass: resolve('thumb-active') })])
          })
        ],
        onChange: onChange,
        onDragStart: function (slider, thumb) {
          Toggling.on(thumb);
        },
        onDragEnd: function (slider, thumb) {
          Toggling.off(thumb);
        },
        onInit: onInit,
        stepSize: 10,
        model: {
          mode: 'x',
          minX: 0,
          maxX: 360,
          getInitialValue: function () {
            return { x: spec$1.getInitialValue() };
          }
        },
        sliderBehaviours: derive$2([orientation(Slider.refresh)])
      });
    };
    var makeItems$1 = function (spec) {
      return [makeSlider$1(spec)];
    };
    var sketch$8 = function (realm, editor) {
      var spec = {
        onChange: function (slider, thumb, color) {
          editor.undoManager.transact(function () {
            editor.formatter.apply('forecolor', { value: color });
            editor.nodeChanged();
          });
        },
        getInitialValue: constant$1(BLACK)
      };
      return button(realm, 'color-levels', function () {
        return makeItems$1(spec);
      }, editor);
    };

    var candidatesArray = [
      '9px',
      '10px',
      '11px',
      '12px',
      '14px',
      '16px',
      '18px',
      '20px',
      '24px',
      '32px',
      '36px'
    ];
    var defaultSize = 'medium';
    var defaultIndex = 2;
    var indexToSize = function (index) {
      return Optional.from(candidatesArray[index]);
    };
    var sizeToIndex = function (size) {
      return findIndex$1(candidatesArray, function (v) {
        return v === size;
      });
    };
    var getRawOrComputed = function (isRoot, rawStart) {
      var optStart = isElement(rawStart) ? Optional.some(rawStart) : parent(rawStart).filter(isElement);
      return optStart.map(function (start) {
        var inline = closest$2(start, function (elem) {
          return getRaw(elem, 'font-size').isSome();
        }, isRoot).bind(function (elem) {
          return getRaw(elem, 'font-size');
        });
        return inline.getOrThunk(function () {
          return get$8(start, 'font-size');
        });
      }).getOr('');
    };
    var getSize = function (editor) {
      var node = editor.selection.getStart();
      var elem = SugarElement.fromDom(node);
      var root = SugarElement.fromDom(editor.getBody());
      var isRoot = function (e) {
        return eq(root, e);
      };
      var elemSize = getRawOrComputed(isRoot, elem);
      return find$2(candidatesArray, function (size) {
        return elemSize === size;
      }).getOr(defaultSize);
    };
    var applySize = function (editor, value) {
      var currentValue = getSize(editor);
      if (currentValue !== value) {
        editor.execCommand('fontSize', false, value);
      }
    };
    var get$4 = function (editor) {
      var size = getSize(editor);
      return sizeToIndex(size).getOr(defaultIndex);
    };
    var apply = function (editor, index) {
      indexToSize(index).each(function (size) {
        applySize(editor, size);
      });
    };
    var candidates = constant$1(candidatesArray);

    var schema$9 = objOfOnly([
      required$1('getInitialValue'),
      required$1('onChange'),
      required$1('category'),
      required$1('sizes')
    ]);
    var sketch$7 = function (rawSpec) {
      var spec$1 = asRawOrDie$1('SizeSlider', schema$9, rawSpec);
      var isValidValue = function (valueIndex) {
        return valueIndex >= 0 && valueIndex < spec$1.sizes.length;
      };
      var onChange = function (slider, thumb, valueIndex) {
        var index = valueIndex.x();
        if (isValidValue(index)) {
          spec$1.onChange(index);
        }
      };
      return Slider.sketch({
        dom: {
          tag: 'div',
          classes: [
            resolve('slider-' + spec$1.category + '-size-container'),
            resolve('slider'),
            resolve('slider-size-container')
          ]
        },
        onChange: onChange,
        onDragStart: function (slider, thumb) {
          Toggling.on(thumb);
        },
        onDragEnd: function (slider, thumb) {
          Toggling.off(thumb);
        },
        model: {
          mode: 'x',
          minX: 0,
          maxX: spec$1.sizes.length - 1,
          getInitialValue: function () {
            return { x: spec$1.getInitialValue() };
          }
        },
        stepSize: 1,
        snapToGrid: true,
        sliderBehaviours: derive$2([orientation(Slider.refresh)]),
        components: [
          Slider.parts.spectrum({
            dom: dom$1('<div class="${prefix}-slider-size-container"></div>'),
            components: [spec('<div class="${prefix}-slider-size-line"></div>')]
          }),
          Slider.parts.thumb({
            dom: dom$1('<div class="${prefix}-slider-thumb"></div>'),
            behaviours: derive$2([Toggling.config({ toggleClass: resolve('thumb-active') })])
          })
        ]
      });
    };

    var sizes = candidates();
    var makeSlider = function (spec) {
      return sketch$7({
        onChange: spec.onChange,
        sizes: sizes,
        category: 'font',
        getInitialValue: spec.getInitialValue
      });
    };
    var makeItems = function (spec$1) {
      return [
        spec('<span class="${prefix}-toolbar-button ${prefix}-icon-small-font ${prefix}-icon"></span>'),
        makeSlider(spec$1),
        spec('<span class="${prefix}-toolbar-button ${prefix}-icon-large-font ${prefix}-icon"></span>')
      ];
    };
    var sketch$6 = function (realm, editor) {
      var spec = {
        onChange: function (value) {
          apply(editor, value);
        },
        getInitialValue: function () {
          return get$4(editor);
        }
      };
      return button(realm, 'font-size', function () {
        return makeItems(spec);
      }, editor);
    };

    var record = function (spec) {
      var uid = isSketchSpec$1(spec) && hasNonNullableKey(spec, 'uid') ? spec.uid : generate$2('memento');
      var get = function (anyInSystem) {
        return anyInSystem.getSystem().getByUid(uid).getOrDie();
      };
      var getOpt = function (anyInSystem) {
        return anyInSystem.getSystem().getByUid(uid).toOptional();
      };
      var asSpec = function () {
        return __assign(__assign({}, spec), { uid: uid });
      };
      return {
        get: get,
        getOpt: getOpt,
        asSpec: asSpec
      };
    };

    var exports$1 = {}, module = { exports: exports$1 };
    (function (define, exports, module, require) {
      (function (global, factory) {
        typeof exports === 'object' && typeof module !== 'undefined' ? module.exports = factory() : typeof define === 'function' && define.amd ? define(factory) : (global = typeof globalThis !== 'undefined' ? globalThis : global || self, global.EphoxContactWrapper = factory());
      }(this, function () {
        var commonjsGlobal = typeof globalThis !== 'undefined' ? globalThis : typeof window !== 'undefined' ? window : typeof global !== 'undefined' ? global : typeof self !== 'undefined' ? self : {};
        var promise = { exports: {} };
        (function (module) {
          (function (root) {
            var setTimeoutFunc = setTimeout;
            function noop() {
            }
            function bind(fn, thisArg) {
              return function () {
                fn.apply(thisArg, arguments);
              };
            }
            function Promise(fn) {
              if (typeof this !== 'object')
                throw new TypeError('Promises must be constructed via new');
              if (typeof fn !== 'function')
                throw new TypeError('not a function');
              this._state = 0;
              this._handled = false;
              this._value = undefined;
              this._deferreds = [];
              doResolve(fn, this);
            }
            function handle(self, deferred) {
              while (self._state === 3) {
                self = self._value;
              }
              if (self._state === 0) {
                self._deferreds.push(deferred);
                return;
              }
              self._handled = true;
              Promise._immediateFn(function () {
                var cb = self._state === 1 ? deferred.onFulfilled : deferred.onRejected;
                if (cb === null) {
                  (self._state === 1 ? resolve : reject)(deferred.promise, self._value);
                  return;
                }
                var ret;
                try {
                  ret = cb(self._value);
                } catch (e) {
                  reject(deferred.promise, e);
                  return;
                }
                resolve(deferred.promise, ret);
              });
            }
            function resolve(self, newValue) {
              try {
                if (newValue === self)
                  throw new TypeError('A promise cannot be resolved with itself.');
                if (newValue && (typeof newValue === 'object' || typeof newValue === 'function')) {
                  var then = newValue.then;
                  if (newValue instanceof Promise) {
                    self._state = 3;
                    self._value = newValue;
                    finale(self);
                    return;
                  } else if (typeof then === 'function') {
                    doResolve(bind(then, newValue), self);
                    return;
                  }
                }
                self._state = 1;
                self._value = newValue;
                finale(self);
              } catch (e) {
                reject(self, e);
              }
            }
            function reject(self, newValue) {
              self._state = 2;
              self._value = newValue;
              finale(self);
            }
            function finale(self) {
              if (self._state === 2 && self._deferreds.length === 0) {
                Promise._immediateFn(function () {
                  if (!self._handled) {
                    Promise._unhandledRejectionFn(self._value);
                  }
                });
              }
              for (var i = 0, len = self._deferreds.length; i < len; i++) {
                handle(self, self._deferreds[i]);
              }
              self._deferreds = null;
            }
            function Handler(onFulfilled, onRejected, promise) {
              this.onFulfilled = typeof onFulfilled === 'function' ? onFulfilled : null;
              this.onRejected = typeof onRejected === 'function' ? onRejected : null;
              this.promise = promise;
            }
            function doResolve(fn, self) {
              var done = false;
              try {
                fn(function (value) {
                  if (done)
                    return;
                  done = true;
                  resolve(self, value);
                }, function (reason) {
                  if (done)
                    return;
                  done = true;
                  reject(self, reason);
                });
              } catch (ex) {
                if (done)
                  return;
                done = true;
                reject(self, ex);
              }
            }
            Promise.prototype['catch'] = function (onRejected) {
              return this.then(null, onRejected);
            };
            Promise.prototype.then = function (onFulfilled, onRejected) {
              var prom = new this.constructor(noop);
              handle(this, new Handler(onFulfilled, onRejected, prom));
              return prom;
            };
            Promise.all = function (arr) {
              var args = Array.prototype.slice.call(arr);
              return new Promise(function (resolve, reject) {
                if (args.length === 0)
                  return resolve([]);
                var remaining = args.length;
                function res(i, val) {
                  try {
                    if (val && (typeof val === 'object' || typeof val === 'function')) {
                      var then = val.then;
                      if (typeof then === 'function') {
                        then.call(val, function (val) {
                          res(i, val);
                        }, reject);
                        return;
                      }
                    }
                    args[i] = val;
                    if (--remaining === 0) {
                      resolve(args);
                    }
                  } catch (ex) {
                    reject(ex);
                  }
                }
                for (var i = 0; i < args.length; i++) {
                  res(i, args[i]);
                }
              });
            };
            Promise.resolve = function (value) {
              if (value && typeof value === 'object' && value.constructor === Promise) {
                return value;
              }
              return new Promise(function (resolve) {
                resolve(value);
              });
            };
            Promise.reject = function (value) {
              return new Promise(function (resolve, reject) {
                reject(value);
              });
            };
            Promise.race = function (values) {
              return new Promise(function (resolve, reject) {
                for (var i = 0, len = values.length; i < len; i++) {
                  values[i].then(resolve, reject);
                }
              });
            };
            Promise._immediateFn = typeof setImmediate === 'function' ? function (fn) {
              setImmediate(fn);
            } : function (fn) {
              setTimeoutFunc(fn, 0);
            };
            Promise._unhandledRejectionFn = function _unhandledRejectionFn(err) {
              if (typeof console !== 'undefined' && console) {
                console.warn('Possible Unhandled Promise Rejection:', err);
              }
            };
            Promise._setImmediateFn = function _setImmediateFn(fn) {
              Promise._immediateFn = fn;
            };
            Promise._setUnhandledRejectionFn = function _setUnhandledRejectionFn(fn) {
              Promise._unhandledRejectionFn = fn;
            };
            if (module.exports) {
              module.exports = Promise;
            } else if (!root.Promise) {
              root.Promise = Promise;
            }
          }(commonjsGlobal));
        }(promise));
        var promisePolyfill = promise.exports;
        var Global = function () {
          if (typeof window !== 'undefined') {
            return window;
          } else {
            return Function('return this;')();
          }
        }();
        var promisePolyfill_1 = { boltExport: Global.Promise || promisePolyfill };
        return promisePolyfill_1;
      }));
    }(undefined, exports$1, module));
    var Promise$1 = module.exports.boltExport;

    var blobToDataUri = function (blob) {
      return new Promise$1(function (resolve) {
        var reader = new FileReader();
        reader.onloadend = function () {
          resolve(reader.result);
        };
        reader.readAsDataURL(blob);
      });
    };
    var blobToBase64$1 = function (blob) {
      return blobToDataUri(blob).then(function (dataUri) {
        return dataUri.split(',')[1];
      });
    };

    var blobToBase64 = function (blob) {
      return blobToBase64$1(blob);
    };

    var addImage = function (editor, blob) {
      blobToBase64(blob).then(function (base64) {
        editor.undoManager.transact(function () {
          var cache = editor.editorUpload.blobCache;
          var info = cache.create(generate$4('mceu'), blob, base64);
          cache.add(info);
          var img = editor.dom.createHTML('img', { src: info.blobUri() });
          editor.insertContent(img);
        });
      });
    };
    var extractBlob = function (simulatedEvent) {
      var event = simulatedEvent.event.raw;
      var files = event.target.files || event.dataTransfer.files;
      return Optional.from(files[0]);
    };
    var sketch$5 = function (editor) {
      var pickerDom = {
        tag: 'input',
        attributes: {
          accept: 'image/*',
          type: 'file',
          title: ''
        },
        styles: {
          visibility: 'hidden',
          position: 'absolute'
        }
      };
      var memPicker = record({
        dom: pickerDom,
        events: derive$3([
          cutter(click()),
          run(change(), function (picker, simulatedEvent) {
            extractBlob(simulatedEvent).each(function (blob) {
              addImage(editor, blob);
            });
          })
        ])
      });
      return Button.sketch({
        dom: getToolbarIconButton('image', editor),
        components: [memPicker.asSpec()],
        action: function (button) {
          var picker = memPicker.get(button);
          picker.element.dom.click();
        }
      });
    };

    var get$3 = function (element) {
      return element.dom.textContent;
    };
    var set$3 = function (element, value) {
      element.dom.textContent = value;
    };

    var isNotEmpty = function (val) {
      return val.length > 0;
    };
    var defaultToEmpty = function (str) {
      return str === undefined || str === null ? '' : str;
    };
    var noLink = function (editor) {
      var text = editor.selection.getContent({ format: 'text' });
      return {
        url: '',
        text: text,
        title: '',
        target: '',
        link: Optional.none()
      };
    };
    var fromLink = function (link) {
      var text = get$3(link);
      var url = get$b(link, 'href');
      var title = get$b(link, 'title');
      var target = get$b(link, 'target');
      return {
        url: defaultToEmpty(url),
        text: text !== url ? defaultToEmpty(text) : '',
        title: defaultToEmpty(title),
        target: defaultToEmpty(target),
        link: Optional.some(link)
      };
    };
    var getInfo = function (editor) {
      return query(editor).fold(function () {
        return noLink(editor);
      }, function (link) {
        return fromLink(link);
      });
    };
    var wasSimple = function (link) {
      var prevHref = get$b(link, 'href');
      var prevText = get$3(link);
      return prevHref === prevText;
    };
    var getTextToApply = function (link, url, info) {
      return info.text.toOptional().filter(isNotEmpty).fold(function () {
        return wasSimple(link) ? Optional.some(url) : Optional.none();
      }, Optional.some);
    };
    var unlinkIfRequired = function (editor, info) {
      var activeLink = info.link.bind(identity);
      activeLink.each(function (_link) {
        editor.execCommand('unlink');
      });
    };
    var getAttrs = function (url, info) {
      var attrs = {};
      attrs.href = url;
      info.title.toOptional().filter(isNotEmpty).each(function (title) {
        attrs.title = title;
      });
      info.target.toOptional().filter(isNotEmpty).each(function (target) {
        attrs.target = target;
      });
      return attrs;
    };
    var applyInfo = function (editor, info) {
      info.url.toOptional().filter(isNotEmpty).fold(function () {
        unlinkIfRequired(editor, info);
      }, function (url) {
        var attrs = getAttrs(url, info);
        var activeLink = info.link.bind(identity);
        activeLink.fold(function () {
          var text = info.text.toOptional().filter(isNotEmpty).getOr(url);
          editor.insertContent(editor.dom.createHTML('a', attrs, editor.dom.encode(text)));
        }, function (link) {
          var text = getTextToApply(link, url, info);
          setAll$1(link, attrs);
          text.each(function (newText) {
            set$3(link, newText);
          });
        });
      });
    };
    var query = function (editor) {
      var start = SugarElement.fromDom(editor.selection.getStart());
      return closest$1(start, 'a');
    };

    var platform = detect$1();
    var preserve$1 = function (f, editor) {
      var rng = editor.selection.getRng();
      f();
      editor.selection.setRng(rng);
    };
    var forAndroid = function (editor, f) {
      var wrapper = platform.os.isAndroid() ? preserve$1 : apply$1;
      wrapper(f, editor);
    };

    var events$4 = function (name, eventHandlers) {
      var events = derive$3(eventHandlers);
      return create$5({
        fields: [required$1('enabled')],
        name: name,
        active: { events: constant$1(events) }
      });
    };
    var config = function (name, eventHandlers) {
      var me = events$4(name, eventHandlers);
      return {
        key: name,
        value: {
          config: {},
          me: me,
          configAsRaw: constant$1({}),
          initialConfig: {},
          state: NoState
        }
      };
    };

    var getCurrent = function (component, composeConfig, _composeState) {
      return composeConfig.find(component);
    };

    var ComposeApis = /*#__PURE__*/Object.freeze({
        __proto__: null,
        getCurrent: getCurrent
    });

    var ComposeSchema = [required$1('find')];

    var Composing = create$5({
      fields: ComposeSchema,
      name: 'composing',
      apis: ComposeApis
    });

    var factory$4 = function (detail) {
      var _a = detail.dom, attributes = _a.attributes, domWithoutAttributes = __rest(_a, ['attributes']);
      return {
        uid: detail.uid,
        dom: __assign({
          tag: 'div',
          attributes: __assign({ role: 'presentation' }, attributes)
        }, domWithoutAttributes),
        components: detail.components,
        behaviours: get$6(detail.containerBehaviours),
        events: detail.events,
        domModification: detail.domModification,
        eventOrder: detail.eventOrder
      };
    };
    var Container = single({
      name: 'Container',
      factory: factory$4,
      configFields: [
        defaulted('components', []),
        field$1('containerBehaviours', []),
        defaulted('events', {}),
        defaulted('domModification', {}),
        defaulted('eventOrder', {})
      ]
    });

    var factory$3 = function (detail) {
      return {
        uid: detail.uid,
        dom: detail.dom,
        behaviours: SketchBehaviours.augment(detail.dataBehaviours, [
          Representing.config({
            store: {
              mode: 'memory',
              initialValue: detail.getInitialValue()
            }
          }),
          Composing.config({ find: Optional.some })
        ]),
        events: derive$3([runOnAttached(function (component, _simulatedEvent) {
            Representing.setValue(component, detail.getInitialValue());
          })])
      };
    };
    var DataField = single({
      name: 'DataField',
      factory: factory$3,
      configFields: [
        required$1('uid'),
        required$1('dom'),
        required$1('getInitialValue'),
        SketchBehaviours.field('dataBehaviours', [
          Representing,
          Composing
        ])
      ]
    });

    var get$2 = function (element) {
      return element.dom.value;
    };
    var set$2 = function (element, value) {
      if (value === undefined) {
        throw new Error('Value.set was undefined');
      }
      element.dom.value = value;
    };

    var schema$8 = constant$1([
      option('data'),
      defaulted('inputAttributes', {}),
      defaulted('inputStyles', {}),
      defaulted('tag', 'input'),
      defaulted('inputClasses', []),
      onHandler('onSetValue'),
      defaulted('styles', {}),
      defaulted('eventOrder', {}),
      field$1('inputBehaviours', [
        Representing,
        Focusing
      ]),
      defaulted('selectOnFocus', true)
    ]);
    var focusBehaviours = function (detail) {
      return derive$2([Focusing.config({
          onFocus: !detail.selectOnFocus ? noop : function (component) {
            var input = component.element;
            var value = get$2(input);
            input.dom.setSelectionRange(0, value.length);
          }
        })]);
    };
    var behaviours = function (detail) {
      return __assign(__assign({}, focusBehaviours(detail)), augment(detail.inputBehaviours, [Representing.config({
          store: __assign(__assign({ mode: 'manual' }, detail.data.map(function (data) {
            return { initialValue: data };
          }).getOr({})), {
            getValue: function (input) {
              return get$2(input.element);
            },
            setValue: function (input, data) {
              var current = get$2(input.element);
              if (current !== data) {
                set$2(input.element, data);
              }
            }
          }),
          onSetValue: detail.onSetValue
        })]));
    };
    var dom = function (detail) {
      return {
        tag: detail.tag,
        attributes: __assign({ type: 'text' }, detail.inputAttributes),
        styles: detail.inputStyles,
        classes: detail.inputClasses
      };
    };

    var factory$2 = function (detail, _spec) {
      return {
        uid: detail.uid,
        dom: dom(detail),
        components: [],
        behaviours: behaviours(detail),
        eventOrder: detail.eventOrder
      };
    };
    var Input = single({
      name: 'Input',
      configFields: schema$8(),
      factory: factory$2
    });

    var exhibit$2 = function (base, tabConfig) {
      return nu$3({
        attributes: wrapAll([{
            key: tabConfig.tabAttr,
            value: 'true'
          }])
      });
    };

    var ActiveTabstopping = /*#__PURE__*/Object.freeze({
        __proto__: null,
        exhibit: exhibit$2
    });

    var TabstopSchema = [defaulted('tabAttr', 'data-alloy-tabstop')];

    var Tabstopping = create$5({
      fields: TabstopSchema,
      name: 'tabstopping',
      active: ActiveTabstopping
    });

    var global$3 = tinymce.util.Tools.resolve('tinymce.util.I18n');

    var clearInputBehaviour = 'input-clearing';
    var field = function (name, placeholder) {
      var inputSpec = record(Input.sketch({
        inputAttributes: { placeholder: global$3.translate(placeholder) },
        onSetValue: function (input, _data) {
          emit(input, input$1());
        },
        inputBehaviours: derive$2([
          Composing.config({ find: Optional.some }),
          Tabstopping.config({}),
          Keying.config({ mode: 'execution' })
        ]),
        selectOnFocus: false
      }));
      var buttonSpec = record(Button.sketch({
        dom: dom$1('<button class="${prefix}-input-container-x ${prefix}-icon-cancel-circle ${prefix}-icon"></button>'),
        action: function (button) {
          var input = inputSpec.get(button);
          Representing.setValue(input, '');
        }
      }));
      return {
        name: name,
        spec: Container.sketch({
          dom: dom$1('<div class="${prefix}-input-container"></div>'),
          components: [
            inputSpec.asSpec(),
            buttonSpec.asSpec()
          ],
          containerBehaviours: derive$2([
            Toggling.config({ toggleClass: resolve('input-container-empty') }),
            Composing.config({
              find: function (comp) {
                return Optional.some(inputSpec.get(comp));
              }
            }),
            config(clearInputBehaviour, [run(input$1(), function (iContainer) {
                var input = inputSpec.get(iContainer);
                var val = Representing.getValue(input);
                var f = val.length > 0 ? Toggling.off : Toggling.on;
                f(iContainer);
              })])
          ])
        })
      };
    };
    var hidden = function (name) {
      return {
        name: name,
        spec: DataField.sketch({
          dom: {
            tag: 'span',
            styles: { display: 'none' }
          },
          getInitialValue: function () {
            return Optional.none();
          }
        })
      };
    };

    var nativeDisabled = [
      'input',
      'button',
      'textarea',
      'select'
    ];
    var onLoad = function (component, disableConfig, disableState) {
      var f = disableConfig.disabled() ? disable : enable;
      f(component, disableConfig);
    };
    var hasNative = function (component, config) {
      return config.useNative === true && contains$1(nativeDisabled, name$1(component.element));
    };
    var nativeIsDisabled = function (component) {
      return has$1(component.element, 'disabled');
    };
    var nativeDisable = function (component) {
      set$8(component.element, 'disabled', 'disabled');
    };
    var nativeEnable = function (component) {
      remove$6(component.element, 'disabled');
    };
    var ariaIsDisabled = function (component) {
      return get$b(component.element, 'aria-disabled') === 'true';
    };
    var ariaDisable = function (component) {
      set$8(component.element, 'aria-disabled', 'true');
    };
    var ariaEnable = function (component) {
      set$8(component.element, 'aria-disabled', 'false');
    };
    var disable = function (component, disableConfig, _disableState) {
      disableConfig.disableClass.each(function (disableClass) {
        add$1(component.element, disableClass);
      });
      var f = hasNative(component, disableConfig) ? nativeDisable : ariaDisable;
      f(component);
      disableConfig.onDisabled(component);
    };
    var enable = function (component, disableConfig, _disableState) {
      disableConfig.disableClass.each(function (disableClass) {
        remove$3(component.element, disableClass);
      });
      var f = hasNative(component, disableConfig) ? nativeEnable : ariaEnable;
      f(component);
      disableConfig.onEnabled(component);
    };
    var isDisabled = function (component, disableConfig) {
      return hasNative(component, disableConfig) ? nativeIsDisabled(component) : ariaIsDisabled(component);
    };
    var set$1 = function (component, disableConfig, disableState, disabled) {
      var f = disabled ? disable : enable;
      f(component, disableConfig);
    };

    var DisableApis = /*#__PURE__*/Object.freeze({
        __proto__: null,
        enable: enable,
        disable: disable,
        isDisabled: isDisabled,
        onLoad: onLoad,
        set: set$1
    });

    var exhibit$1 = function (base, disableConfig) {
      return nu$3({ classes: disableConfig.disabled() ? disableConfig.disableClass.toArray() : [] });
    };
    var events$3 = function (disableConfig, disableState) {
      return derive$3([
        abort(execute$5(), function (component, _simulatedEvent) {
          return isDisabled(component, disableConfig);
        }),
        loadEvent(disableConfig, disableState, onLoad)
      ]);
    };

    var ActiveDisable = /*#__PURE__*/Object.freeze({
        __proto__: null,
        exhibit: exhibit$1,
        events: events$3
    });

    var DisableSchema = [
      defaultedFunction('disabled', never),
      defaulted('useNative', true),
      option('disableClass'),
      onHandler('onDisabled'),
      onHandler('onEnabled')
    ];

    var Disabling = create$5({
      fields: DisableSchema,
      name: 'disabling',
      active: ActiveDisable,
      apis: DisableApis
    });

    var owner$1 = 'form';
    var schema$7 = [field$1('formBehaviours', [Representing])];
    var getPartName = function (name) {
      return '<alloy.field.' + name + '>';
    };
    var sketch$4 = function (fSpec) {
      var parts = function () {
        var record = [];
        var field = function (name, config) {
          record.push(name);
          return generateOne(owner$1, getPartName(name), config);
        };
        return {
          field: field,
          record: constant$1(record)
        };
      }();
      var spec = fSpec(parts);
      var partNames = parts.record();
      var fieldParts = map$2(partNames, function (n) {
        return required({
          name: n,
          pname: getPartName(n)
        });
      });
      return composite$1(owner$1, schema$7, fieldParts, make$4, spec);
    };
    var toResult = function (o, e) {
      return o.fold(function () {
        return Result.error(e);
      }, Result.value);
    };
    var make$4 = function (detail, components) {
      return {
        uid: detail.uid,
        dom: detail.dom,
        components: components,
        behaviours: augment(detail.formBehaviours, [Representing.config({
            store: {
              mode: 'manual',
              getValue: function (form) {
                var resPs = getAllParts(form, detail);
                return map$1(resPs, function (resPThunk, pName) {
                  return resPThunk().bind(function (v) {
                    var opt = Composing.getCurrent(v);
                    return toResult(opt, new Error('Cannot find a current component to extract the value from for form part \'' + pName + '\': ' + element(v.element)));
                  }).map(Representing.getValue);
                });
              },
              setValue: function (form, values) {
                each(values, function (newValue, key) {
                  getPart(form, detail, key).each(function (wrapper) {
                    Composing.getCurrent(wrapper).each(function (field) {
                      Representing.setValue(field, newValue);
                    });
                  });
                });
              }
            }
          })]),
        apis: {
          getField: function (form, key) {
            return getPart(form, detail, key).bind(Composing.getCurrent);
          }
        }
      };
    };
    var Form = {
      getField: makeApi(function (apis, component, key) {
        return apis.getField(component, key);
      }),
      sketch: sketch$4
    };

    var SWIPING_LEFT = 1;
    var SWIPING_RIGHT = -1;
    var SWIPING_NONE = 0;
    var init$3 = function (xValue) {
      return {
        xValue: xValue,
        points: []
      };
    };
    var move = function (model, xValue) {
      if (xValue === model.xValue) {
        return model;
      }
      var currentDirection = xValue - model.xValue > 0 ? SWIPING_LEFT : SWIPING_RIGHT;
      var newPoint = {
        direction: currentDirection,
        xValue: xValue
      };
      var priorPoints = function () {
        if (model.points.length === 0) {
          return [];
        } else {
          var prev = model.points[model.points.length - 1];
          return prev.direction === currentDirection ? model.points.slice(0, model.points.length - 1) : model.points;
        }
      }();
      return {
        xValue: xValue,
        points: priorPoints.concat([newPoint])
      };
    };
    var complete = function (model) {
      if (model.points.length === 0) {
        return SWIPING_NONE;
      } else {
        var firstDirection = model.points[0].direction;
        var lastDirection = model.points[model.points.length - 1].direction;
        return firstDirection === SWIPING_RIGHT && lastDirection === SWIPING_RIGHT ? SWIPING_RIGHT : firstDirection === SWIPING_LEFT && lastDirection === SWIPING_LEFT ? SWIPING_LEFT : SWIPING_NONE;
      }
    };

    var sketch$3 = function (rawSpec) {
      var navigateEvent = 'navigateEvent';
      var wrapperAdhocEvents = 'serializer-wrapper-events';
      var formAdhocEvents = 'form-events';
      var schema = objOf([
        required$1('fields'),
        defaulted('maxFieldIndex', rawSpec.fields.length - 1),
        required$1('onExecute'),
        required$1('getInitialValue'),
        customField('state', function () {
          return {
            dialogSwipeState: value(),
            currentScreen: Cell(0)
          };
        })
      ]);
      var spec$1 = asRawOrDie$1('SerialisedDialog', schema, rawSpec);
      var navigationButton = function (direction, directionName, enabled) {
        return Button.sketch({
          dom: dom$1('<span class="${prefix}-icon-' + directionName + ' ${prefix}-icon"></span>'),
          action: function (button) {
            emitWith(button, navigateEvent, { direction: direction });
          },
          buttonBehaviours: derive$2([Disabling.config({
              disableClass: resolve('toolbar-navigation-disabled'),
              disabled: function () {
                return !enabled;
              }
            })])
        });
      };
      var reposition = function (dialog, message) {
        descendant(dialog.element, '.' + resolve('serialised-dialog-chain')).each(function (parent) {
          set$5(parent, 'left', -spec$1.state.currentScreen.get() * message.width + 'px');
        });
      };
      var navigate = function (dialog, direction) {
        var screens = descendants(dialog.element, '.' + resolve('serialised-dialog-screen'));
        descendant(dialog.element, '.' + resolve('serialised-dialog-chain')).each(function (parent) {
          if (spec$1.state.currentScreen.get() + direction >= 0 && spec$1.state.currentScreen.get() + direction < screens.length) {
            getRaw(parent, 'left').each(function (left) {
              var currentLeft = parseInt(left, 10);
              var w = get$5(screens[0]);
              set$5(parent, 'left', currentLeft - direction * w + 'px');
            });
            spec$1.state.currentScreen.set(spec$1.state.currentScreen.get() + direction);
          }
        });
      };
      var focusInput = function (dialog) {
        var inputs = descendants(dialog.element, 'input');
        var optInput = Optional.from(inputs[spec$1.state.currentScreen.get()]);
        optInput.each(function (input) {
          dialog.getSystem().getByDom(input).each(function (inputComp) {
            dispatchFocus(dialog, inputComp.element);
          });
        });
        var dotitems = memDots.get(dialog);
        Highlighting.highlightAt(dotitems, spec$1.state.currentScreen.get());
      };
      var resetState = function () {
        spec$1.state.currentScreen.set(0);
        spec$1.state.dialogSwipeState.clear();
      };
      var memForm = record(Form.sketch(function (parts) {
        return {
          dom: dom$1('<div class="${prefix}-serialised-dialog"></div>'),
          components: [Container.sketch({
              dom: dom$1('<div class="${prefix}-serialised-dialog-chain" style="left: 0px; position: absolute;"></div>'),
              components: map$2(spec$1.fields, function (field, i) {
                return i <= spec$1.maxFieldIndex ? Container.sketch({
                  dom: dom$1('<div class="${prefix}-serialised-dialog-screen"></div>'),
                  components: [
                    navigationButton(-1, 'previous', i > 0),
                    parts.field(field.name, field.spec),
                    navigationButton(+1, 'next', i < spec$1.maxFieldIndex)
                  ]
                }) : parts.field(field.name, field.spec);
              })
            })],
          formBehaviours: derive$2([
            orientation(function (dialog, message) {
              reposition(dialog, message);
            }),
            Keying.config({
              mode: 'special',
              focusIn: function (dialog, _specialInfo) {
                focusInput(dialog);
              },
              onTab: function (dialog, _specialInfo) {
                navigate(dialog, +1);
                return Optional.some(true);
              },
              onShiftTab: function (dialog, _specialInfo) {
                navigate(dialog, -1);
                return Optional.some(true);
              }
            }),
            config(formAdhocEvents, [
              runOnAttached(function (dialog, _simulatedEvent) {
                resetState();
                var dotitems = memDots.get(dialog);
                Highlighting.highlightFirst(dotitems);
                spec$1.getInitialValue(dialog).each(function (v) {
                  Representing.setValue(dialog, v);
                });
              }),
              runOnExecute(spec$1.onExecute),
              run(transitionend(), function (dialog, simulatedEvent) {
                var event = simulatedEvent.event;
                if (event.raw.propertyName === 'left') {
                  focusInput(dialog);
                }
              }),
              run(navigateEvent, function (dialog, simulatedEvent) {
                var event = simulatedEvent.event;
                var direction = event.direction;
                navigate(dialog, direction);
              })
            ])
          ])
        };
      }));
      var memDots = record({
        dom: dom$1('<div class="${prefix}-dot-container"></div>'),
        behaviours: derive$2([Highlighting.config({
            highlightClass: resolve('dot-active'),
            itemClass: resolve('dot-item')
          })]),
        components: bind$3(spec$1.fields, function (_f, i) {
          return i <= spec$1.maxFieldIndex ? [spec('<div class="${prefix}-dot-item ${prefix}-icon-full-dot ${prefix}-icon"></div>')] : [];
        })
      });
      return {
        dom: dom$1('<div class="${prefix}-serializer-wrapper"></div>'),
        components: [
          memForm.asSpec(),
          memDots.asSpec()
        ],
        behaviours: derive$2([
          Keying.config({
            mode: 'special',
            focusIn: function (wrapper) {
              var form = memForm.get(wrapper);
              Keying.focusIn(form);
            }
          }),
          config(wrapperAdhocEvents, [
            run(touchstart(), function (_wrapper, simulatedEvent) {
              var event = simulatedEvent.event;
              spec$1.state.dialogSwipeState.set(init$3(event.raw.touches[0].clientX));
            }),
            run(touchmove(), function (_wrapper, simulatedEvent) {
              var event = simulatedEvent.event;
              spec$1.state.dialogSwipeState.on(function (state) {
                simulatedEvent.event.prevent();
                spec$1.state.dialogSwipeState.set(move(state, event.raw.touches[0].clientX));
              });
            }),
            run(touchend(), function (wrapper, _simulatedEvent) {
              spec$1.state.dialogSwipeState.on(function (state) {
                var dialog = memForm.get(wrapper);
                var direction = -1 * complete(state);
                navigate(dialog, direction);
              });
            })
          ])
        ])
      };
    };

    var getGroups = cached(function (realm, editor) {
      return [{
          label: 'the link group',
          items: [sketch$3({
              fields: [
                field('url', 'Type or paste URL'),
                field('text', 'Link text'),
                field('title', 'Link title'),
                field('target', 'Link target'),
                hidden('link')
              ],
              maxFieldIndex: [
                'url',
                'text',
                'title',
                'target'
              ].length - 1,
              getInitialValue: function () {
                return Optional.some(getInfo(editor));
              },
              onExecute: function (dialog, _simulatedEvent) {
                var info = Representing.getValue(dialog);
                applyInfo(editor, info);
                realm.restoreToolbar();
                editor.focus();
              }
            })]
        }];
    });
    var sketch$2 = function (realm, editor) {
      return forToolbarStateAction(editor, 'link', 'link', function () {
        var groups = getGroups(realm, editor);
        realm.setContextToolbar(groups);
        forAndroid(editor, function () {
          realm.focusToolbar();
        });
        query(editor).each(function (link) {
          editor.selection.select(link.dom);
        });
      });
    };

    var isRecursive = function (component, originator, target) {
      return eq(originator, component.element) && !eq(originator, target);
    };
    var events$2 = derive$3([can(focus$4(), function (component, simulatedEvent) {
        var event = simulatedEvent.event;
        var originator = event.originator;
        var target = event.target;
        if (isRecursive(component, originator, target)) {
          console.warn(focus$4() + ' did not get interpreted by the desired target. ' + '\nOriginator: ' + element(originator) + '\nTarget: ' + element(target) + '\nCheck the ' + focus$4() + ' event handlers');
          return false;
        } else {
          return true;
        }
      })]);

    var DefaultEvents = /*#__PURE__*/Object.freeze({
        __proto__: null,
        events: events$2
    });

    var make$3 = identity;

    var NoContextApi = function (getComp) {
      var getMessage = function (event) {
        return 'The component must be in a context to execute: ' + event + (getComp ? '\n' + element(getComp().element) + ' is not in context.' : '');
      };
      var fail = function (event) {
        return function () {
          throw new Error(getMessage(event));
        };
      };
      var warn = function (event) {
        return function () {
          console.warn(getMessage(event));
        };
      };
      return {
        debugInfo: constant$1('fake'),
        triggerEvent: warn('triggerEvent'),
        triggerFocus: warn('triggerFocus'),
        triggerEscape: warn('triggerEscape'),
        broadcast: warn('broadcast'),
        broadcastOn: warn('broadcastOn'),
        broadcastEvent: warn('broadcastEvent'),
        build: fail('build'),
        addToWorld: fail('addToWorld'),
        removeFromWorld: fail('removeFromWorld'),
        addToGui: fail('addToGui'),
        removeFromGui: fail('removeFromGui'),
        getByUid: fail('getByUid'),
        getByDom: fail('getByDom'),
        isConnected: never
      };
    };
    var singleton = NoContextApi();

    var generateFrom$1 = function (spec, all) {
      var schema = map$2(all, function (a) {
        return optionObjOf(a.name(), [
          required$1('config'),
          defaulted('state', NoState)
        ]);
      });
      var validated = asRaw('component.behaviours', objOf(schema), spec.behaviours).fold(function (errInfo) {
        throw new Error(formatError(errInfo) + '\nComplete spec:\n' + JSON.stringify(spec, null, 2));
      }, identity);
      return {
        list: all,
        data: map$1(validated, function (optBlobThunk) {
          var output = optBlobThunk.map(function (blob) {
            return {
              config: blob.config,
              state: blob.state.init(blob.config)
            };
          });
          return constant$1(output);
        })
      };
    };
    var getBehaviours$1 = function (bData) {
      return bData.list;
    };
    var getData = function (bData) {
      return bData.data;
    };

    var byInnerKey = function (data, tuple) {
      var r = {};
      each(data, function (detail, key) {
        each(detail, function (value, indexKey) {
          var chain = get$c(r, indexKey).getOr([]);
          r[indexKey] = chain.concat([tuple(key, value)]);
        });
      });
      return r;
    };

    var combine$1 = function (info, baseMod, behaviours, base) {
      var modsByBehaviour = __assign({}, baseMod);
      each$1(behaviours, function (behaviour) {
        modsByBehaviour[behaviour.name()] = behaviour.exhibit(info, base);
      });
      var byAspect = byInnerKey(modsByBehaviour, function (name, modification) {
        return {
          name: name,
          modification: modification
        };
      });
      var combineObjects = function (objects) {
        return foldr(objects, function (b, a) {
          return __assign(__assign({}, a.modification), b);
        }, {});
      };
      var combinedClasses = foldr(byAspect.classes, function (b, a) {
        return a.modification.concat(b);
      }, []);
      var combinedAttributes = combineObjects(byAspect.attributes);
      var combinedStyles = combineObjects(byAspect.styles);
      return nu$3({
        classes: combinedClasses,
        attributes: combinedAttributes,
        styles: combinedStyles
      });
    };

    var sortKeys = function (label, keyName, array, order) {
      try {
        var sorted = sort(array, function (a, b) {
          var aKey = a[keyName];
          var bKey = b[keyName];
          var aIndex = order.indexOf(aKey);
          var bIndex = order.indexOf(bKey);
          if (aIndex === -1) {
            throw new Error('The ordering for ' + label + ' does not have an entry for ' + aKey + '.\nOrder specified: ' + JSON.stringify(order, null, 2));
          }
          if (bIndex === -1) {
            throw new Error('The ordering for ' + label + ' does not have an entry for ' + bKey + '.\nOrder specified: ' + JSON.stringify(order, null, 2));
          }
          if (aIndex < bIndex) {
            return -1;
          } else if (bIndex < aIndex) {
            return 1;
          } else {
            return 0;
          }
        });
        return Result.value(sorted);
      } catch (err) {
        return Result.error([err]);
      }
    };

    var uncurried = function (handler, purpose) {
      return {
        handler: handler,
        purpose: purpose
      };
    };
    var curried = function (handler, purpose) {
      return {
        cHandler: handler,
        purpose: purpose
      };
    };
    var curryArgs = function (descHandler, extraArgs) {
      return curried(curry.apply(undefined, [descHandler.handler].concat(extraArgs)), descHandler.purpose);
    };
    var getCurried = function (descHandler) {
      return descHandler.cHandler;
    };

    var behaviourTuple = function (name, handler) {
      return {
        name: name,
        handler: handler
      };
    };
    var nameToHandlers = function (behaviours, info) {
      var r = {};
      each$1(behaviours, function (behaviour) {
        r[behaviour.name()] = behaviour.handlers(info);
      });
      return r;
    };
    var groupByEvents = function (info, behaviours, base) {
      var behaviourEvents = __assign(__assign({}, base), nameToHandlers(behaviours, info));
      return byInnerKey(behaviourEvents, behaviourTuple);
    };
    var combine = function (info, eventOrder, behaviours, base) {
      var byEventName = groupByEvents(info, behaviours, base);
      return combineGroups(byEventName, eventOrder);
    };
    var assemble = function (rawHandler) {
      var handler = read$1(rawHandler);
      return function (component, simulatedEvent) {
        var rest = [];
        for (var _i = 2; _i < arguments.length; _i++) {
          rest[_i - 2] = arguments[_i];
        }
        var args = [
          component,
          simulatedEvent
        ].concat(rest);
        if (handler.abort.apply(undefined, args)) {
          simulatedEvent.stop();
        } else if (handler.can.apply(undefined, args)) {
          handler.run.apply(undefined, args);
        }
      };
    };
    var missingOrderError = function (eventName, tuples) {
      return Result.error(['The event (' + eventName + ') has more than one behaviour that listens to it.\nWhen this occurs, you must ' + 'specify an event ordering for the behaviours in your spec (e.g. [ "listing", "toggling" ]).\nThe behaviours that ' + 'can trigger it are: ' + JSON.stringify(map$2(tuples, function (c) {
          return c.name;
        }), null, 2)]);
    };
    var fuse = function (tuples, eventOrder, eventName) {
      var order = eventOrder[eventName];
      if (!order) {
        return missingOrderError(eventName, tuples);
      } else {
        return sortKeys('Event: ' + eventName, 'name', tuples, order).map(function (sortedTuples) {
          var handlers = map$2(sortedTuples, function (tuple) {
            return tuple.handler;
          });
          return fuse$1(handlers);
        });
      }
    };
    var combineGroups = function (byEventName, eventOrder) {
      var r = mapToArray(byEventName, function (tuples, eventName) {
        var combined = tuples.length === 1 ? Result.value(tuples[0].handler) : fuse(tuples, eventOrder, eventName);
        return combined.map(function (handler) {
          var assembled = assemble(handler);
          var purpose = tuples.length > 1 ? filter$2(eventOrder[eventName], function (o) {
            return exists(tuples, function (t) {
              return t.name === o;
            });
          }).join(' > ') : tuples[0].name;
          return wrap(eventName, uncurried(assembled, purpose));
        });
      });
      return consolidate(r, {});
    };

    var _a;
    var baseBehaviour = 'alloy.base.behaviour';
    var schema$6 = objOf([
      field$2('dom', 'dom', required$2(), objOf([
        required$1('tag'),
        defaulted('styles', {}),
        defaulted('classes', []),
        defaulted('attributes', {}),
        option('value'),
        option('innerHtml')
      ])),
      required$1('components'),
      required$1('uid'),
      defaulted('events', {}),
      defaulted('apis', {}),
      field$2('eventOrder', 'eventOrder', mergeWith((_a = {}, _a[execute$5()] = [
        'disabling',
        baseBehaviour,
        'toggling',
        'typeaheadevents'
      ], _a[focus$4()] = [
        baseBehaviour,
        'focusing',
        'keying'
      ], _a[systemInit()] = [
        baseBehaviour,
        'disabling',
        'toggling',
        'representing'
      ], _a[input$1()] = [
        baseBehaviour,
        'representing',
        'streaming',
        'invalidating'
      ], _a[detachedFromDom()] = [
        baseBehaviour,
        'representing',
        'item-events',
        'tooltipping'
      ], _a[mousedown()] = [
        'focusing',
        baseBehaviour,
        'item-type-events'
      ], _a[touchstart()] = [
        'focusing',
        baseBehaviour,
        'item-type-events'
      ], _a[mouseover()] = [
        'item-type-events',
        'tooltipping'
      ], _a[receive$1()] = [
        'receiving',
        'reflecting',
        'tooltipping'
      ], _a)), anyValue()),
      option('domModification')
    ]);
    var toInfo = function (spec) {
      return asRaw('custom.definition', schema$6, spec);
    };
    var toDefinition = function (detail) {
      return __assign(__assign({}, detail.dom), {
        uid: detail.uid,
        domChildren: map$2(detail.components, function (comp) {
          return comp.element;
        })
      });
    };
    var toModification = function (detail) {
      return detail.domModification.fold(function () {
        return nu$3({});
      }, nu$3);
    };
    var toEvents = function (info) {
      return info.events;
    };

    var add = function (element, classes) {
      each$1(classes, function (x) {
        add$1(element, x);
      });
    };
    var remove$1 = function (element, classes) {
      each$1(classes, function (x) {
        remove$3(element, x);
      });
    };

    var renderToDom = function (definition) {
      var subject = SugarElement.fromTag(definition.tag);
      setAll$1(subject, definition.attributes);
      add(subject, definition.classes);
      setAll(subject, definition.styles);
      definition.innerHtml.each(function (html) {
        return set$7(subject, html);
      });
      var children = definition.domChildren;
      append$1(subject, children);
      definition.value.each(function (value) {
        set$2(subject, value);
      });
      if (!definition.uid) {
        debugger;
      }
      writeOnly(subject, definition.uid);
      return subject;
    };

    var getBehaviours = function (spec) {
      var behaviours = get$c(spec, 'behaviours').getOr({});
      return bind$3(keys(behaviours), function (name) {
        var behaviour = behaviours[name];
        return isNonNullable(behaviour) ? [behaviour.me] : [];
      });
    };
    var generateFrom = function (spec, all) {
      return generateFrom$1(spec, all);
    };
    var generate$1 = function (spec) {
      var all = getBehaviours(spec);
      return generateFrom(spec, all);
    };

    var getDomDefinition = function (info, bList, bData) {
      var definition = toDefinition(info);
      var infoModification = toModification(info);
      var baseModification = { 'alloy.base.modification': infoModification };
      var modification = bList.length > 0 ? combine$1(bData, baseModification, bList, definition) : infoModification;
      return merge(definition, modification);
    };
    var getEvents = function (info, bList, bData) {
      var baseEvents = { 'alloy.base.behaviour': toEvents(info) };
      return combine(bData, info.eventOrder, bList, baseEvents).getOrDie();
    };
    var build$2 = function (spec) {
      var getMe = function () {
        return me;
      };
      var systemApi = Cell(singleton);
      var info = getOrDie(toInfo(spec));
      var bBlob = generate$1(spec);
      var bList = getBehaviours$1(bBlob);
      var bData = getData(bBlob);
      var modDefinition = getDomDefinition(info, bList, bData);
      var item = renderToDom(modDefinition);
      var events = getEvents(info, bList, bData);
      var subcomponents = Cell(info.components);
      var connect = function (newApi) {
        systemApi.set(newApi);
      };
      var disconnect = function () {
        systemApi.set(NoContextApi(getMe));
      };
      var syncComponents = function () {
        var children$1 = children(item);
        var subs = bind$3(children$1, function (child) {
          return systemApi.get().getByDom(child).fold(function () {
            return [];
          }, pure$2);
        });
        subcomponents.set(subs);
      };
      var config = function (behaviour) {
        var b = bData;
        var f = isFunction(b[behaviour.name()]) ? b[behaviour.name()] : function () {
          throw new Error('Could not find ' + behaviour.name() + ' in ' + JSON.stringify(spec, null, 2));
        };
        return f();
      };
      var hasConfigured = function (behaviour) {
        return isFunction(bData[behaviour.name()]);
      };
      var getApis = function () {
        return info.apis;
      };
      var readState = function (behaviourName) {
        return bData[behaviourName]().map(function (b) {
          return b.state.readState();
        }).getOr('not enabled');
      };
      var me = {
        uid: spec.uid,
        getSystem: systemApi.get,
        config: config,
        hasConfigured: hasConfigured,
        spec: spec,
        readState: readState,
        getApis: getApis,
        connect: connect,
        disconnect: disconnect,
        element: item,
        syncComponents: syncComponents,
        components: subcomponents.get,
        events: events
      };
      return me;
    };

    var buildSubcomponents = function (spec) {
      var components = get$c(spec, 'components').getOr([]);
      return map$2(components, build$1);
    };
    var buildFromSpec = function (userSpec) {
      var _a = make$3(userSpec), specEvents = _a.events, spec = __rest(_a, ['events']);
      var components = buildSubcomponents(spec);
      var completeSpec = __assign(__assign({}, spec), {
        events: __assign(__assign({}, DefaultEvents), specEvents),
        components: components
      });
      return Result.value(build$2(completeSpec));
    };
    var text = function (textContent) {
      var element = SugarElement.fromText(textContent);
      return external({ element: element });
    };
    var external = function (spec) {
      var extSpec = asRawOrDie$1('external.component', objOfOnly([
        required$1('element'),
        option('uid')
      ]), spec);
      var systemApi = Cell(NoContextApi());
      var connect = function (newApi) {
        systemApi.set(newApi);
      };
      var disconnect = function () {
        systemApi.set(NoContextApi(function () {
          return me;
        }));
      };
      var uid = extSpec.uid.getOrThunk(function () {
        return generate$2('external');
      });
      writeOnly(extSpec.element, uid);
      var me = {
        uid: uid,
        getSystem: systemApi.get,
        config: Optional.none,
        hasConfigured: never,
        connect: connect,
        disconnect: disconnect,
        getApis: function () {
          return {};
        },
        element: extSpec.element,
        spec: spec,
        readState: constant$1('No state'),
        syncComponents: noop,
        components: constant$1([]),
        events: {}
      };
      return premade$1(me);
    };
    var uids = generate$2;
    var isSketchSpec = function (spec) {
      return has$2(spec, 'uid');
    };
    var build$1 = function (spec) {
      return getPremade(spec).getOrThunk(function () {
        var userSpecWithUid = isSketchSpec(spec) ? spec : __assign({ uid: uids('') }, spec);
        return buildFromSpec(userSpecWithUid).getOrDie();
      });
    };
    var premade = premade$1;

    var hoverEvent = 'alloy.item-hover';
    var focusEvent = 'alloy.item-focus';
    var onHover = function (item) {
      if (search(item.element).isNone() || Focusing.isFocused(item)) {
        if (!Focusing.isFocused(item)) {
          Focusing.focus(item);
        }
        emitWith(item, hoverEvent, { item: item });
      }
    };
    var onFocus = function (item) {
      emitWith(item, focusEvent, { item: item });
    };
    var hover = constant$1(hoverEvent);
    var focus$1 = constant$1(focusEvent);

    var builder$2 = function (detail) {
      return {
        dom: detail.dom,
        domModification: __assign(__assign({}, detail.domModification), { attributes: __assign(__assign(__assign({ 'role': detail.toggling.isSome() ? 'menuitemcheckbox' : 'menuitem' }, detail.domModification.attributes), { 'aria-haspopup': detail.hasSubmenu }), detail.hasSubmenu ? { 'aria-expanded': false } : {}) }),
        behaviours: SketchBehaviours.augment(detail.itemBehaviours, [
          detail.toggling.fold(Toggling.revoke, function (tConfig) {
            return Toggling.config(__assign({ aria: { mode: 'checked' } }, tConfig));
          }),
          Focusing.config({
            ignore: detail.ignoreFocus,
            stopMousedown: detail.ignoreFocus,
            onFocus: function (component) {
              onFocus(component);
            }
          }),
          Keying.config({ mode: 'execution' }),
          Representing.config({
            store: {
              mode: 'memory',
              initialValue: detail.data
            }
          }),
          config('item-type-events', __spreadArray(__spreadArray([], pointerEvents(), true), [
            run(mouseover(), onHover),
            run(focusItem(), Focusing.focus)
          ], false))
        ]),
        components: detail.components,
        eventOrder: detail.eventOrder
      };
    };
    var schema$5 = [
      required$1('data'),
      required$1('components'),
      required$1('dom'),
      defaulted('hasSubmenu', false),
      option('toggling'),
      SketchBehaviours.field('itemBehaviours', [
        Toggling,
        Focusing,
        Keying,
        Representing
      ]),
      defaulted('ignoreFocus', false),
      defaulted('domModification', {}),
      output('builder', builder$2),
      defaulted('eventOrder', {})
    ];

    var builder$1 = function (detail) {
      return {
        dom: detail.dom,
        components: detail.components,
        events: derive$3([stopper(focusItem())])
      };
    };
    var schema$4 = [
      required$1('dom'),
      required$1('components'),
      output('builder', builder$1)
    ];

    var owner = constant$1('item-widget');
    var parts$3 = constant$1([required({
        name: 'widget',
        overrides: function (detail) {
          return {
            behaviours: derive$2([Representing.config({
                store: {
                  mode: 'manual',
                  getValue: function (_component) {
                    return detail.data;
                  },
                  setValue: noop
                }
              })])
          };
        }
      })]);

    var builder = function (detail) {
      var subs = substitutes(owner(), detail, parts$3());
      var components$1 = components(owner(), detail, subs.internals());
      var focusWidget = function (component) {
        return getPart(component, detail, 'widget').map(function (widget) {
          Keying.focusIn(widget);
          return widget;
        });
      };
      var onHorizontalArrow = function (component, simulatedEvent) {
        return inside(simulatedEvent.event.target) ? Optional.none() : function () {
          if (detail.autofocus) {
            simulatedEvent.setSource(component.element);
            return Optional.none();
          } else {
            return Optional.none();
          }
        }();
      };
      return {
        dom: detail.dom,
        components: components$1,
        domModification: detail.domModification,
        events: derive$3([
          runOnExecute(function (component, simulatedEvent) {
            focusWidget(component).each(function (_widget) {
              simulatedEvent.stop();
            });
          }),
          run(mouseover(), onHover),
          run(focusItem(), function (component, _simulatedEvent) {
            if (detail.autofocus) {
              focusWidget(component);
            } else {
              Focusing.focus(component);
            }
          })
        ]),
        behaviours: SketchBehaviours.augment(detail.widgetBehaviours, [
          Representing.config({
            store: {
              mode: 'memory',
              initialValue: detail.data
            }
          }),
          Focusing.config({
            ignore: detail.ignoreFocus,
            onFocus: function (component) {
              onFocus(component);
            }
          }),
          Keying.config({
            mode: 'special',
            focusIn: detail.autofocus ? function (component) {
              focusWidget(component);
            } : revoke(),
            onLeft: onHorizontalArrow,
            onRight: onHorizontalArrow,
            onEscape: function (component, simulatedEvent) {
              if (!Focusing.isFocused(component) && !detail.autofocus) {
                Focusing.focus(component);
                return Optional.some(true);
              } else if (detail.autofocus) {
                simulatedEvent.setSource(component.element);
                return Optional.none();
              } else {
                return Optional.none();
              }
            }
          })
        ])
      };
    };
    var schema$3 = [
      required$1('uid'),
      required$1('data'),
      required$1('components'),
      required$1('dom'),
      defaulted('autofocus', false),
      defaulted('ignoreFocus', false),
      SketchBehaviours.field('widgetBehaviours', [
        Representing,
        Focusing,
        Keying
      ]),
      defaulted('domModification', {}),
      defaultUidsSchema(parts$3()),
      output('builder', builder)
    ];

    var itemSchema = choose$1('type', {
      widget: schema$3,
      item: schema$5,
      separator: schema$4
    });
    var configureGrid = function (detail, movementInfo) {
      return {
        mode: 'flatgrid',
        selector: '.' + detail.markers.item,
        initSize: {
          numColumns: movementInfo.initSize.numColumns,
          numRows: movementInfo.initSize.numRows
        },
        focusManager: detail.focusManager
      };
    };
    var configureMatrix = function (detail, movementInfo) {
      return {
        mode: 'matrix',
        selectors: {
          row: movementInfo.rowSelector,
          cell: '.' + detail.markers.item
        },
        focusManager: detail.focusManager
      };
    };
    var configureMenu = function (detail, movementInfo) {
      return {
        mode: 'menu',
        selector: '.' + detail.markers.item,
        moveOnTab: movementInfo.moveOnTab,
        focusManager: detail.focusManager
      };
    };
    var parts$2 = constant$1([group({
        factory: {
          sketch: function (spec) {
            var itemInfo = asRawOrDie$1('menu.spec item', itemSchema, spec);
            return itemInfo.builder(itemInfo);
          }
        },
        name: 'items',
        unit: 'item',
        defaults: function (detail, u) {
          return has$2(u, 'uid') ? u : __assign(__assign({}, u), { uid: generate$2('item') });
        },
        overrides: function (detail, u) {
          return {
            type: u.type,
            ignoreFocus: detail.fakeFocus,
            domModification: { classes: [detail.markers.item] }
          };
        }
      })]);
    var schema$2 = constant$1([
      required$1('value'),
      required$1('items'),
      required$1('dom'),
      required$1('components'),
      defaulted('eventOrder', {}),
      field$1('menuBehaviours', [
        Highlighting,
        Representing,
        Composing,
        Keying
      ]),
      defaultedOf('movement', {
        mode: 'menu',
        moveOnTab: true
      }, choose$1('mode', {
        grid: [
          initSize(),
          output('config', configureGrid)
        ],
        matrix: [
          output('config', configureMatrix),
          required$1('rowSelector')
        ],
        menu: [
          defaulted('moveOnTab', true),
          output('config', configureMenu)
        ]
      })),
      itemMarkers(),
      defaulted('fakeFocus', false),
      defaulted('focusManager', dom$2()),
      onHandler('onHighlight')
    ]);

    var focus = constant$1('alloy.menu-focus');

    var make$2 = function (detail, components, _spec, _externals) {
      return {
        uid: detail.uid,
        dom: detail.dom,
        markers: detail.markers,
        behaviours: augment(detail.menuBehaviours, [
          Highlighting.config({
            highlightClass: detail.markers.selectedItem,
            itemClass: detail.markers.item,
            onHighlight: detail.onHighlight
          }),
          Representing.config({
            store: {
              mode: 'memory',
              initialValue: detail.value
            }
          }),
          Composing.config({ find: Optional.some }),
          Keying.config(detail.movement.config(detail, detail.movement))
        ]),
        events: derive$3([
          run(focus$1(), function (menu, simulatedEvent) {
            var event = simulatedEvent.event;
            menu.getSystem().getByDom(event.target).each(function (item) {
              Highlighting.highlight(menu, item);
              simulatedEvent.stop();
              emitWith(menu, focus(), {
                menu: menu,
                item: item
              });
            });
          }),
          run(hover(), function (menu, simulatedEvent) {
            var item = simulatedEvent.event.item;
            Highlighting.highlight(menu, item);
          })
        ]),
        components: components,
        eventOrder: detail.eventOrder,
        domModification: { attributes: { role: 'menu' } }
      };
    };

    var Menu = composite({
      name: 'Menu',
      configFields: schema$2(),
      partFields: parts$2(),
      factory: make$2
    });

    var preserve = function (f, container) {
      var dos = getRootNode(container);
      var refocus = active(dos).bind(function (focused) {
        var hasFocus = function (elem) {
          return eq(focused, elem);
        };
        return hasFocus(container) ? Optional.some(container) : descendant$1(container, hasFocus);
      });
      var result = f(container);
      refocus.each(function (oldFocus) {
        active(dos).filter(function (newFocus) {
          return eq(newFocus, oldFocus);
        }).fold(function () {
          focus$3(oldFocus);
        }, noop);
      });
      return result;
    };

    var set = function (component, replaceConfig, replaceState, data) {
      preserve(function () {
        var newChildren = map$2(data, component.getSystem().build);
        replaceChildren(component, newChildren);
      }, component.element);
    };
    var insert = function (component, replaceConfig, insertion, childSpec) {
      var child = component.getSystem().build(childSpec);
      attachWith(component, child, insertion);
    };
    var append = function (component, replaceConfig, replaceState, appendee) {
      insert(component, replaceConfig, append$2, appendee);
    };
    var prepend = function (component, replaceConfig, replaceState, prependee) {
      insert(component, replaceConfig, prepend$1, prependee);
    };
    var remove = function (component, replaceConfig, replaceState, removee) {
      var children = contents(component);
      var foundChild = find$2(children, function (child) {
        return eq(removee.element, child.element);
      });
      foundChild.each(detach);
    };
    var contents = function (component, _replaceConfig) {
      return component.components();
    };
    var replaceAt = function (component, replaceConfig, replaceState, replaceeIndex, replacer) {
      var children = contents(component);
      return Optional.from(children[replaceeIndex]).map(function (replacee) {
        remove(component, replaceConfig, replaceState, replacee);
        replacer.each(function (r) {
          insert(component, replaceConfig, function (p, c) {
            appendAt(p, c, replaceeIndex);
          }, r);
        });
        return replacee;
      });
    };
    var replaceBy = function (component, replaceConfig, replaceState, replaceePred, replacer) {
      var children = contents(component);
      return findIndex$1(children, replaceePred).bind(function (replaceeIndex) {
        return replaceAt(component, replaceConfig, replaceState, replaceeIndex, replacer);
      });
    };

    var ReplaceApis = /*#__PURE__*/Object.freeze({
        __proto__: null,
        append: append,
        prepend: prepend,
        remove: remove,
        replaceAt: replaceAt,
        replaceBy: replaceBy,
        set: set,
        contents: contents
    });

    var Replacing = create$5({
      fields: [],
      name: 'replacing',
      apis: ReplaceApis
    });

    var transpose = function (obj) {
      return tupleMap(obj, function (v, k) {
        return {
          k: v,
          v: k
        };
      });
    };
    var trace = function (items, byItem, byMenu, finish) {
      return get$c(byMenu, finish).bind(function (triggerItem) {
        return get$c(items, triggerItem).bind(function (triggerMenu) {
          var rest = trace(items, byItem, byMenu, triggerMenu);
          return Optional.some([triggerMenu].concat(rest));
        });
      }).getOr([]);
    };
    var generate = function (menus, expansions) {
      var items = {};
      each(menus, function (menuItems, menu) {
        each$1(menuItems, function (item) {
          items[item] = menu;
        });
      });
      var byItem = expansions;
      var byMenu = transpose(expansions);
      var menuPaths = map$1(byMenu, function (_triggerItem, submenu) {
        return [submenu].concat(trace(items, byItem, byMenu, submenu));
      });
      return map$1(items, function (menu) {
        return get$c(menuPaths, menu).getOr([menu]);
      });
    };

    var init$2 = function () {
      var expansions = Cell({});
      var menus = Cell({});
      var paths = Cell({});
      var primary = value();
      var directory = Cell({});
      var clear = function () {
        expansions.set({});
        menus.set({});
        paths.set({});
        primary.clear();
      };
      var isClear = function () {
        return primary.get().isNone();
      };
      var setMenuBuilt = function (menuName, built) {
        var _a;
        menus.set(__assign(__assign({}, menus.get()), (_a = {}, _a[menuName] = {
          type: 'prepared',
          menu: built
        }, _a)));
      };
      var setContents = function (sPrimary, sMenus, sExpansions, dir) {
        primary.set(sPrimary);
        expansions.set(sExpansions);
        menus.set(sMenus);
        directory.set(dir);
        var sPaths = generate(dir, sExpansions);
        paths.set(sPaths);
      };
      var getTriggeringItem = function (menuValue) {
        return find(expansions.get(), function (v, _k) {
          return v === menuValue;
        });
      };
      var getTriggerData = function (menuValue, getItemByValue, path) {
        return getPreparedMenu(menuValue).bind(function (menu) {
          return getTriggeringItem(menuValue).bind(function (triggeringItemValue) {
            return getItemByValue(triggeringItemValue).map(function (triggeredItem) {
              return {
                triggeredMenu: menu,
                triggeringItem: triggeredItem,
                triggeringPath: path
              };
            });
          });
        });
      };
      var getTriggeringPath = function (itemValue, getItemByValue) {
        var extraPath = filter$2(lookupItem(itemValue).toArray(), function (menuValue) {
          return getPreparedMenu(menuValue).isSome();
        });
        return get$c(paths.get(), itemValue).bind(function (path) {
          var revPath = reverse(extraPath.concat(path));
          var triggers = bind$3(revPath, function (menuValue, menuIndex) {
            return getTriggerData(menuValue, getItemByValue, revPath.slice(0, menuIndex + 1)).fold(function () {
              return is(primary.get(), menuValue) ? [] : [Optional.none()];
            }, function (data) {
              return [Optional.some(data)];
            });
          });
          return sequence(triggers);
        });
      };
      var expand = function (itemValue) {
        return get$c(expansions.get(), itemValue).map(function (menu) {
          var current = get$c(paths.get(), itemValue).getOr([]);
          return [menu].concat(current);
        });
      };
      var collapse = function (itemValue) {
        return get$c(paths.get(), itemValue).bind(function (path) {
          return path.length > 1 ? Optional.some(path.slice(1)) : Optional.none();
        });
      };
      var refresh = function (itemValue) {
        return get$c(paths.get(), itemValue);
      };
      var getPreparedMenu = function (menuValue) {
        return lookupMenu(menuValue).bind(extractPreparedMenu);
      };
      var lookupMenu = function (menuValue) {
        return get$c(menus.get(), menuValue);
      };
      var lookupItem = function (itemValue) {
        return get$c(expansions.get(), itemValue);
      };
      var otherMenus = function (path) {
        var menuValues = directory.get();
        return difference(keys(menuValues), path);
      };
      var getPrimary = function () {
        return primary.get().bind(getPreparedMenu);
      };
      var getMenus = function () {
        return menus.get();
      };
      return {
        setMenuBuilt: setMenuBuilt,
        setContents: setContents,
        expand: expand,
        refresh: refresh,
        collapse: collapse,
        lookupMenu: lookupMenu,
        lookupItem: lookupItem,
        otherMenus: otherMenus,
        getPrimary: getPrimary,
        getMenus: getMenus,
        clear: clear,
        isClear: isClear,
        getTriggeringPath: getTriggeringPath
      };
    };
    var extractPreparedMenu = function (prep) {
      return prep.type === 'prepared' ? Optional.some(prep.menu) : Optional.none();
    };
    var LayeredState = {
      init: init$2,
      extractPreparedMenu: extractPreparedMenu
    };

    var make$1 = function (detail, _rawUiSpec) {
      var submenuParentItems = value();
      var buildMenus = function (container, primaryName, menus) {
        return map$1(menus, function (spec, name) {
          var makeSketch = function () {
            return Menu.sketch(__assign(__assign({}, spec), {
              value: name,
              markers: detail.markers,
              fakeFocus: detail.fakeFocus,
              onHighlight: detail.onHighlight,
              focusManager: detail.fakeFocus ? highlights() : dom$2()
            }));
          };
          return name === primaryName ? {
            type: 'prepared',
            menu: container.getSystem().build(makeSketch())
          } : {
            type: 'notbuilt',
            nbMenu: makeSketch
          };
        });
      };
      var layeredState = LayeredState.init();
      var setup = function (container) {
        var componentMap = buildMenus(container, detail.data.primary, detail.data.menus);
        var directory = toDirectory();
        layeredState.setContents(detail.data.primary, componentMap, detail.data.expansions, directory);
        return layeredState.getPrimary();
      };
      var getItemValue = function (item) {
        return Representing.getValue(item).value;
      };
      var getItemByValue = function (_container, menus, itemValue) {
        return findMap(menus, function (menu) {
          if (!menu.getSystem().isConnected()) {
            return Optional.none();
          }
          var candidates = Highlighting.getCandidates(menu);
          return find$2(candidates, function (c) {
            return getItemValue(c) === itemValue;
          });
        });
      };
      var toDirectory = function (_container) {
        return map$1(detail.data.menus, function (data, _menuName) {
          return bind$3(data.items, function (item) {
            return item.type === 'separator' ? [] : [item.data.value];
          });
        });
      };
      var setActiveMenu = function (container, menu) {
        Highlighting.highlight(container, menu);
        Highlighting.getHighlighted(menu).orThunk(function () {
          return Highlighting.getFirst(menu);
        }).each(function (item) {
          dispatch(container, item.element, focusItem());
        });
      };
      var getMenus = function (state, menuValues) {
        return cat(map$2(menuValues, function (mv) {
          return state.lookupMenu(mv).bind(function (prep) {
            return prep.type === 'prepared' ? Optional.some(prep.menu) : Optional.none();
          });
        }));
      };
      var closeOthers = function (container, state, path) {
        var others = getMenus(state, state.otherMenus(path));
        each$1(others, function (o) {
          remove$1(o.element, [detail.markers.backgroundMenu]);
          if (!detail.stayInDom) {
            Replacing.remove(container, o);
          }
        });
      };
      var getSubmenuParents = function (container) {
        return submenuParentItems.get().getOrThunk(function () {
          var r = {};
          var items = descendants(container.element, '.' + detail.markers.item);
          var parentItems = filter$2(items, function (i) {
            return get$b(i, 'aria-haspopup') === 'true';
          });
          each$1(parentItems, function (i) {
            container.getSystem().getByDom(i).each(function (itemComp) {
              var key = getItemValue(itemComp);
              r[key] = itemComp;
            });
          });
          submenuParentItems.set(r);
          return r;
        });
      };
      var updateAriaExpansions = function (container, path) {
        var parentItems = getSubmenuParents(container);
        each(parentItems, function (v, k) {
          var expanded = contains$1(path, k);
          set$8(v.element, 'aria-expanded', expanded);
        });
      };
      var updateMenuPath = function (container, state, path) {
        return Optional.from(path[0]).bind(function (latestMenuName) {
          return state.lookupMenu(latestMenuName).bind(function (menuPrep) {
            if (menuPrep.type === 'notbuilt') {
              return Optional.none();
            } else {
              var activeMenu = menuPrep.menu;
              var rest = getMenus(state, path.slice(1));
              each$1(rest, function (r) {
                add$1(r.element, detail.markers.backgroundMenu);
              });
              if (!inBody(activeMenu.element)) {
                Replacing.append(container, premade(activeMenu));
              }
              remove$1(activeMenu.element, [detail.markers.backgroundMenu]);
              setActiveMenu(container, activeMenu);
              closeOthers(container, state, path);
              return Optional.some(activeMenu);
            }
          });
        });
      };
      var ExpandHighlightDecision;
      (function (ExpandHighlightDecision) {
        ExpandHighlightDecision[ExpandHighlightDecision['HighlightSubmenu'] = 0] = 'HighlightSubmenu';
        ExpandHighlightDecision[ExpandHighlightDecision['HighlightParent'] = 1] = 'HighlightParent';
      }(ExpandHighlightDecision || (ExpandHighlightDecision = {})));
      var buildIfRequired = function (container, menuName, menuPrep) {
        if (menuPrep.type === 'notbuilt') {
          var menu = container.getSystem().build(menuPrep.nbMenu());
          layeredState.setMenuBuilt(menuName, menu);
          return menu;
        } else {
          return menuPrep.menu;
        }
      };
      var expandRight = function (container, item, decision) {
        if (decision === void 0) {
          decision = ExpandHighlightDecision.HighlightSubmenu;
        }
        if (item.hasConfigured(Disabling) && Disabling.isDisabled(item)) {
          return Optional.some(item);
        } else {
          var value = getItemValue(item);
          return layeredState.expand(value).bind(function (path) {
            updateAriaExpansions(container, path);
            return Optional.from(path[0]).bind(function (menuName) {
              return layeredState.lookupMenu(menuName).bind(function (activeMenuPrep) {
                var activeMenu = buildIfRequired(container, menuName, activeMenuPrep);
                if (!inBody(activeMenu.element)) {
                  Replacing.append(container, premade(activeMenu));
                }
                detail.onOpenSubmenu(container, item, activeMenu, reverse(path));
                if (decision === ExpandHighlightDecision.HighlightSubmenu) {
                  Highlighting.highlightFirst(activeMenu);
                  return updateMenuPath(container, layeredState, path);
                } else {
                  Highlighting.dehighlightAll(activeMenu);
                  return Optional.some(item);
                }
              });
            });
          });
        }
      };
      var collapseLeft = function (container, item) {
        var value = getItemValue(item);
        return layeredState.collapse(value).bind(function (path) {
          updateAriaExpansions(container, path);
          return updateMenuPath(container, layeredState, path).map(function (activeMenu) {
            detail.onCollapseMenu(container, item, activeMenu);
            return activeMenu;
          });
        });
      };
      var updateView = function (container, item) {
        var value = getItemValue(item);
        return layeredState.refresh(value).bind(function (path) {
          updateAriaExpansions(container, path);
          return updateMenuPath(container, layeredState, path);
        });
      };
      var onRight = function (container, item) {
        return inside(item.element) ? Optional.none() : expandRight(container, item, ExpandHighlightDecision.HighlightSubmenu);
      };
      var onLeft = function (container, item) {
        return inside(item.element) ? Optional.none() : collapseLeft(container, item);
      };
      var onEscape = function (container, item) {
        return collapseLeft(container, item).orThunk(function () {
          return detail.onEscape(container, item).map(function () {
            return container;
          });
        });
      };
      var keyOnItem = function (f) {
        return function (container, simulatedEvent) {
          return closest$1(simulatedEvent.getSource(), '.' + detail.markers.item).bind(function (target) {
            return container.getSystem().getByDom(target).toOptional().bind(function (item) {
              return f(container, item).map(always);
            });
          });
        };
      };
      var events = derive$3([
        run(focus(), function (sandbox, simulatedEvent) {
          var item = simulatedEvent.event.item;
          layeredState.lookupItem(getItemValue(item)).each(function () {
            var menu = simulatedEvent.event.menu;
            Highlighting.highlight(sandbox, menu);
            var value = getItemValue(simulatedEvent.event.item);
            layeredState.refresh(value).each(function (path) {
              return closeOthers(sandbox, layeredState, path);
            });
          });
        }),
        runOnExecute(function (component, simulatedEvent) {
          var target = simulatedEvent.event.target;
          component.getSystem().getByDom(target).each(function (item) {
            var itemValue = getItemValue(item);
            if (itemValue.indexOf('collapse-item') === 0) {
              collapseLeft(component, item);
            }
            expandRight(component, item, ExpandHighlightDecision.HighlightSubmenu).fold(function () {
              detail.onExecute(component, item);
            }, noop);
          });
        }),
        runOnAttached(function (container, _simulatedEvent) {
          setup(container).each(function (primary) {
            Replacing.append(container, premade(primary));
            detail.onOpenMenu(container, primary);
            if (detail.highlightImmediately) {
              setActiveMenu(container, primary);
            }
          });
        })
      ].concat(detail.navigateOnHover ? [run(hover(), function (sandbox, simulatedEvent) {
          var item = simulatedEvent.event.item;
          updateView(sandbox, item);
          expandRight(sandbox, item, ExpandHighlightDecision.HighlightParent);
          detail.onHover(sandbox, item);
        })] : []));
      var getActiveItem = function (container) {
        return Highlighting.getHighlighted(container).bind(Highlighting.getHighlighted);
      };
      var collapseMenuApi = function (container) {
        getActiveItem(container).each(function (currentItem) {
          collapseLeft(container, currentItem);
        });
      };
      var highlightPrimary = function (container) {
        layeredState.getPrimary().each(function (primary) {
          setActiveMenu(container, primary);
        });
      };
      var extractMenuFromContainer = function (container) {
        return Optional.from(container.components()[0]).filter(function (comp) {
          return get$b(comp.element, 'role') === 'menu';
        });
      };
      var repositionMenus = function (container) {
        var maybeActivePrimary = layeredState.getPrimary().bind(function (primary) {
          return getActiveItem(container).bind(function (currentItem) {
            var itemValue = getItemValue(currentItem);
            var allMenus = values(layeredState.getMenus());
            var preparedMenus = cat(map$2(allMenus, LayeredState.extractPreparedMenu));
            return layeredState.getTriggeringPath(itemValue, function (v) {
              return getItemByValue(container, preparedMenus, v);
            });
          }).map(function (triggeringPath) {
            return {
              primary: primary,
              triggeringPath: triggeringPath
            };
          });
        });
        maybeActivePrimary.fold(function () {
          extractMenuFromContainer(container).each(function (primaryMenu) {
            detail.onRepositionMenu(container, primaryMenu, []);
          });
        }, function (_a) {
          var primary = _a.primary, triggeringPath = _a.triggeringPath;
          detail.onRepositionMenu(container, primary, triggeringPath);
        });
      };
      var apis = {
        collapseMenu: collapseMenuApi,
        highlightPrimary: highlightPrimary,
        repositionMenus: repositionMenus
      };
      return {
        uid: detail.uid,
        dom: detail.dom,
        markers: detail.markers,
        behaviours: augment(detail.tmenuBehaviours, [
          Keying.config({
            mode: 'special',
            onRight: keyOnItem(onRight),
            onLeft: keyOnItem(onLeft),
            onEscape: keyOnItem(onEscape),
            focusIn: function (container, _keyInfo) {
              layeredState.getPrimary().each(function (primary) {
                dispatch(container, primary.element, focusItem());
              });
            }
          }),
          Highlighting.config({
            highlightClass: detail.markers.selectedMenu,
            itemClass: detail.markers.menu
          }),
          Composing.config({
            find: function (container) {
              return Highlighting.getHighlighted(container);
            }
          }),
          Replacing.config({})
        ]),
        eventOrder: detail.eventOrder,
        apis: apis,
        events: events
      };
    };
    var collapseItem$1 = constant$1('collapse-item');

    var tieredData = function (primary, menus, expansions) {
      return {
        primary: primary,
        menus: menus,
        expansions: expansions
      };
    };
    var singleData = function (name, menu) {
      return {
        primary: name,
        menus: wrap(name, menu),
        expansions: {}
      };
    };
    var collapseItem = function (text) {
      return {
        value: generate$4(collapseItem$1()),
        meta: { text: text }
      };
    };
    var tieredMenu = single({
      name: 'TieredMenu',
      configFields: [
        onStrictKeyboardHandler('onExecute'),
        onStrictKeyboardHandler('onEscape'),
        onStrictHandler('onOpenMenu'),
        onStrictHandler('onOpenSubmenu'),
        onHandler('onRepositionMenu'),
        onHandler('onCollapseMenu'),
        defaulted('highlightImmediately', true),
        requiredObjOf('data', [
          required$1('primary'),
          required$1('menus'),
          required$1('expansions')
        ]),
        defaulted('fakeFocus', false),
        onHandler('onHighlight'),
        onHandler('onHover'),
        tieredMenuMarkers(),
        required$1('dom'),
        defaulted('navigateOnHover', true),
        defaulted('stayInDom', false),
        field$1('tmenuBehaviours', [
          Keying,
          Highlighting,
          Composing,
          Replacing
        ]),
        defaulted('eventOrder', {})
      ],
      apis: {
        collapseMenu: function (apis, tmenu) {
          apis.collapseMenu(tmenu);
        },
        highlightPrimary: function (apis, tmenu) {
          apis.highlightPrimary(tmenu);
        },
        repositionMenus: function (apis, tmenu) {
          apis.repositionMenus(tmenu);
        }
      },
      factory: make$1,
      extraApis: {
        tieredData: tieredData,
        singleData: singleData,
        collapseItem: collapseItem
      }
    });

    var findRoute = function (component, transConfig, transState, route) {
      return get$c(transConfig.routes, route.start).bind(function (sConfig) {
        return get$c(sConfig, route.destination);
      });
    };
    var getTransition = function (comp, transConfig, transState) {
      var route = getCurrentRoute(comp, transConfig);
      return route.bind(function (r) {
        return getTransitionOf(comp, transConfig, transState, r);
      });
    };
    var getTransitionOf = function (comp, transConfig, transState, route) {
      return findRoute(comp, transConfig, transState, route).bind(function (r) {
        return r.transition.map(function (t) {
          return {
            transition: t,
            route: r
          };
        });
      });
    };
    var disableTransition = function (comp, transConfig, transState) {
      getTransition(comp, transConfig, transState).each(function (routeTransition) {
        var t = routeTransition.transition;
        remove$3(comp.element, t.transitionClass);
        remove$6(comp.element, transConfig.destinationAttr);
      });
    };
    var getNewRoute = function (comp, transConfig, transState, destination) {
      return {
        start: get$b(comp.element, transConfig.stateAttr),
        destination: destination
      };
    };
    var getCurrentRoute = function (comp, transConfig, _transState) {
      var el = comp.element;
      return getOpt(el, transConfig.destinationAttr).map(function (destination) {
        return {
          start: get$b(comp.element, transConfig.stateAttr),
          destination: destination
        };
      });
    };
    var jumpTo = function (comp, transConfig, transState, destination) {
      disableTransition(comp, transConfig, transState);
      if (has$1(comp.element, transConfig.stateAttr) && get$b(comp.element, transConfig.stateAttr) !== destination) {
        transConfig.onFinish(comp, destination);
      }
      set$8(comp.element, transConfig.stateAttr, destination);
    };
    var fasttrack = function (comp, transConfig, _transState, _destination) {
      if (has$1(comp.element, transConfig.destinationAttr)) {
        getOpt(comp.element, transConfig.destinationAttr).each(function (destination) {
          set$8(comp.element, transConfig.stateAttr, destination);
        });
        remove$6(comp.element, transConfig.destinationAttr);
      }
    };
    var progressTo = function (comp, transConfig, transState, destination) {
      fasttrack(comp, transConfig);
      var route = getNewRoute(comp, transConfig, transState, destination);
      getTransitionOf(comp, transConfig, transState, route).fold(function () {
        jumpTo(comp, transConfig, transState, destination);
      }, function (routeTransition) {
        disableTransition(comp, transConfig, transState);
        var t = routeTransition.transition;
        add$1(comp.element, t.transitionClass);
        set$8(comp.element, transConfig.destinationAttr, destination);
      });
    };
    var getState = function (comp, transConfig, _transState) {
      return getOpt(comp.element, transConfig.stateAttr);
    };

    var TransitionApis = /*#__PURE__*/Object.freeze({
        __proto__: null,
        findRoute: findRoute,
        disableTransition: disableTransition,
        getCurrentRoute: getCurrentRoute,
        jumpTo: jumpTo,
        progressTo: progressTo,
        getState: getState
    });

    var events$1 = function (transConfig, transState) {
      return derive$3([
        run(transitionend(), function (component, simulatedEvent) {
          var raw = simulatedEvent.event.raw;
          getCurrentRoute(component, transConfig).each(function (route) {
            findRoute(component, transConfig, transState, route).each(function (rInfo) {
              rInfo.transition.each(function (rTransition) {
                if (raw.propertyName === rTransition.property) {
                  jumpTo(component, transConfig, transState, route.destination);
                  transConfig.onTransition(component, route);
                }
              });
            });
          });
        }),
        runOnAttached(function (comp, _se) {
          jumpTo(comp, transConfig, transState, transConfig.initialState);
        })
      ]);
    };

    var ActiveTransitioning = /*#__PURE__*/Object.freeze({
        __proto__: null,
        events: events$1
    });

    var TransitionSchema = [
      defaulted('destinationAttr', 'data-transitioning-destination'),
      defaulted('stateAttr', 'data-transitioning-state'),
      required$1('initialState'),
      onHandler('onTransition'),
      onHandler('onFinish'),
      requiredOf('routes', setOf(Result.value, setOf(Result.value, objOfOnly([optionObjOfOnly('transition', [
          required$1('property'),
          required$1('transitionClass')
        ])]))))
    ];

    var createRoutes = function (routes) {
      var r = {};
      each(routes, function (v, k) {
        var waypoints = k.split('<->');
        r[waypoints[0]] = wrap(waypoints[1], v);
        r[waypoints[1]] = wrap(waypoints[0], v);
      });
      return r;
    };
    var createBistate = function (first, second, transitions) {
      return wrapAll([
        {
          key: first,
          value: wrap(second, transitions)
        },
        {
          key: second,
          value: wrap(first, transitions)
        }
      ]);
    };
    var createTristate = function (first, second, third, transitions) {
      return wrapAll([
        {
          key: first,
          value: wrapAll([
            {
              key: second,
              value: transitions
            },
            {
              key: third,
              value: transitions
            }
          ])
        },
        {
          key: second,
          value: wrapAll([
            {
              key: first,
              value: transitions
            },
            {
              key: third,
              value: transitions
            }
          ])
        },
        {
          key: third,
          value: wrapAll([
            {
              key: first,
              value: transitions
            },
            {
              key: second,
              value: transitions
            }
          ])
        }
      ]);
    };
    var Transitioning = create$5({
      fields: TransitionSchema,
      name: 'transitioning',
      active: ActiveTransitioning,
      apis: TransitionApis,
      extra: {
        createRoutes: createRoutes,
        createBistate: createBistate,
        createTristate: createTristate
      }
    });

    var scrollableStyle = resolve('scrollable');
    var register$2 = function (element) {
      add$1(element, scrollableStyle);
    };
    var deregister = function (element) {
      remove$3(element, scrollableStyle);
    };
    var scrollable = scrollableStyle;

    var getValue = function (item) {
      return get$c(item, 'format').getOr(item.title);
    };
    var convert = function (formats, memMenuThunk) {
      var mainMenu = makeMenu('Styles', [].concat(map$2(formats.items, function (k) {
        return makeItem(getValue(k), k.title, k.isSelected(), k.getPreview(), hasNonNullableKey(formats.expansions, getValue(k)));
      })), memMenuThunk, false);
      var submenus = map$1(formats.menus, function (menuItems, menuName) {
        var items = map$2(menuItems, function (item) {
          return makeItem(getValue(item), item.title, item.isSelected !== undefined ? item.isSelected() : false, item.getPreview !== undefined ? item.getPreview() : '', hasNonNullableKey(formats.expansions, getValue(item)));
        });
        return makeMenu(menuName, items, memMenuThunk, true);
      });
      var menus = deepMerge(submenus, wrap('styles', mainMenu));
      var tmenu = tieredMenu.tieredData('styles', menus, formats.expansions);
      return { tmenu: tmenu };
    };
    var makeItem = function (value, text, selected, preview, isMenu) {
      return {
        data: {
          value: value,
          text: text
        },
        type: 'item',
        dom: {
          tag: 'div',
          classes: isMenu ? [resolve('styles-item-is-menu')] : []
        },
        toggling: {
          toggleOnExecute: false,
          toggleClass: resolve('format-matches'),
          selected: selected
        },
        itemBehaviours: derive$2(isMenu ? [] : [format(value, function (comp, status) {
            var toggle = status ? Toggling.on : Toggling.off;
            toggle(comp);
          })]),
        components: [{
            dom: {
              tag: 'div',
              attributes: { style: preview },
              innerHtml: text
            }
          }]
      };
    };
    var makeMenu = function (value, items, memMenuThunk, collapsable) {
      return {
        value: value,
        dom: { tag: 'div' },
        components: [
          Button.sketch({
            dom: {
              tag: 'div',
              classes: [resolve('styles-collapser')]
            },
            components: collapsable ? [
              {
                dom: {
                  tag: 'span',
                  classes: [resolve('styles-collapse-icon')]
                }
              },
              text(value)
            ] : [text(value)],
            action: function (item) {
              if (collapsable) {
                var comp = memMenuThunk().get(item);
                tieredMenu.collapseMenu(comp);
              }
            }
          }),
          {
            dom: {
              tag: 'div',
              classes: [resolve('styles-menu-items-container')]
            },
            components: [Menu.parts.items({})],
            behaviours: derive$2([config('adhoc-scrollable-menu', [
                runOnAttached(function (component, _simulatedEvent) {
                  set$5(component.element, 'overflow-y', 'auto');
                  set$5(component.element, '-webkit-overflow-scrolling', 'touch');
                  register$2(component.element);
                }),
                runOnDetached(function (component) {
                  remove$2(component.element, 'overflow-y');
                  remove$2(component.element, '-webkit-overflow-scrolling');
                  deregister(component.element);
                })
              ])])
          }
        ],
        items: items,
        menuBehaviours: derive$2([Transitioning.config({
            initialState: 'after',
            routes: Transitioning.createTristate('before', 'current', 'after', {
              transition: {
                property: 'transform',
                transitionClass: 'transitioning'
              }
            })
          })])
      };
    };
    var sketch$1 = function (settings) {
      var dataset = convert(settings.formats, function () {
        return memMenu;
      });
      var memMenu = record(tieredMenu.sketch({
        dom: {
          tag: 'div',
          classes: [resolve('styles-menu')]
        },
        components: [],
        fakeFocus: true,
        stayInDom: true,
        onExecute: function (_tmenu, item) {
          var v = Representing.getValue(item);
          settings.handle(item, v.value);
          return Optional.none();
        },
        onEscape: function () {
          return Optional.none();
        },
        onOpenMenu: function (container, menu) {
          var w = get$5(container.element);
          set$4(menu.element, w);
          Transitioning.jumpTo(menu, 'current');
        },
        onOpenSubmenu: function (container, item, submenu) {
          var w = get$5(container.element);
          var menu = ancestor(item.element, '[role="menu"]').getOrDie('hacky');
          var menuComp = container.getSystem().getByDom(menu).getOrDie();
          set$4(submenu.element, w);
          Transitioning.progressTo(menuComp, 'before');
          Transitioning.jumpTo(submenu, 'after');
          Transitioning.progressTo(submenu, 'current');
        },
        onCollapseMenu: function (container, item, menu) {
          var submenu = ancestor(item.element, '[role="menu"]').getOrDie('hacky');
          var submenuComp = container.getSystem().getByDom(submenu).getOrDie();
          Transitioning.progressTo(submenuComp, 'after');
          Transitioning.progressTo(menu, 'current');
        },
        navigateOnHover: false,
        highlightImmediately: true,
        data: dataset.tmenu,
        markers: {
          backgroundMenu: resolve('styles-background-menu'),
          menu: resolve('styles-menu'),
          selectedMenu: resolve('styles-selected-menu'),
          item: resolve('styles-item'),
          selectedItem: resolve('styles-selected-item')
        }
      }));
      return memMenu.asSpec();
    };

    var getFromExpandingItem = function (item) {
      var newItem = deepMerge(exclude(item, ['items']), { menu: true });
      var rest = expand(item.items);
      var newMenus = deepMerge(rest.menus, wrap(item.title, rest.items));
      var newExpansions = deepMerge(rest.expansions, wrap(item.title, item.title));
      return {
        item: newItem,
        menus: newMenus,
        expansions: newExpansions
      };
    };
    var getFromItem = function (item) {
      return hasNonNullableKey(item, 'items') ? getFromExpandingItem(item) : {
        item: item,
        menus: {},
        expansions: {}
      };
    };
    var expand = function (items) {
      return foldr(items, function (acc, item) {
        var newData = getFromItem(item);
        return {
          menus: deepMerge(acc.menus, newData.menus),
          items: [newData.item].concat(acc.items),
          expansions: deepMerge(acc.expansions, newData.expansions)
        };
      }, {
        menus: {},
        expansions: {},
        items: []
      });
    };

    var register$1 = function (editor) {
      var isSelectedFor = function (format) {
        return function () {
          return editor.formatter.match(format);
        };
      };
      var getPreview = function (format) {
        return function () {
          return editor.formatter.getCssText(format);
        };
      };
      var enrichSupported = function (item) {
        return deepMerge(item, {
          isSelected: isSelectedFor(item.format),
          getPreview: getPreview(item.format)
        });
      };
      var enrichMenu = function (item) {
        return deepMerge(item, {
          isSelected: never,
          getPreview: constant$1('')
        });
      };
      var enrichCustom = function (item) {
        var formatName = generate$4(item.title);
        var newItem = deepMerge(item, {
          format: formatName,
          isSelected: isSelectedFor(formatName),
          getPreview: getPreview(formatName)
        });
        editor.formatter.register(formatName, newItem);
        return newItem;
      };
      var doEnrich = function (items) {
        return map$2(items, function (item) {
          if (hasNonNullableKey(item, 'items')) {
            var newItems = doEnrich(item.items);
            return deepMerge(enrichMenu(item), { items: newItems });
          } else if (hasNonNullableKey(item, 'format')) {
            return enrichSupported(item);
          } else {
            return enrichCustom(item);
          }
        });
      };
      return doEnrich(getStyleFormats(editor));
    };
    var prune = function (editor, formats) {
      var doPrune = function (items) {
        return bind$3(items, function (item) {
          if (item.items !== undefined) {
            var newItems = doPrune(item.items);
            return newItems.length > 0 ? [item] : [];
          } else {
            var keep = hasNonNullableKey(item, 'format') ? editor.formatter.canApply(item.format) : true;
            return keep ? [item] : [];
          }
        });
      };
      var prunedItems = doPrune(formats);
      return expand(prunedItems);
    };
    var ui = function (editor, formats, onDone) {
      var pruned = prune(editor, formats);
      return sketch$1({
        formats: pruned,
        handle: function (item, value) {
          editor.undoManager.transact(function () {
            if (Toggling.isOn(item)) {
              editor.formatter.remove(value);
            } else {
              editor.formatter.apply(value);
            }
          });
          onDone();
        }
      });
    };

    var extract = function (rawToolbar) {
      var toolbar = rawToolbar.replace(/\|/g, ' ').trim();
      return toolbar.length > 0 ? toolbar.split(/\s+/) : [];
    };
    var identifyFromArray = function (toolbar) {
      return bind$3(toolbar, function (item) {
        return isArray(item) ? identifyFromArray(item) : extract(item);
      });
    };
    var identify = function (editor) {
      var toolbar = getToolbar(editor);
      return isArray(toolbar) ? identifyFromArray(toolbar) : extract(toolbar);
    };
    var setup$3 = function (realm, editor) {
      var commandSketch = function (name) {
        return function () {
          return forToolbarCommand(editor, name);
        };
      };
      var stateCommandSketch = function (name) {
        return function () {
          return forToolbarStateCommand(editor, name);
        };
      };
      var actionSketch = function (name, query, action) {
        return function () {
          return forToolbarStateAction(editor, name, query, action);
        };
      };
      var undo = commandSketch('undo');
      var redo = commandSketch('redo');
      var bold = stateCommandSketch('bold');
      var italic = stateCommandSketch('italic');
      var underline = stateCommandSketch('underline');
      var removeformat = commandSketch('removeformat');
      var link = function () {
        return sketch$2(realm, editor);
      };
      var unlink = actionSketch('unlink', 'link', function () {
        editor.execCommand('unlink', null, false);
      });
      var image = function () {
        return sketch$5(editor);
      };
      var bullist = actionSketch('unordered-list', 'ul', function () {
        editor.execCommand('InsertUnorderedList', null, false);
      });
      var numlist = actionSketch('ordered-list', 'ol', function () {
        editor.execCommand('InsertOrderedList', null, false);
      });
      var fontsizeselect = function () {
        return sketch$6(realm, editor);
      };
      var forecolor = function () {
        return sketch$8(realm, editor);
      };
      var styleFormats = register$1(editor);
      var styleFormatsMenu = function () {
        return ui(editor, styleFormats, function () {
          editor.fire('scrollIntoView');
        });
      };
      var styleselect = function () {
        return forToolbar('style-formats', function (button) {
          editor.fire('toReading');
          realm.dropup.appear(styleFormatsMenu, Toggling.on, button);
        }, derive$2([
          Toggling.config({
            toggleClass: resolve('toolbar-button-selected'),
            toggleOnExecute: false,
            aria: { mode: 'pressed' }
          }),
          Receiving.config({
            channels: wrapAll([
              receive(orientationChanged, Toggling.off),
              receive(dropupDismissed, Toggling.off)
            ])
          })
        ]), editor);
      };
      var feature = function (prereq, sketch) {
        return {
          isSupported: function () {
            var buttons = editor.ui.registry.getAll().buttons;
            return prereq.forall(function (p) {
              return hasNonNullableKey(buttons, p);
            });
          },
          sketch: sketch
        };
      };
      return {
        undo: feature(Optional.none(), undo),
        redo: feature(Optional.none(), redo),
        bold: feature(Optional.none(), bold),
        italic: feature(Optional.none(), italic),
        underline: feature(Optional.none(), underline),
        removeformat: feature(Optional.none(), removeformat),
        link: feature(Optional.none(), link),
        unlink: feature(Optional.none(), unlink),
        image: feature(Optional.none(), image),
        bullist: feature(Optional.some('bullist'), bullist),
        numlist: feature(Optional.some('numlist'), numlist),
        fontsizeselect: feature(Optional.none(), fontsizeselect),
        forecolor: feature(Optional.none(), forecolor),
        styleselect: feature(Optional.none(), styleselect)
      };
    };
    var detect = function (editor, features) {
      var itemNames = identify(editor);
      var present = {};
      return bind$3(itemNames, function (iName) {
        var r = !hasNonNullableKey(present, iName) && hasNonNullableKey(features, iName) && features[iName].isSupported() ? [features[iName].sketch()] : [];
        present[iName] = true;
        return r;
      });
    };

    var mkEvent = function (target, x, y, stop, prevent, kill, raw) {
      return {
        target: target,
        x: x,
        y: y,
        stop: stop,
        prevent: prevent,
        kill: kill,
        raw: raw
      };
    };
    var fromRawEvent = function (rawEvent) {
      var target = SugarElement.fromDom(getOriginalEventTarget(rawEvent).getOr(rawEvent.target));
      var stop = function () {
        return rawEvent.stopPropagation();
      };
      var prevent = function () {
        return rawEvent.preventDefault();
      };
      var kill = compose(prevent, stop);
      return mkEvent(target, rawEvent.clientX, rawEvent.clientY, stop, prevent, kill, rawEvent);
    };
    var handle = function (filter, handler) {
      return function (rawEvent) {
        if (filter(rawEvent)) {
          handler(fromRawEvent(rawEvent));
        }
      };
    };
    var binder = function (element, event, filter, handler, useCapture) {
      var wrapped = handle(filter, handler);
      element.dom.addEventListener(event, wrapped, useCapture);
      return { unbind: curry(unbind, element, event, wrapped, useCapture) };
    };
    var bind$1 = function (element, event, filter, handler) {
      return binder(element, event, filter, handler, false);
    };
    var capture$1 = function (element, event, filter, handler) {
      return binder(element, event, filter, handler, true);
    };
    var unbind = function (element, event, handler, useCapture) {
      element.dom.removeEventListener(event, handler, useCapture);
    };

    var filter = always;
    var bind = function (element, event, handler) {
      return bind$1(element, event, filter, handler);
    };
    var capture = function (element, event, handler) {
      return capture$1(element, event, filter, handler);
    };

    var global$2 = tinymce.util.Tools.resolve('tinymce.util.Delay');

    var INTERVAL = 50;
    var INSURANCE = 1000 / INTERVAL;
    var get$1 = function (outerWindow) {
      var isPortrait = outerWindow.matchMedia('(orientation: portrait)').matches;
      return { isPortrait: constant$1(isPortrait) };
    };
    var getActualWidth = function (outerWindow) {
      var isIos = detect$1().os.isiOS();
      var isPortrait = get$1(outerWindow).isPortrait();
      return isIos && !isPortrait ? outerWindow.screen.height : outerWindow.screen.width;
    };
    var onChange = function (outerWindow, listeners) {
      var win = SugarElement.fromDom(outerWindow);
      var poller = null;
      var change = function () {
        global$2.clearInterval(poller);
        var orientation = get$1(outerWindow);
        listeners.onChange(orientation);
        onAdjustment(function () {
          listeners.onReady(orientation);
        });
      };
      var orientationHandle = bind(win, 'orientationchange', change);
      var onAdjustment = function (f) {
        global$2.clearInterval(poller);
        var flag = outerWindow.innerHeight;
        var insurance = 0;
        poller = global$2.setInterval(function () {
          if (flag !== outerWindow.innerHeight) {
            global$2.clearInterval(poller);
            f(Optional.some(outerWindow.innerHeight));
          } else if (insurance > INSURANCE) {
            global$2.clearInterval(poller);
            f(Optional.none());
          }
          insurance++;
        }, INTERVAL);
      };
      var destroy = function () {
        orientationHandle.unbind();
      };
      return {
        onAdjustment: onAdjustment,
        destroy: destroy
      };
    };

    var setStart = function (rng, situ) {
      situ.fold(function (e) {
        rng.setStartBefore(e.dom);
      }, function (e, o) {
        rng.setStart(e.dom, o);
      }, function (e) {
        rng.setStartAfter(e.dom);
      });
    };
    var setFinish = function (rng, situ) {
      situ.fold(function (e) {
        rng.setEndBefore(e.dom);
      }, function (e, o) {
        rng.setEnd(e.dom, o);
      }, function (e) {
        rng.setEndAfter(e.dom);
      });
    };
    var relativeToNative = function (win, startSitu, finishSitu) {
      var range = win.document.createRange();
      setStart(range, startSitu);
      setFinish(range, finishSitu);
      return range;
    };
    var exactToNative = function (win, start, soffset, finish, foffset) {
      var rng = win.document.createRange();
      rng.setStart(start.dom, soffset);
      rng.setEnd(finish.dom, foffset);
      return rng;
    };
    var toRect$1 = function (rect) {
      return {
        left: rect.left,
        top: rect.top,
        right: rect.right,
        bottom: rect.bottom,
        width: rect.width,
        height: rect.height
      };
    };
    var getFirstRect$1 = function (rng) {
      var rects = rng.getClientRects();
      var rect = rects.length > 0 ? rects[0] : rng.getBoundingClientRect();
      return rect.width > 0 || rect.height > 0 ? Optional.some(rect).map(toRect$1) : Optional.none();
    };

    var adt$3 = Adt.generate([
      {
        ltr: [
          'start',
          'soffset',
          'finish',
          'foffset'
        ]
      },
      {
        rtl: [
          'start',
          'soffset',
          'finish',
          'foffset'
        ]
      }
    ]);
    var fromRange = function (win, type, range) {
      return type(SugarElement.fromDom(range.startContainer), range.startOffset, SugarElement.fromDom(range.endContainer), range.endOffset);
    };
    var getRanges = function (win, selection) {
      return selection.match({
        domRange: function (rng) {
          return {
            ltr: constant$1(rng),
            rtl: Optional.none
          };
        },
        relative: function (startSitu, finishSitu) {
          return {
            ltr: cached(function () {
              return relativeToNative(win, startSitu, finishSitu);
            }),
            rtl: cached(function () {
              return Optional.some(relativeToNative(win, finishSitu, startSitu));
            })
          };
        },
        exact: function (start, soffset, finish, foffset) {
          return {
            ltr: cached(function () {
              return exactToNative(win, start, soffset, finish, foffset);
            }),
            rtl: cached(function () {
              return Optional.some(exactToNative(win, finish, foffset, start, soffset));
            })
          };
        }
      });
    };
    var doDiagnose = function (win, ranges) {
      var rng = ranges.ltr();
      if (rng.collapsed) {
        var reversed = ranges.rtl().filter(function (rev) {
          return rev.collapsed === false;
        });
        return reversed.map(function (rev) {
          return adt$3.rtl(SugarElement.fromDom(rev.endContainer), rev.endOffset, SugarElement.fromDom(rev.startContainer), rev.startOffset);
        }).getOrThunk(function () {
          return fromRange(win, adt$3.ltr, rng);
        });
      } else {
        return fromRange(win, adt$3.ltr, rng);
      }
    };
    var diagnose = function (win, selection) {
      var ranges = getRanges(win, selection);
      return doDiagnose(win, ranges);
    };
    var asLtrRange = function (win, selection) {
      var diagnosis = diagnose(win, selection);
      return diagnosis.match({
        ltr: function (start, soffset, finish, foffset) {
          var rng = win.document.createRange();
          rng.setStart(start.dom, soffset);
          rng.setEnd(finish.dom, foffset);
          return rng;
        },
        rtl: function (start, soffset, finish, foffset) {
          var rng = win.document.createRange();
          rng.setStart(finish.dom, foffset);
          rng.setEnd(start.dom, soffset);
          return rng;
        }
      });
    };
    adt$3.ltr;
    adt$3.rtl;

    var create$3 = function (start, soffset, finish, foffset) {
      return {
        start: start,
        soffset: soffset,
        finish: finish,
        foffset: foffset
      };
    };
    var SimRange = { create: create$3 };

    var NodeValue = function (is, name) {
      var get = function (element) {
        if (!is(element)) {
          throw new Error('Can only get ' + name + ' value of a ' + name + ' node');
        }
        return getOption(element).getOr('');
      };
      var getOption = function (element) {
        return is(element) ? Optional.from(element.dom.nodeValue) : Optional.none();
      };
      var set = function (element, value) {
        if (!is(element)) {
          throw new Error('Can only set raw ' + name + ' value of a ' + name + ' node');
        }
        element.dom.nodeValue = value;
      };
      return {
        get: get,
        getOption: getOption,
        set: set
      };
    };

    var api = NodeValue(isText, 'text');
    var getOption = function (element) {
      return api.getOption(element);
    };

    var getEnd = function (element) {
      return name$1(element) === 'img' ? 1 : getOption(element).fold(function () {
        return children(element).length;
      }, function (v) {
        return v.length;
      });
    };

    var adt$2 = Adt.generate([
      { before: ['element'] },
      {
        on: [
          'element',
          'offset'
        ]
      },
      { after: ['element'] }
    ]);
    var cata = function (subject, onBefore, onOn, onAfter) {
      return subject.fold(onBefore, onOn, onAfter);
    };
    var getStart$1 = function (situ) {
      return situ.fold(identity, identity, identity);
    };
    var before = adt$2.before;
    var on = adt$2.on;
    var after$1 = adt$2.after;
    var Situ = {
      before: before,
      on: on,
      after: after$1,
      cata: cata,
      getStart: getStart$1
    };

    var adt$1 = Adt.generate([
      { domRange: ['rng'] },
      {
        relative: [
          'startSitu',
          'finishSitu'
        ]
      },
      {
        exact: [
          'start',
          'soffset',
          'finish',
          'foffset'
        ]
      }
    ]);
    var exactFromRange = function (simRange) {
      return adt$1.exact(simRange.start, simRange.soffset, simRange.finish, simRange.foffset);
    };
    var getStart = function (selection) {
      return selection.match({
        domRange: function (rng) {
          return SugarElement.fromDom(rng.startContainer);
        },
        relative: function (startSitu, _finishSitu) {
          return Situ.getStart(startSitu);
        },
        exact: function (start, _soffset, _finish, _foffset) {
          return start;
        }
      });
    };
    var domRange = adt$1.domRange;
    var relative = adt$1.relative;
    var exact = adt$1.exact;
    var getWin$1 = function (selection) {
      var start = getStart(selection);
      return defaultView(start);
    };
    var range = SimRange.create;
    var SimSelection = {
      domRange: domRange,
      relative: relative,
      exact: exact,
      exactFromRange: exactFromRange,
      getWin: getWin$1,
      range: range
    };

    var beforeSpecial = function (element, offset) {
      var name = name$1(element);
      if ('input' === name) {
        return Situ.after(element);
      } else if (!contains$1([
          'br',
          'img'
        ], name)) {
        return Situ.on(element, offset);
      } else {
        return offset === 0 ? Situ.before(element) : Situ.after(element);
      }
    };
    var preprocessExact = function (start, soffset, finish, foffset) {
      var startSitu = beforeSpecial(start, soffset);
      var finishSitu = beforeSpecial(finish, foffset);
      return SimSelection.relative(startSitu, finishSitu);
    };

    var makeRange = function (start, soffset, finish, foffset) {
      var doc = owner$2(start);
      var rng = doc.dom.createRange();
      rng.setStart(start.dom, soffset);
      rng.setEnd(finish.dom, foffset);
      return rng;
    };
    var after = function (start, soffset, finish, foffset) {
      var r = makeRange(start, soffset, finish, foffset);
      var same = eq(start, finish) && soffset === foffset;
      return r.collapsed && !same;
    };

    var getNativeSelection = function (win) {
      return Optional.from(win.getSelection());
    };
    var doSetNativeRange = function (win, rng) {
      getNativeSelection(win).each(function (selection) {
        selection.removeAllRanges();
        selection.addRange(rng);
      });
    };
    var doSetRange = function (win, start, soffset, finish, foffset) {
      var rng = exactToNative(win, start, soffset, finish, foffset);
      doSetNativeRange(win, rng);
    };
    var setLegacyRtlRange = function (win, selection, start, soffset, finish, foffset) {
      selection.collapse(start.dom, soffset);
      selection.extend(finish.dom, foffset);
    };
    var setRangeFromRelative = function (win, relative) {
      return diagnose(win, relative).match({
        ltr: function (start, soffset, finish, foffset) {
          doSetRange(win, start, soffset, finish, foffset);
        },
        rtl: function (start, soffset, finish, foffset) {
          getNativeSelection(win).each(function (selection) {
            if (selection.setBaseAndExtent) {
              selection.setBaseAndExtent(start.dom, soffset, finish.dom, foffset);
            } else if (selection.extend) {
              try {
                setLegacyRtlRange(win, selection, start, soffset, finish, foffset);
              } catch (e) {
                doSetRange(win, finish, foffset, start, soffset);
              }
            } else {
              doSetRange(win, finish, foffset, start, soffset);
            }
          });
        }
      });
    };
    var setExact = function (win, start, soffset, finish, foffset) {
      var relative = preprocessExact(start, soffset, finish, foffset);
      setRangeFromRelative(win, relative);
    };
    var readRange = function (selection) {
      if (selection.rangeCount > 0) {
        var firstRng = selection.getRangeAt(0);
        var lastRng = selection.getRangeAt(selection.rangeCount - 1);
        return Optional.some(SimRange.create(SugarElement.fromDom(firstRng.startContainer), firstRng.startOffset, SugarElement.fromDom(lastRng.endContainer), lastRng.endOffset));
      } else {
        return Optional.none();
      }
    };
    var doGetExact = function (selection) {
      if (selection.anchorNode === null || selection.focusNode === null) {
        return readRange(selection);
      } else {
        var anchor = SugarElement.fromDom(selection.anchorNode);
        var focus_1 = SugarElement.fromDom(selection.focusNode);
        return after(anchor, selection.anchorOffset, focus_1, selection.focusOffset) ? Optional.some(SimRange.create(anchor, selection.anchorOffset, focus_1, selection.focusOffset)) : readRange(selection);
      }
    };
    var getExact = function (win) {
      return getNativeSelection(win).filter(function (sel) {
        return sel.rangeCount > 0;
      }).bind(doGetExact);
    };
    var get = function (win) {
      return getExact(win).map(function (range) {
        return SimSelection.exact(range.start, range.soffset, range.finish, range.foffset);
      });
    };
    var getFirstRect = function (win, selection) {
      var rng = asLtrRange(win, selection);
      return getFirstRect$1(rng);
    };
    var clear = function (win) {
      getNativeSelection(win).each(function (selection) {
        return selection.removeAllRanges();
      });
    };

    var getBodyFromFrame = function (frame) {
      return Optional.some(SugarElement.fromDom(frame.dom.contentWindow.document.body));
    };
    var getDocFromFrame = function (frame) {
      return Optional.some(SugarElement.fromDom(frame.dom.contentWindow.document));
    };
    var getWinFromFrame = function (frame) {
      return Optional.from(frame.dom.contentWindow);
    };
    var getSelectionFromFrame = function (frame) {
      var optWin = getWinFromFrame(frame);
      return optWin.bind(getExact);
    };
    var getFrame = function (editor) {
      return editor.getFrame();
    };
    var getOrDerive = function (name, f) {
      return function (editor) {
        var g = editor[name].getOrThunk(function () {
          var frame = getFrame(editor);
          return function () {
            return f(frame);
          };
        });
        return g();
      };
    };
    var getOrListen = function (editor, doc, name, type) {
      return editor[name].getOrThunk(function () {
        return function (handler) {
          return bind(doc, type, handler);
        };
      });
    };
    var getActiveApi = function (editor) {
      var frame = getFrame(editor);
      var tryFallbackBox = function (win) {
        var isCollapsed = function (sel) {
          return eq(sel.start, sel.finish) && sel.soffset === sel.foffset;
        };
        var toStartRect = function (sel) {
          var rect = sel.start.dom.getBoundingClientRect();
          return rect.width > 0 || rect.height > 0 ? Optional.some(rect) : Optional.none();
        };
        return getExact(win).filter(isCollapsed).bind(toStartRect);
      };
      return getBodyFromFrame(frame).bind(function (body) {
        return getDocFromFrame(frame).bind(function (doc) {
          return getWinFromFrame(frame).map(function (win) {
            var html = SugarElement.fromDom(doc.dom.documentElement);
            var getCursorBox = editor.getCursorBox.getOrThunk(function () {
              return function () {
                return get(win).bind(function (sel) {
                  return getFirstRect(win, sel).orThunk(function () {
                    return tryFallbackBox(win);
                  });
                });
              };
            });
            var setSelection = editor.setSelection.getOrThunk(function () {
              return function (start, soffset, finish, foffset) {
                setExact(win, start, soffset, finish, foffset);
              };
            });
            var clearSelection = editor.clearSelection.getOrThunk(function () {
              return function () {
                clear(win);
              };
            });
            return {
              body: body,
              doc: doc,
              win: win,
              html: html,
              getSelection: curry(getSelectionFromFrame, frame),
              setSelection: setSelection,
              clearSelection: clearSelection,
              frame: frame,
              onKeyup: getOrListen(editor, doc, 'onKeyup', 'keyup'),
              onNodeChanged: getOrListen(editor, doc, 'onNodeChanged', 'SelectionChange'),
              onDomChanged: editor.onDomChanged,
              onScrollToCursor: editor.onScrollToCursor,
              onScrollToElement: editor.onScrollToElement,
              onToReading: editor.onToReading,
              onToEditing: editor.onToEditing,
              onToolbarScrollStart: editor.onToolbarScrollStart,
              onTouchContent: editor.onTouchContent,
              onTapContent: editor.onTapContent,
              onTouchToolstrip: editor.onTouchToolstrip,
              getCursorBox: getCursorBox
            };
          });
        });
      });
    };
    var getWin = getOrDerive('getWin', getWinFromFrame);

    var tag = function () {
      var head = first$1('head').getOrDie();
      var nu = function () {
        var meta = SugarElement.fromTag('meta');
        set$8(meta, 'name', 'viewport');
        append$2(head, meta);
        return meta;
      };
      var element = first$1('meta[name="viewport"]').getOrThunk(nu);
      var backup = get$b(element, 'content');
      var maximize = function () {
        set$8(element, 'content', 'width=device-width, initial-scale=1.0, user-scalable=no, maximum-scale=1.0');
      };
      var restore = function () {
        if (backup !== undefined && backup !== null && backup.length > 0) {
          set$8(element, 'content', backup);
        } else {
          set$8(element, 'content', 'user-scalable=yes');
        }
      };
      return {
        maximize: maximize,
        restore: restore
      };
    };

    var attr = 'data-ephox-mobile-fullscreen-style';
    var siblingStyles = 'display:none!important;';
    var ancestorPosition = 'position:absolute!important;';
    var ancestorStyles = 'top:0!important;left:0!important;margin:0!important;padding:0!important;width:100%!important;height:100%!important;overflow:visible!important;';
    var bgFallback = 'background-color:rgb(255,255,255)!important;';
    var isAndroid = detect$1().os.isAndroid();
    var matchColor = function (editorBody) {
      var color = get$8(editorBody, 'background-color');
      return color !== undefined && color !== '' ? 'background-color:' + color + '!important' : bgFallback;
    };
    var clobberStyles = function (container, editorBody) {
      var gatherSiblings = function (element) {
        return siblings(element, '*');
      };
      var clobber = function (clobberStyle) {
        return function (element) {
          var styles = get$b(element, 'style');
          var backup = styles === undefined ? 'no-styles' : styles.trim();
          if (backup === clobberStyle) {
            return;
          } else {
            set$8(element, attr, backup);
            set$8(element, 'style', clobberStyle);
          }
        };
      };
      var ancestors$1 = ancestors(container, '*');
      var siblings$1 = bind$3(ancestors$1, gatherSiblings);
      var bgColor = matchColor(editorBody);
      each$1(siblings$1, clobber(siblingStyles));
      each$1(ancestors$1, clobber(ancestorPosition + ancestorStyles + bgColor));
      var containerStyles = isAndroid === true ? '' : ancestorPosition;
      clobber(containerStyles + ancestorStyles + bgColor)(container);
    };
    var restoreStyles = function () {
      var clobberedEls = all('[' + attr + ']');
      each$1(clobberedEls, function (element) {
        var restore = get$b(element, attr);
        if (restore !== 'no-styles') {
          set$8(element, 'style', restore);
        } else {
          remove$6(element, 'style');
        }
        remove$6(element, attr);
      });
    };

    var DelayedFunction = function (fun, delay) {
      var ref = null;
      var schedule = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        ref = setTimeout(function () {
          fun.apply(null, args);
          ref = null;
        }, delay);
      };
      var cancel = function () {
        if (ref !== null) {
          clearTimeout(ref);
          ref = null;
        }
      };
      return {
        cancel: cancel,
        schedule: schedule
      };
    };

    var SIGNIFICANT_MOVE = 5;
    var LONGPRESS_DELAY = 400;
    var getTouch = function (event) {
      var raw = event.raw;
      if (raw.touches === undefined || raw.touches.length !== 1) {
        return Optional.none();
      }
      return Optional.some(raw.touches[0]);
    };
    var isFarEnough = function (touch, data) {
      var distX = Math.abs(touch.clientX - data.x);
      var distY = Math.abs(touch.clientY - data.y);
      return distX > SIGNIFICANT_MOVE || distY > SIGNIFICANT_MOVE;
    };
    var monitor$1 = function (settings) {
      var startData = value();
      var longpressFired = Cell(false);
      var longpress$1 = DelayedFunction(function (event) {
        settings.triggerEvent(longpress(), event);
        longpressFired.set(true);
      }, LONGPRESS_DELAY);
      var handleTouchstart = function (event) {
        getTouch(event).each(function (touch) {
          longpress$1.cancel();
          var data = {
            x: touch.clientX,
            y: touch.clientY,
            target: event.target
          };
          longpress$1.schedule(event);
          longpressFired.set(false);
          startData.set(data);
        });
        return Optional.none();
      };
      var handleTouchmove = function (event) {
        longpress$1.cancel();
        getTouch(event).each(function (touch) {
          startData.on(function (data) {
            if (isFarEnough(touch, data)) {
              startData.clear();
            }
          });
        });
        return Optional.none();
      };
      var handleTouchend = function (event) {
        longpress$1.cancel();
        var isSame = function (data) {
          return eq(data.target, event.target);
        };
        return startData.get().filter(isSame).map(function (_data) {
          if (longpressFired.get()) {
            event.prevent();
            return false;
          } else {
            return settings.triggerEvent(tap(), event);
          }
        });
      };
      var handlers = wrapAll([
        {
          key: touchstart(),
          value: handleTouchstart
        },
        {
          key: touchmove(),
          value: handleTouchmove
        },
        {
          key: touchend(),
          value: handleTouchend
        }
      ]);
      var fireIfReady = function (event, type) {
        return get$c(handlers, type).bind(function (handler) {
          return handler(event);
        });
      };
      return { fireIfReady: fireIfReady };
    };

    var monitor = function (editorApi) {
      var tapEvent = monitor$1({
        triggerEvent: function (type, evt) {
          editorApi.onTapContent(evt);
        }
      });
      var onTouchend = function () {
        return bind(editorApi.body, 'touchend', function (evt) {
          tapEvent.fireIfReady(evt, 'touchend');
        });
      };
      var onTouchmove = function () {
        return bind(editorApi.body, 'touchmove', function (evt) {
          tapEvent.fireIfReady(evt, 'touchmove');
        });
      };
      var fireTouchstart = function (evt) {
        tapEvent.fireIfReady(evt, 'touchstart');
      };
      return {
        fireTouchstart: fireTouchstart,
        onTouchend: onTouchend,
        onTouchmove: onTouchmove
      };
    };

    var isAndroid6 = detect$1().os.version.major >= 6;
    var initEvents$1 = function (editorApi, toolstrip, alloy) {
      var tapping = monitor(editorApi);
      var outerDoc = owner$2(toolstrip);
      var isRanged = function (sel) {
        return !eq(sel.start, sel.finish) || sel.soffset !== sel.foffset;
      };
      var hasRangeInUi = function () {
        return active(outerDoc).filter(function (input) {
          return name$1(input) === 'input';
        }).exists(function (input) {
          return input.dom.selectionStart !== input.dom.selectionEnd;
        });
      };
      var updateMargin = function () {
        var rangeInContent = editorApi.doc.dom.hasFocus() && editorApi.getSelection().exists(isRanged);
        alloy.getByDom(toolstrip).each((rangeInContent || hasRangeInUi()) === true ? Toggling.on : Toggling.off);
      };
      var listeners = [
        bind(editorApi.body, 'touchstart', function (evt) {
          editorApi.onTouchContent();
          tapping.fireTouchstart(evt);
        }),
        tapping.onTouchmove(),
        tapping.onTouchend(),
        bind(toolstrip, 'touchstart', function (_evt) {
          editorApi.onTouchToolstrip();
        }),
        editorApi.onToReading(function () {
          blur$1(editorApi.body);
        }),
        editorApi.onToEditing(noop),
        editorApi.onScrollToCursor(function (tinyEvent) {
          tinyEvent.preventDefault();
          editorApi.getCursorBox().each(function (bounds) {
            var cWin = editorApi.win;
            var isOutside = bounds.top > cWin.innerHeight || bounds.bottom > cWin.innerHeight;
            var cScrollBy = isOutside ? bounds.bottom - cWin.innerHeight + 50 : 0;
            if (cScrollBy !== 0) {
              cWin.scrollTo(cWin.pageXOffset, cWin.pageYOffset + cScrollBy);
            }
          });
        })
      ].concat(isAndroid6 === true ? [] : [
        bind(SugarElement.fromDom(editorApi.win), 'blur', function () {
          alloy.getByDom(toolstrip).each(Toggling.off);
        }),
        bind(outerDoc, 'select', updateMargin),
        bind(editorApi.doc, 'selectionchange', updateMargin)
      ]);
      var destroy = function () {
        each$1(listeners, function (l) {
          l.unbind();
        });
      };
      return { destroy: destroy };
    };

    var safeParse = function (element, attribute) {
      var parsed = parseInt(get$b(element, attribute), 10);
      return isNaN(parsed) ? 0 : parsed;
    };

    var COLLAPSED_WIDTH = 2;
    var collapsedRect = function (rect) {
      return __assign(__assign({}, rect), { width: COLLAPSED_WIDTH });
    };
    var toRect = function (rawRect) {
      return {
        left: rawRect.left,
        top: rawRect.top,
        right: rawRect.right,
        bottom: rawRect.bottom,
        width: rawRect.width,
        height: rawRect.height
      };
    };
    var getRectsFromRange = function (range) {
      if (!range.collapsed) {
        return map$2(range.getClientRects(), toRect);
      } else {
        var start_1 = SugarElement.fromDom(range.startContainer);
        return parent(start_1).bind(function (parent) {
          var selection = SimSelection.exact(start_1, range.startOffset, parent, getEnd(parent));
          var optRect = getFirstRect(range.startContainer.ownerDocument.defaultView, selection);
          return optRect.map(collapsedRect).map(pure$2);
        }).getOr([]);
      }
    };
    var getRectangles = function (cWin) {
      var sel = cWin.getSelection();
      return sel !== undefined && sel.rangeCount > 0 ? getRectsFromRange(sel.getRangeAt(0)) : [];
    };

    var autocompleteHack = function () {
      return function (f) {
        global$2.setTimeout(function () {
          f();
        }, 0);
      };
    };
    var resume$1 = function (cWin) {
      cWin.focus();
      var iBody = SugarElement.fromDom(cWin.document.body);
      var inInput = active().exists(function (elem) {
        return contains$1([
          'input',
          'textarea'
        ], name$1(elem));
      });
      var transaction = inInput ? autocompleteHack() : apply$1;
      transaction(function () {
        active().each(blur$1);
        focus$3(iBody);
      });
    };

    var EXTRA_SPACING = 50;
    var data = 'data-' + resolve('last-outer-height');
    var setLastHeight = function (cBody, value) {
      set$8(cBody, data, value);
    };
    var getLastHeight = function (cBody) {
      return safeParse(cBody, data);
    };
    var getBoundsFrom = function (rect) {
      return {
        top: rect.top,
        bottom: rect.top + rect.height
      };
    };
    var getBounds = function (cWin) {
      var rects = getRectangles(cWin);
      return rects.length > 0 ? Optional.some(rects[0]).map(getBoundsFrom) : Optional.none();
    };
    var findDelta = function (outerWindow, cBody) {
      var last = getLastHeight(cBody);
      var current = outerWindow.innerHeight;
      return last > current ? Optional.some(last - current) : Optional.none();
    };
    var calculate = function (cWin, bounds, delta) {
      var isOutside = bounds.top > cWin.innerHeight || bounds.bottom > cWin.innerHeight;
      return isOutside ? Math.min(delta, bounds.bottom - cWin.innerHeight + EXTRA_SPACING) : 0;
    };
    var setup$2 = function (outerWindow, cWin) {
      var cBody = SugarElement.fromDom(cWin.document.body);
      var toEditing = function () {
        resume$1(cWin);
      };
      var onResize = bind(SugarElement.fromDom(outerWindow), 'resize', function () {
        findDelta(outerWindow, cBody).each(function (delta) {
          getBounds(cWin).each(function (bounds) {
            var cScrollBy = calculate(cWin, bounds, delta);
            if (cScrollBy !== 0) {
              cWin.scrollTo(cWin.pageXOffset, cWin.pageYOffset + cScrollBy);
            }
          });
        });
        setLastHeight(cBody, outerWindow.innerHeight);
      });
      setLastHeight(cBody, outerWindow.innerHeight);
      var destroy = function () {
        onResize.unbind();
      };
      return {
        toEditing: toEditing,
        destroy: destroy
      };
    };

    var create$2 = function (platform, mask) {
      var meta = tag();
      var androidApi = api$2();
      var androidEvents = api$2();
      var enter = function () {
        mask.hide();
        add$1(platform.container, resolve('fullscreen-maximized'));
        add$1(platform.container, resolve('android-maximized'));
        meta.maximize();
        add$1(platform.body, resolve('android-scroll-reload'));
        androidApi.set(setup$2(platform.win, getWin(platform.editor).getOrDie('no')));
        getActiveApi(platform.editor).each(function (editorApi) {
          clobberStyles(platform.container, editorApi.body);
          androidEvents.set(initEvents$1(editorApi, platform.toolstrip, platform.alloy));
        });
      };
      var exit = function () {
        meta.restore();
        mask.show();
        remove$3(platform.container, resolve('fullscreen-maximized'));
        remove$3(platform.container, resolve('android-maximized'));
        restoreStyles();
        remove$3(platform.body, resolve('android-scroll-reload'));
        androidEvents.clear();
        androidApi.clear();
      };
      return {
        enter: enter,
        exit: exit
      };
    };

    var first = function (fn, rate) {
      var timer = null;
      var cancel = function () {
        if (!isNull(timer)) {
          clearTimeout(timer);
          timer = null;
        }
      };
      var throttle = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        if (isNull(timer)) {
          timer = setTimeout(function () {
            timer = null;
            fn.apply(null, args);
          }, rate);
        }
      };
      return {
        cancel: cancel,
        throttle: throttle
      };
    };
    var last = function (fn, rate) {
      var timer = null;
      var cancel = function () {
        if (!isNull(timer)) {
          clearTimeout(timer);
          timer = null;
        }
      };
      var throttle = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        cancel();
        timer = setTimeout(function () {
          timer = null;
          fn.apply(null, args);
        }, rate);
      };
      return {
        cancel: cancel,
        throttle: throttle
      };
    };

    var sketch = function (onView, _translate) {
      var memIcon = record(Container.sketch({
        dom: dom$1('<div aria-hidden="true" class="${prefix}-mask-tap-icon"></div>'),
        containerBehaviours: derive$2([Toggling.config({
            toggleClass: resolve('mask-tap-icon-selected'),
            toggleOnExecute: false
          })])
      }));
      var onViewThrottle = first(onView, 200);
      return Container.sketch({
        dom: dom$1('<div class="${prefix}-disabled-mask"></div>'),
        components: [Container.sketch({
            dom: dom$1('<div class="${prefix}-content-container"></div>'),
            components: [Button.sketch({
                dom: dom$1('<div class="${prefix}-content-tap-section"></div>'),
                components: [memIcon.asSpec()],
                action: function (_button) {
                  onViewThrottle.throttle();
                },
                buttonBehaviours: derive$2([Toggling.config({ toggleClass: resolve('mask-tap-icon-selected') })])
              })]
          })]
      });
    };

    var unbindNoop = constant$1({ unbind: noop });
    var MobileSchema = objOf([
      requiredObjOf('editor', [
        required$1('getFrame'),
        option('getBody'),
        option('getDoc'),
        option('getWin'),
        option('getSelection'),
        option('setSelection'),
        option('clearSelection'),
        option('cursorSaver'),
        option('onKeyup'),
        option('onNodeChanged'),
        option('getCursorBox'),
        required$1('onDomChanged'),
        defaulted('onTouchContent', noop),
        defaulted('onTapContent', noop),
        defaulted('onTouchToolstrip', noop),
        defaulted('onScrollToCursor', unbindNoop),
        defaulted('onScrollToElement', unbindNoop),
        defaulted('onToEditing', unbindNoop),
        defaulted('onToReading', unbindNoop),
        defaulted('onToolbarScrollStart', identity)
      ]),
      required$1('socket'),
      required$1('toolstrip'),
      required$1('dropup'),
      required$1('toolbar'),
      required$1('container'),
      required$1('alloy'),
      customField('win', function (spec) {
        return owner$2(spec.socket).dom.defaultView;
      }),
      customField('body', function (spec) {
        return SugarElement.fromDom(spec.socket.dom.ownerDocument.body);
      }),
      defaulted('translate', identity),
      defaulted('setReadOnly', noop),
      defaulted('readOnlyOnInit', always)
    ]);

    var produce$1 = function (raw) {
      var mobile = asRawOrDie$1('Getting AndroidWebapp schema', MobileSchema, raw);
      set$5(mobile.toolstrip, 'width', '100%');
      var onTap = function () {
        mobile.setReadOnly(mobile.readOnlyOnInit());
        mode.enter();
      };
      var mask = build$1(sketch(onTap, mobile.translate));
      mobile.alloy.add(mask);
      var maskApi = {
        show: function () {
          mobile.alloy.add(mask);
        },
        hide: function () {
          mobile.alloy.remove(mask);
        }
      };
      append$2(mobile.container, mask.element);
      var mode = create$2(mobile, maskApi);
      return {
        setReadOnly: mobile.setReadOnly,
        refreshStructure: noop,
        enter: mode.enter,
        exit: mode.exit,
        destroy: noop
      };
    };

    var schema$1 = constant$1([
      required$1('dom'),
      defaulted('shell', true),
      field$1('toolbarBehaviours', [Replacing])
    ]);
    var enhanceGroups = function () {
      return { behaviours: derive$2([Replacing.config({})]) };
    };
    var parts$1 = constant$1([optional({
        name: 'groups',
        overrides: enhanceGroups
      })]);

    var factory$1 = function (detail, components, _spec, _externals) {
      var setGroups = function (toolbar, groups) {
        getGroupContainer(toolbar).fold(function () {
          console.error('Toolbar was defined to not be a shell, but no groups container was specified in components');
          throw new Error('Toolbar was defined to not be a shell, but no groups container was specified in components');
        }, function (container) {
          Replacing.set(container, groups);
        });
      };
      var getGroupContainer = function (component) {
        return detail.shell ? Optional.some(component) : getPart(component, detail, 'groups');
      };
      var extra = detail.shell ? {
        behaviours: [Replacing.config({})],
        components: []
      } : {
        behaviours: [],
        components: components
      };
      return {
        uid: detail.uid,
        dom: detail.dom,
        components: extra.components,
        behaviours: augment(detail.toolbarBehaviours, extra.behaviours),
        apis: { setGroups: setGroups },
        domModification: { attributes: { role: 'group' } }
      };
    };
    var Toolbar = composite({
      name: 'Toolbar',
      configFields: schema$1(),
      partFields: parts$1(),
      factory: factory$1,
      apis: {
        setGroups: function (apis, toolbar, groups) {
          apis.setGroups(toolbar, groups);
        }
      }
    });

    var schema = constant$1([
      required$1('items'),
      markers(['itemSelector']),
      field$1('tgroupBehaviours', [Keying])
    ]);
    var parts = constant$1([group({
        name: 'items',
        unit: 'item'
      })]);

    var factory = function (detail, components, _spec, _externals) {
      return {
        uid: detail.uid,
        dom: detail.dom,
        components: components,
        behaviours: augment(detail.tgroupBehaviours, [Keying.config({
            mode: 'flow',
            selector: detail.markers.itemSelector
          })]),
        domModification: { attributes: { role: 'toolbar' } }
      };
    };
    var ToolbarGroup = composite({
      name: 'ToolbarGroup',
      configFields: schema(),
      partFields: parts(),
      factory: factory
    });

    var dataHorizontal = 'data-' + resolve('horizontal-scroll');
    var canScrollVertically = function (container) {
      container.dom.scrollTop = 1;
      var result = container.dom.scrollTop !== 0;
      container.dom.scrollTop = 0;
      return result;
    };
    var canScrollHorizontally = function (container) {
      container.dom.scrollLeft = 1;
      var result = container.dom.scrollLeft !== 0;
      container.dom.scrollLeft = 0;
      return result;
    };
    var hasVerticalScroll = function (container) {
      return container.dom.scrollTop > 0 || canScrollVertically(container);
    };
    var hasHorizontalScroll = function (container) {
      return container.dom.scrollLeft > 0 || canScrollHorizontally(container);
    };
    var markAsHorizontal = function (container) {
      set$8(container, dataHorizontal, 'true');
    };
    var hasScroll = function (container) {
      return get$b(container, dataHorizontal) === 'true' ? hasHorizontalScroll(container) : hasVerticalScroll(container);
    };
    var exclusive = function (scope, selector) {
      return bind(scope, 'touchmove', function (event) {
        closest$1(event.target, selector).filter(hasScroll).fold(function () {
          event.prevent();
        }, noop);
      });
    };

    var ScrollingToolbar = function () {
      var makeGroup = function (gSpec) {
        var scrollClass = gSpec.scrollable === true ? '${prefix}-toolbar-scrollable-group' : '';
        return {
          dom: dom$1('<div aria-label="' + gSpec.label + '" class="${prefix}-toolbar-group ' + scrollClass + '"></div>'),
          tgroupBehaviours: derive$2([config('adhoc-scrollable-toolbar', gSpec.scrollable === true ? [runOnInit(function (component, _simulatedEvent) {
                set$5(component.element, 'overflow-x', 'auto');
                markAsHorizontal(component.element);
                register$2(component.element);
              })] : [])]),
          components: [Container.sketch({ components: [ToolbarGroup.parts.items({})] })],
          markers: { itemSelector: '.' + resolve('toolbar-group-item') },
          items: gSpec.items
        };
      };
      var toolbar = build$1(Toolbar.sketch({
        dom: dom$1('<div class="${prefix}-toolbar"></div>'),
        components: [Toolbar.parts.groups({})],
        toolbarBehaviours: derive$2([
          Toggling.config({
            toggleClass: resolve('context-toolbar'),
            toggleOnExecute: false,
            aria: { mode: 'none' }
          }),
          Keying.config({ mode: 'cyclic' })
        ]),
        shell: true
      }));
      var wrapper = build$1(Container.sketch({
        dom: { classes: [resolve('toolstrip')] },
        components: [premade(toolbar)],
        containerBehaviours: derive$2([Toggling.config({
            toggleClass: resolve('android-selection-context-toolbar'),
            toggleOnExecute: false
          })])
      }));
      var resetGroups = function () {
        Toolbar.setGroups(toolbar, initGroups.get());
        Toggling.off(toolbar);
      };
      var initGroups = Cell([]);
      var setGroups = function (gs) {
        initGroups.set(gs);
        resetGroups();
      };
      var createGroups = function (gs) {
        return map$2(gs, compose(ToolbarGroup.sketch, makeGroup));
      };
      var refresh = function () {
      };
      var setContextToolbar = function (gs) {
        Toggling.on(toolbar);
        Toolbar.setGroups(toolbar, gs);
      };
      var restoreToolbar = function () {
        if (Toggling.isOn(toolbar)) {
          resetGroups();
        }
      };
      var focus = function () {
        Keying.focusIn(toolbar);
      };
      return {
        wrapper: wrapper,
        toolbar: toolbar,
        createGroups: createGroups,
        setGroups: setGroups,
        setContextToolbar: setContextToolbar,
        restoreToolbar: restoreToolbar,
        refresh: refresh,
        focus: focus
      };
    };

    var makeEditSwitch = function (webapp) {
      return build$1(Button.sketch({
        dom: dom$1('<div class="${prefix}-mask-edit-icon ${prefix}-icon"></div>'),
        action: function () {
          webapp.run(function (w) {
            w.setReadOnly(false);
          });
        }
      }));
    };
    var makeSocket = function () {
      return build$1(Container.sketch({
        dom: dom$1('<div class="${prefix}-editor-socket"></div>'),
        components: [],
        containerBehaviours: derive$2([Replacing.config({})])
      }));
    };
    var showEdit = function (socket, switchToEdit) {
      Replacing.append(socket, premade(switchToEdit));
    };
    var hideEdit = function (socket, switchToEdit) {
      Replacing.remove(socket, switchToEdit);
    };
    var updateMode = function (socket, switchToEdit, readOnly, root) {
      var swap = readOnly === true ? Swapping.toAlpha : Swapping.toOmega;
      swap(root);
      var f = readOnly ? showEdit : hideEdit;
      f(socket, switchToEdit);
    };

    var getAnimationRoot = function (component, slideConfig) {
      return slideConfig.getAnimationRoot.fold(function () {
        return component.element;
      }, function (get) {
        return get(component);
      });
    };

    var getDimensionProperty = function (slideConfig) {
      return slideConfig.dimension.property;
    };
    var getDimension = function (slideConfig, elem) {
      return slideConfig.dimension.getDimension(elem);
    };
    var disableTransitions = function (component, slideConfig) {
      var root = getAnimationRoot(component, slideConfig);
      remove$1(root, [
        slideConfig.shrinkingClass,
        slideConfig.growingClass
      ]);
    };
    var setShrunk = function (component, slideConfig) {
      remove$3(component.element, slideConfig.openClass);
      add$1(component.element, slideConfig.closedClass);
      set$5(component.element, getDimensionProperty(slideConfig), '0px');
      reflow(component.element);
    };
    var setGrown = function (component, slideConfig) {
      remove$3(component.element, slideConfig.closedClass);
      add$1(component.element, slideConfig.openClass);
      remove$2(component.element, getDimensionProperty(slideConfig));
    };
    var doImmediateShrink = function (component, slideConfig, slideState, _calculatedSize) {
      slideState.setCollapsed();
      set$5(component.element, getDimensionProperty(slideConfig), getDimension(slideConfig, component.element));
      reflow(component.element);
      disableTransitions(component, slideConfig);
      setShrunk(component, slideConfig);
      slideConfig.onStartShrink(component);
      slideConfig.onShrunk(component);
    };
    var doStartShrink = function (component, slideConfig, slideState, calculatedSize) {
      var size = calculatedSize.getOrThunk(function () {
        return getDimension(slideConfig, component.element);
      });
      slideState.setCollapsed();
      set$5(component.element, getDimensionProperty(slideConfig), size);
      reflow(component.element);
      var root = getAnimationRoot(component, slideConfig);
      remove$3(root, slideConfig.growingClass);
      add$1(root, slideConfig.shrinkingClass);
      setShrunk(component, slideConfig);
      slideConfig.onStartShrink(component);
    };
    var doStartSmartShrink = function (component, slideConfig, slideState) {
      var size = getDimension(slideConfig, component.element);
      var shrinker = size === '0px' ? doImmediateShrink : doStartShrink;
      shrinker(component, slideConfig, slideState, Optional.some(size));
    };
    var doStartGrow = function (component, slideConfig, slideState) {
      var root = getAnimationRoot(component, slideConfig);
      var wasShrinking = has(root, slideConfig.shrinkingClass);
      var beforeSize = getDimension(slideConfig, component.element);
      setGrown(component, slideConfig);
      var fullSize = getDimension(slideConfig, component.element);
      var startPartialGrow = function () {
        set$5(component.element, getDimensionProperty(slideConfig), beforeSize);
        reflow(component.element);
      };
      var startCompleteGrow = function () {
        setShrunk(component, slideConfig);
      };
      var setStartSize = wasShrinking ? startPartialGrow : startCompleteGrow;
      setStartSize();
      remove$3(root, slideConfig.shrinkingClass);
      add$1(root, slideConfig.growingClass);
      setGrown(component, slideConfig);
      set$5(component.element, getDimensionProperty(slideConfig), fullSize);
      slideState.setExpanded();
      slideConfig.onStartGrow(component);
    };
    var refresh$1 = function (component, slideConfig, slideState) {
      if (slideState.isExpanded()) {
        remove$2(component.element, getDimensionProperty(slideConfig));
        var fullSize = getDimension(slideConfig, component.element);
        set$5(component.element, getDimensionProperty(slideConfig), fullSize);
      }
    };
    var grow = function (component, slideConfig, slideState) {
      if (!slideState.isExpanded()) {
        doStartGrow(component, slideConfig, slideState);
      }
    };
    var shrink = function (component, slideConfig, slideState) {
      if (slideState.isExpanded()) {
        doStartSmartShrink(component, slideConfig, slideState);
      }
    };
    var immediateShrink = function (component, slideConfig, slideState) {
      if (slideState.isExpanded()) {
        doImmediateShrink(component, slideConfig, slideState);
      }
    };
    var hasGrown = function (component, slideConfig, slideState) {
      return slideState.isExpanded();
    };
    var hasShrunk = function (component, slideConfig, slideState) {
      return slideState.isCollapsed();
    };
    var isGrowing = function (component, slideConfig, _slideState) {
      var root = getAnimationRoot(component, slideConfig);
      return has(root, slideConfig.growingClass) === true;
    };
    var isShrinking = function (component, slideConfig, _slideState) {
      var root = getAnimationRoot(component, slideConfig);
      return has(root, slideConfig.shrinkingClass) === true;
    };
    var isTransitioning = function (component, slideConfig, slideState) {
      return isGrowing(component, slideConfig) || isShrinking(component, slideConfig);
    };
    var toggleGrow = function (component, slideConfig, slideState) {
      var f = slideState.isExpanded() ? doStartSmartShrink : doStartGrow;
      f(component, slideConfig, slideState);
    };

    var SlidingApis = /*#__PURE__*/Object.freeze({
        __proto__: null,
        refresh: refresh$1,
        grow: grow,
        shrink: shrink,
        immediateShrink: immediateShrink,
        hasGrown: hasGrown,
        hasShrunk: hasShrunk,
        isGrowing: isGrowing,
        isShrinking: isShrinking,
        isTransitioning: isTransitioning,
        toggleGrow: toggleGrow,
        disableTransitions: disableTransitions
    });

    var exhibit = function (base, slideConfig, _slideState) {
      var expanded = slideConfig.expanded;
      return expanded ? nu$3({
        classes: [slideConfig.openClass],
        styles: {}
      }) : nu$3({
        classes: [slideConfig.closedClass],
        styles: wrap(slideConfig.dimension.property, '0px')
      });
    };
    var events = function (slideConfig, slideState) {
      return derive$3([runOnSource(transitionend(), function (component, simulatedEvent) {
          var raw = simulatedEvent.event.raw;
          if (raw.propertyName === slideConfig.dimension.property) {
            disableTransitions(component, slideConfig);
            if (slideState.isExpanded()) {
              remove$2(component.element, slideConfig.dimension.property);
            }
            var notify = slideState.isExpanded() ? slideConfig.onGrown : slideConfig.onShrunk;
            notify(component);
          }
        })]);
    };

    var ActiveSliding = /*#__PURE__*/Object.freeze({
        __proto__: null,
        exhibit: exhibit,
        events: events
    });

    var SlidingSchema = [
      required$1('closedClass'),
      required$1('openClass'),
      required$1('shrinkingClass'),
      required$1('growingClass'),
      option('getAnimationRoot'),
      onHandler('onShrunk'),
      onHandler('onStartShrink'),
      onHandler('onGrown'),
      onHandler('onStartGrow'),
      defaulted('expanded', false),
      requiredOf('dimension', choose$1('property', {
        width: [
          output('property', 'width'),
          output('getDimension', function (elem) {
            return get$5(elem) + 'px';
          })
        ],
        height: [
          output('property', 'height'),
          output('getDimension', function (elem) {
            return get$7(elem) + 'px';
          })
        ]
      }))
    ];

    var init$1 = function (spec) {
      var state = Cell(spec.expanded);
      var readState = function () {
        return 'expanded: ' + state.get();
      };
      return nu$2({
        isExpanded: function () {
          return state.get() === true;
        },
        isCollapsed: function () {
          return state.get() === false;
        },
        setCollapsed: curry(state.set, false),
        setExpanded: curry(state.set, true),
        readState: readState
      });
    };

    var SlidingState = /*#__PURE__*/Object.freeze({
        __proto__: null,
        init: init$1
    });

    var Sliding = create$5({
      fields: SlidingSchema,
      name: 'sliding',
      active: ActiveSliding,
      apis: SlidingApis,
      state: SlidingState
    });

    var build = function (refresh, scrollIntoView) {
      var dropup = build$1(Container.sketch({
        dom: {
          tag: 'div',
          classes: [resolve('dropup')]
        },
        components: [],
        containerBehaviours: derive$2([
          Replacing.config({}),
          Sliding.config({
            closedClass: resolve('dropup-closed'),
            openClass: resolve('dropup-open'),
            shrinkingClass: resolve('dropup-shrinking'),
            growingClass: resolve('dropup-growing'),
            dimension: { property: 'height' },
            onShrunk: function (component) {
              refresh();
              scrollIntoView();
              Replacing.set(component, []);
            },
            onGrown: function (_component) {
              refresh();
              scrollIntoView();
            }
          }),
          orientation(function (_component, _data) {
            disappear(noop);
          })
        ])
      }));
      var appear = function (menu, update, component) {
        if (Sliding.hasShrunk(dropup) === true && Sliding.isTransitioning(dropup) === false) {
          window.requestAnimationFrame(function () {
            update(component);
            Replacing.set(dropup, [menu()]);
            Sliding.grow(dropup);
          });
        }
      };
      var disappear = function (onReadyToShrink) {
        window.requestAnimationFrame(function () {
          onReadyToShrink();
          Sliding.shrink(dropup);
        });
      };
      return {
        appear: appear,
        disappear: disappear,
        component: dropup,
        element: dropup.element
      };
    };

    var closest = function (scope, selector, isRoot) {
      return closest$1(scope, selector, isRoot).isSome();
    };

    var isDangerous = function (event) {
      var keyEv = event.raw;
      return keyEv.which === BACKSPACE[0] && !contains$1([
        'input',
        'textarea'
      ], name$1(event.target)) && !closest(event.target, '[contenteditable="true"]');
    };
    var isFirefox = function () {
      return detect$1().browser.isFirefox();
    };
    var bindFocus = function (container, handler) {
      if (isFirefox()) {
        return capture(container, 'focus', handler);
      } else {
        return bind(container, 'focusin', handler);
      }
    };
    var bindBlur = function (container, handler) {
      if (isFirefox()) {
        return capture(container, 'blur', handler);
      } else {
        return bind(container, 'focusout', handler);
      }
    };
    var setup$1 = function (container, rawSettings) {
      var settings = __assign({ stopBackspace: true }, rawSettings);
      var pointerEvents = [
        'touchstart',
        'touchmove',
        'touchend',
        'touchcancel',
        'gesturestart',
        'mousedown',
        'mouseup',
        'mouseover',
        'mousemove',
        'mouseout',
        'click'
      ];
      var tapEvent = monitor$1(settings);
      var simpleEvents = map$2(pointerEvents.concat([
        'selectstart',
        'input',
        'contextmenu',
        'change',
        'transitionend',
        'transitioncancel',
        'drag',
        'dragstart',
        'dragend',
        'dragenter',
        'dragleave',
        'dragover',
        'drop',
        'keyup'
      ]), function (type) {
        return bind(container, type, function (event) {
          tapEvent.fireIfReady(event, type).each(function (tapStopped) {
            if (tapStopped) {
              event.kill();
            }
          });
          var stopped = settings.triggerEvent(type, event);
          if (stopped) {
            event.kill();
          }
        });
      });
      var pasteTimeout = value();
      var onPaste = bind(container, 'paste', function (event) {
        tapEvent.fireIfReady(event, 'paste').each(function (tapStopped) {
          if (tapStopped) {
            event.kill();
          }
        });
        var stopped = settings.triggerEvent('paste', event);
        if (stopped) {
          event.kill();
        }
        pasteTimeout.set(setTimeout(function () {
          settings.triggerEvent(postPaste(), event);
        }, 0));
      });
      var onKeydown = bind(container, 'keydown', function (event) {
        var stopped = settings.triggerEvent('keydown', event);
        if (stopped) {
          event.kill();
        } else if (settings.stopBackspace && isDangerous(event)) {
          event.prevent();
        }
      });
      var onFocusIn = bindFocus(container, function (event) {
        var stopped = settings.triggerEvent('focusin', event);
        if (stopped) {
          event.kill();
        }
      });
      var focusoutTimeout = value();
      var onFocusOut = bindBlur(container, function (event) {
        var stopped = settings.triggerEvent('focusout', event);
        if (stopped) {
          event.kill();
        }
        focusoutTimeout.set(setTimeout(function () {
          settings.triggerEvent(postBlur(), event);
        }, 0));
      });
      var unbind = function () {
        each$1(simpleEvents, function (e) {
          e.unbind();
        });
        onKeydown.unbind();
        onFocusIn.unbind();
        onFocusOut.unbind();
        onPaste.unbind();
        pasteTimeout.on(clearTimeout);
        focusoutTimeout.on(clearTimeout);
      };
      return { unbind: unbind };
    };

    var derive$1 = function (rawEvent, rawTarget) {
      var source = get$c(rawEvent, 'target').getOr(rawTarget);
      return Cell(source);
    };

    var fromSource = function (event, source) {
      var stopper = Cell(false);
      var cutter = Cell(false);
      var stop = function () {
        stopper.set(true);
      };
      var cut = function () {
        cutter.set(true);
      };
      return {
        stop: stop,
        cut: cut,
        isStopped: stopper.get,
        isCut: cutter.get,
        event: event,
        setSource: source.set,
        getSource: source.get
      };
    };
    var fromExternal = function (event) {
      var stopper = Cell(false);
      var stop = function () {
        stopper.set(true);
      };
      return {
        stop: stop,
        cut: noop,
        isStopped: stopper.get,
        isCut: never,
        event: event,
        setSource: die('Cannot set source of a broadcasted event'),
        getSource: die('Cannot get source of a broadcasted event')
      };
    };

    var adt = Adt.generate([
      { stopped: [] },
      { resume: ['element'] },
      { complete: [] }
    ]);
    var doTriggerHandler = function (lookup, eventType, rawEvent, target, source, logger) {
      var handler = lookup(eventType, target);
      var simulatedEvent = fromSource(rawEvent, source);
      return handler.fold(function () {
        logger.logEventNoHandlers(eventType, target);
        return adt.complete();
      }, function (handlerInfo) {
        var descHandler = handlerInfo.descHandler;
        var eventHandler = getCurried(descHandler);
        eventHandler(simulatedEvent);
        if (simulatedEvent.isStopped()) {
          logger.logEventStopped(eventType, handlerInfo.element, descHandler.purpose);
          return adt.stopped();
        } else if (simulatedEvent.isCut()) {
          logger.logEventCut(eventType, handlerInfo.element, descHandler.purpose);
          return adt.complete();
        } else {
          return parent(handlerInfo.element).fold(function () {
            logger.logNoParent(eventType, handlerInfo.element, descHandler.purpose);
            return adt.complete();
          }, function (parent) {
            logger.logEventResponse(eventType, handlerInfo.element, descHandler.purpose);
            return adt.resume(parent);
          });
        }
      });
    };
    var doTriggerOnUntilStopped = function (lookup, eventType, rawEvent, rawTarget, source, logger) {
      return doTriggerHandler(lookup, eventType, rawEvent, rawTarget, source, logger).fold(always, function (parent) {
        return doTriggerOnUntilStopped(lookup, eventType, rawEvent, parent, source, logger);
      }, never);
    };
    var triggerHandler = function (lookup, eventType, rawEvent, target, logger) {
      var source = derive$1(rawEvent, target);
      return doTriggerHandler(lookup, eventType, rawEvent, target, source, logger);
    };
    var broadcast = function (listeners, rawEvent, _logger) {
      var simulatedEvent = fromExternal(rawEvent);
      each$1(listeners, function (listener) {
        var descHandler = listener.descHandler;
        var handler = getCurried(descHandler);
        handler(simulatedEvent);
      });
      return simulatedEvent.isStopped();
    };
    var triggerUntilStopped = function (lookup, eventType, rawEvent, logger) {
      return triggerOnUntilStopped(lookup, eventType, rawEvent, rawEvent.target, logger);
    };
    var triggerOnUntilStopped = function (lookup, eventType, rawEvent, rawTarget, logger) {
      var source = derive$1(rawEvent, rawTarget);
      return doTriggerOnUntilStopped(lookup, eventType, rawEvent, rawTarget, source, logger);
    };

    var eventHandler = function (element, descHandler) {
      return {
        element: element,
        descHandler: descHandler
      };
    };
    var broadcastHandler = function (id, handler) {
      return {
        id: id,
        descHandler: handler
      };
    };
    var EventRegistry = function () {
      var registry = {};
      var registerId = function (extraArgs, id, events) {
        each(events, function (v, k) {
          var handlers = registry[k] !== undefined ? registry[k] : {};
          handlers[id] = curryArgs(v, extraArgs);
          registry[k] = handlers;
        });
      };
      var findHandler = function (handlers, elem) {
        return read(elem).bind(function (id) {
          return get$c(handlers, id);
        }).map(function (descHandler) {
          return eventHandler(elem, descHandler);
        });
      };
      var filterByType = function (type) {
        return get$c(registry, type).map(function (handlers) {
          return mapToArray(handlers, function (f, id) {
            return broadcastHandler(id, f);
          });
        }).getOr([]);
      };
      var find = function (isAboveRoot, type, target) {
        return get$c(registry, type).bind(function (handlers) {
          return closest$3(target, function (elem) {
            return findHandler(handlers, elem);
          }, isAboveRoot);
        });
      };
      var unregisterId = function (id) {
        each(registry, function (handlersById, _eventName) {
          if (has$2(handlersById, id)) {
            delete handlersById[id];
          }
        });
      };
      return {
        registerId: registerId,
        unregisterId: unregisterId,
        filterByType: filterByType,
        find: find
      };
    };

    var Registry = function () {
      var events = EventRegistry();
      var components = {};
      var readOrTag = function (component) {
        var elem = component.element;
        return read(elem).getOrThunk(function () {
          return write('uid-', component.element);
        });
      };
      var failOnDuplicate = function (component, tagId) {
        var conflict = components[tagId];
        if (conflict === component) {
          unregister(component);
        } else {
          throw new Error('The tagId "' + tagId + '" is already used by: ' + element(conflict.element) + '\nCannot use it for: ' + element(component.element) + '\n' + 'The conflicting element is' + (inBody(conflict.element) ? ' ' : ' not ') + 'already in the DOM');
        }
      };
      var register = function (component) {
        var tagId = readOrTag(component);
        if (hasNonNullableKey(components, tagId)) {
          failOnDuplicate(component, tagId);
        }
        var extraArgs = [component];
        events.registerId(extraArgs, tagId, component.events);
        components[tagId] = component;
      };
      var unregister = function (component) {
        read(component.element).each(function (tagId) {
          delete components[tagId];
          events.unregisterId(tagId);
        });
      };
      var filter = function (type) {
        return events.filterByType(type);
      };
      var find = function (isAboveRoot, type, target) {
        return events.find(isAboveRoot, type, target);
      };
      var getById = function (id) {
        return get$c(components, id);
      };
      return {
        find: find,
        filter: filter,
        register: register,
        unregister: unregister,
        getById: getById
      };
    };

    var takeover$1 = function (root) {
      var isAboveRoot = function (el) {
        return parent(root.element).fold(always, function (parent) {
          return eq(el, parent);
        });
      };
      var registry = Registry();
      var lookup = function (eventName, target) {
        return registry.find(isAboveRoot, eventName, target);
      };
      var domEvents = setup$1(root.element, {
        triggerEvent: function (eventName, event) {
          return monitorEvent(eventName, event.target, function (logger) {
            return triggerUntilStopped(lookup, eventName, event, logger);
          });
        }
      });
      var systemApi = {
        debugInfo: constant$1('real'),
        triggerEvent: function (eventName, target, data) {
          monitorEvent(eventName, target, function (logger) {
            return triggerOnUntilStopped(lookup, eventName, data, target, logger);
          });
        },
        triggerFocus: function (target, originator) {
          read(target).fold(function () {
            focus$3(target);
          }, function (_alloyId) {
            monitorEvent(focus$4(), target, function (logger) {
              triggerHandler(lookup, focus$4(), {
                originator: originator,
                kill: noop,
                prevent: noop,
                target: target
              }, target, logger);
              return false;
            });
          });
        },
        triggerEscape: function (comp, simulatedEvent) {
          systemApi.triggerEvent('keydown', comp.element, simulatedEvent.event);
        },
        getByUid: function (uid) {
          return getByUid(uid);
        },
        getByDom: function (elem) {
          return getByDom(elem);
        },
        build: build$1,
        addToGui: function (c) {
          add(c);
        },
        removeFromGui: function (c) {
          remove(c);
        },
        addToWorld: function (c) {
          addToWorld(c);
        },
        removeFromWorld: function (c) {
          removeFromWorld(c);
        },
        broadcast: function (message) {
          broadcast$1(message);
        },
        broadcastOn: function (channels, message) {
          broadcastOn(channels, message);
        },
        broadcastEvent: function (eventName, event) {
          broadcastEvent(eventName, event);
        },
        isConnected: always
      };
      var addToWorld = function (component) {
        component.connect(systemApi);
        if (!isText(component.element)) {
          registry.register(component);
          each$1(component.components(), addToWorld);
          systemApi.triggerEvent(systemInit(), component.element, { target: component.element });
        }
      };
      var removeFromWorld = function (component) {
        if (!isText(component.element)) {
          each$1(component.components(), removeFromWorld);
          registry.unregister(component);
        }
        component.disconnect();
      };
      var add = function (component) {
        attach(root, component);
      };
      var remove = function (component) {
        detach(component);
      };
      var destroy = function () {
        domEvents.unbind();
        remove$7(root.element);
      };
      var broadcastData = function (data) {
        var receivers = registry.filter(receive$1());
        each$1(receivers, function (receiver) {
          var descHandler = receiver.descHandler;
          var handler = getCurried(descHandler);
          handler(data);
        });
      };
      var broadcast$1 = function (message) {
        broadcastData({
          universal: true,
          data: message
        });
      };
      var broadcastOn = function (channels, message) {
        broadcastData({
          universal: false,
          channels: channels,
          data: message
        });
      };
      var broadcastEvent = function (eventName, event) {
        var listeners = registry.filter(eventName);
        return broadcast(listeners, event);
      };
      var getByUid = function (uid) {
        return registry.getById(uid).fold(function () {
          return Result.error(new Error('Could not find component with uid: "' + uid + '" in system.'));
        }, Result.value);
      };
      var getByDom = function (elem) {
        var uid = read(elem).getOr('not found');
        return getByUid(uid);
      };
      addToWorld(root);
      return {
        root: root,
        element: root.element,
        destroy: destroy,
        add: add,
        remove: remove,
        getByUid: getByUid,
        getByDom: getByDom,
        addToWorld: addToWorld,
        removeFromWorld: removeFromWorld,
        broadcast: broadcast$1,
        broadcastOn: broadcastOn,
        broadcastEvent: broadcastEvent
      };
    };

    var READ_ONLY_MODE_CLASS = resolve('readonly-mode');
    var EDIT_MODE_CLASS = resolve('edit-mode');
    function OuterContainer (spec) {
      var root = build$1(Container.sketch({
        dom: { classes: [resolve('outer-container')].concat(spec.classes) },
        containerBehaviours: derive$2([Swapping.config({
            alpha: READ_ONLY_MODE_CLASS,
            omega: EDIT_MODE_CLASS
          })])
      }));
      return takeover$1(root);
    }

    function AndroidRealm (scrollIntoView) {
      var alloy = OuterContainer({ classes: [resolve('android-container')] });
      var toolbar = ScrollingToolbar();
      var webapp = api$2();
      var switchToEdit = makeEditSwitch(webapp);
      var socket = makeSocket();
      var dropup = build(noop, scrollIntoView);
      alloy.add(toolbar.wrapper);
      alloy.add(socket);
      alloy.add(dropup.component);
      var setToolbarGroups = function (rawGroups) {
        var groups = toolbar.createGroups(rawGroups);
        toolbar.setGroups(groups);
      };
      var setContextToolbar = function (rawGroups) {
        var groups = toolbar.createGroups(rawGroups);
        toolbar.setContextToolbar(groups);
      };
      var focusToolbar = function () {
        toolbar.focus();
      };
      var restoreToolbar = function () {
        toolbar.restoreToolbar();
      };
      var init = function (spec) {
        webapp.set(produce$1(spec));
      };
      var exit = function () {
        webapp.run(function (w) {
          w.exit();
          Replacing.remove(socket, switchToEdit);
        });
      };
      var updateMode$1 = function (readOnly) {
        updateMode(socket, switchToEdit, readOnly, alloy.root);
      };
      return {
        system: alloy,
        element: alloy.element,
        init: init,
        exit: exit,
        setToolbarGroups: setToolbarGroups,
        setContextToolbar: setContextToolbar,
        focusToolbar: focusToolbar,
        restoreToolbar: restoreToolbar,
        updateMode: updateMode$1,
        socket: socket,
        dropup: dropup
      };
    }

    var input = function (parent, operation) {
      var input = SugarElement.fromTag('input');
      setAll(input, {
        opacity: '0',
        position: 'absolute',
        top: '-1000px',
        left: '-1000px'
      });
      append$2(parent, input);
      focus$3(input);
      operation(input);
      remove$7(input);
    };

    var refresh = function (winScope) {
      var sel = winScope.getSelection();
      if (sel.rangeCount > 0) {
        var br = sel.getRangeAt(0);
        var r = winScope.document.createRange();
        r.setStart(br.startContainer, br.startOffset);
        r.setEnd(br.endContainer, br.endOffset);
        sel.removeAllRanges();
        sel.addRange(r);
      }
    };

    var resume = function (cWin, frame) {
      active().each(function (active) {
        if (!eq(active, frame)) {
          blur$1(active);
        }
      });
      cWin.focus();
      focus$3(SugarElement.fromDom(cWin.document.body));
      refresh(cWin);
    };

    var stubborn = function (outerBody, cWin, page, frame) {
      var toEditing = function () {
        resume(cWin, frame);
      };
      var toReading = function () {
        input(outerBody, blur$1);
      };
      var captureInput = bind(page, 'keydown', function (evt) {
        if (!contains$1([
            'input',
            'textarea'
          ], name$1(evt.target))) {
          toEditing();
        }
      });
      var onToolbarTouch = noop;
      var destroy = function () {
        captureInput.unbind();
      };
      return {
        toReading: toReading,
        toEditing: toEditing,
        onToolbarTouch: onToolbarTouch,
        destroy: destroy
      };
    };

    var initEvents = function (editorApi, iosApi, toolstrip, socket, _dropup) {
      var saveSelectionFirst = function () {
        iosApi.run(function (api) {
          api.highlightSelection();
        });
      };
      var refreshIosSelection = function () {
        iosApi.run(function (api) {
          api.refreshSelection();
        });
      };
      var scrollToY = function (yTop, height) {
        var y = yTop - socket.dom.scrollTop;
        iosApi.run(function (api) {
          api.scrollIntoView(y, y + height);
        });
      };
      var scrollToElement = function (_target) {
        scrollToY(iosApi, socket);
      };
      var scrollToCursor = function () {
        editorApi.getCursorBox().each(function (box) {
          scrollToY(box.top, box.height);
        });
      };
      var clearSelection = function () {
        iosApi.run(function (api) {
          api.clearSelection();
        });
      };
      var clearAndRefresh = function () {
        clearSelection();
        refreshThrottle.throttle();
      };
      var refreshView = function () {
        scrollToCursor();
        iosApi.run(function (api) {
          api.syncHeight();
        });
      };
      var reposition = function () {
        var toolbarHeight = get$7(toolstrip);
        iosApi.run(function (api) {
          api.setViewportOffset(toolbarHeight);
        });
        refreshIosSelection();
        refreshView();
      };
      var toEditing = function () {
        iosApi.run(function (api) {
          api.toEditing();
        });
      };
      var toReading = function () {
        iosApi.run(function (api) {
          api.toReading();
        });
      };
      var onToolbarTouch = function (event) {
        iosApi.run(function (api) {
          api.onToolbarTouch(event);
        });
      };
      var tapping = monitor(editorApi);
      var refreshThrottle = last(refreshView, 300);
      var listeners = [
        editorApi.onKeyup(clearAndRefresh),
        editorApi.onNodeChanged(refreshIosSelection),
        editorApi.onDomChanged(refreshThrottle.throttle),
        editorApi.onDomChanged(refreshIosSelection),
        editorApi.onScrollToCursor(function (tinyEvent) {
          tinyEvent.preventDefault();
          refreshThrottle.throttle();
        }),
        editorApi.onScrollToElement(function (event) {
          scrollToElement(event.element);
        }),
        editorApi.onToEditing(toEditing),
        editorApi.onToReading(toReading),
        bind(editorApi.doc, 'touchend', function (touchEvent) {
          if (eq(editorApi.html, touchEvent.target) || eq(editorApi.body, touchEvent.target)) ;
        }),
        bind(toolstrip, 'transitionend', function (transitionEvent) {
          if (transitionEvent.raw.propertyName === 'height') {
            reposition();
          }
        }),
        capture(toolstrip, 'touchstart', function (touchEvent) {
          saveSelectionFirst();
          onToolbarTouch(touchEvent);
          editorApi.onTouchToolstrip();
        }),
        bind(editorApi.body, 'touchstart', function (evt) {
          clearSelection();
          editorApi.onTouchContent();
          tapping.fireTouchstart(evt);
        }),
        tapping.onTouchmove(),
        tapping.onTouchend(),
        bind(editorApi.body, 'click', function (event) {
          event.kill();
        }),
        bind(toolstrip, 'touchmove', function () {
          editorApi.onToolbarScrollStart();
        })
      ];
      var destroy = function () {
        each$1(listeners, function (l) {
          l.unbind();
        });
      };
      return { destroy: destroy };
    };

    function FakeSelection (win, frame) {
      var doc = win.document;
      var container = SugarElement.fromTag('div');
      add$1(container, resolve('unfocused-selections'));
      append$2(SugarElement.fromDom(doc.documentElement), container);
      var onTouch = bind(container, 'touchstart', function (event) {
        event.prevent();
        resume(win, frame);
        clear();
      });
      var make = function (rectangle) {
        var span = SugarElement.fromTag('span');
        add(span, [
          resolve('layer-editor'),
          resolve('unfocused-selection')
        ]);
        setAll(span, {
          left: rectangle.left + 'px',
          top: rectangle.top + 'px',
          width: rectangle.width + 'px',
          height: rectangle.height + 'px'
        });
        return span;
      };
      var update = function () {
        clear();
        var rectangles = getRectangles(win);
        var spans = map$2(rectangles, make);
        append$1(container, spans);
      };
      var clear = function () {
        empty(container);
      };
      var destroy = function () {
        onTouch.unbind();
        remove$7(container);
      };
      var isActive = function () {
        return children(container).length > 0;
      };
      return {
        update: update,
        isActive: isActive,
        destroy: destroy,
        clear: clear
      };
    }

    var nu$1 = function (baseFn) {
      var data = Optional.none();
      var callbacks = [];
      var map = function (f) {
        return nu$1(function (nCallback) {
          get(function (data) {
            nCallback(f(data));
          });
        });
      };
      var get = function (nCallback) {
        if (isReady()) {
          call(nCallback);
        } else {
          callbacks.push(nCallback);
        }
      };
      var set = function (x) {
        if (!isReady()) {
          data = Optional.some(x);
          run(callbacks);
          callbacks = [];
        }
      };
      var isReady = function () {
        return data.isSome();
      };
      var run = function (cbs) {
        each$1(cbs, call);
      };
      var call = function (cb) {
        data.each(function (x) {
          setTimeout(function () {
            cb(x);
          }, 0);
        });
      };
      baseFn(set);
      return {
        get: get,
        map: map,
        isReady: isReady
      };
    };
    var pure$1 = function (a) {
      return nu$1(function (callback) {
        callback(a);
      });
    };
    var LazyValue = {
      nu: nu$1,
      pure: pure$1
    };

    var errorReporter = function (err) {
      setTimeout(function () {
        throw err;
      }, 0);
    };
    var make = function (run) {
      var get = function (callback) {
        run().then(callback, errorReporter);
      };
      var map = function (fab) {
        return make(function () {
          return run().then(fab);
        });
      };
      var bind = function (aFutureB) {
        return make(function () {
          return run().then(function (v) {
            return aFutureB(v).toPromise();
          });
        });
      };
      var anonBind = function (futureB) {
        return make(function () {
          return run().then(function () {
            return futureB.toPromise();
          });
        });
      };
      var toLazy = function () {
        return LazyValue.nu(get);
      };
      var toCached = function () {
        var cache = null;
        return make(function () {
          if (cache === null) {
            cache = run();
          }
          return cache;
        });
      };
      var toPromise = run;
      return {
        map: map,
        bind: bind,
        anonBind: anonBind,
        toLazy: toLazy,
        toCached: toCached,
        toPromise: toPromise,
        get: get
      };
    };
    var nu = function (baseFn) {
      return make(function () {
        return new Promise$1(baseFn);
      });
    };
    var pure = function (a) {
      return make(function () {
        return Promise$1.resolve(a);
      });
    };
    var Future = {
      nu: nu,
      pure: pure
    };

    var adjust = function (value, destination, amount) {
      if (Math.abs(value - destination) <= amount) {
        return Optional.none();
      } else if (value < destination) {
        return Optional.some(value + amount);
      } else {
        return Optional.some(value - amount);
      }
    };
    var create$1 = function () {
      var interval = null;
      var animate = function (getCurrent, destination, amount, increment, doFinish, rate) {
        var finished = false;
        var finish = function (v) {
          finished = true;
          doFinish(v);
        };
        global$2.clearInterval(interval);
        var abort = function (v) {
          global$2.clearInterval(interval);
          finish(v);
        };
        interval = global$2.setInterval(function () {
          var value = getCurrent();
          adjust(value, destination, amount).fold(function () {
            global$2.clearInterval(interval);
            finish(destination);
          }, function (s) {
            increment(s, abort);
            if (!finished) {
              var newValue = getCurrent();
              if (newValue !== s || Math.abs(newValue - destination) > Math.abs(value - destination)) {
                global$2.clearInterval(interval);
                finish(destination);
              }
            }
          });
        }, rate);
      };
      return { animate: animate };
    };

    var findDevice = function (deviceWidth, deviceHeight) {
      var devices = [
        {
          width: 320,
          height: 480,
          keyboard: {
            portrait: 300,
            landscape: 240
          }
        },
        {
          width: 320,
          height: 568,
          keyboard: {
            portrait: 300,
            landscape: 240
          }
        },
        {
          width: 375,
          height: 667,
          keyboard: {
            portrait: 305,
            landscape: 240
          }
        },
        {
          width: 414,
          height: 736,
          keyboard: {
            portrait: 320,
            landscape: 240
          }
        },
        {
          width: 768,
          height: 1024,
          keyboard: {
            portrait: 320,
            landscape: 400
          }
        },
        {
          width: 1024,
          height: 1366,
          keyboard: {
            portrait: 380,
            landscape: 460
          }
        }
      ];
      return findMap(devices, function (device) {
        return someIf(deviceWidth <= device.width && deviceHeight <= device.height, device.keyboard);
      }).getOr({
        portrait: deviceHeight / 5,
        landscape: deviceWidth / 4
      });
    };

    var softKeyboardLimits = function (outerWindow) {
      return findDevice(outerWindow.screen.width, outerWindow.screen.height);
    };
    var accountableKeyboardHeight = function (outerWindow) {
      var portrait = get$1(outerWindow).isPortrait();
      var limits = softKeyboardLimits(outerWindow);
      var keyboard = portrait ? limits.portrait : limits.landscape;
      var visualScreenHeight = portrait ? outerWindow.screen.height : outerWindow.screen.width;
      return visualScreenHeight - outerWindow.innerHeight > keyboard ? 0 : keyboard;
    };
    var getGreenzone = function (socket, dropup) {
      var outerWindow = owner$2(socket).dom.defaultView;
      var viewportHeight = get$7(socket) + get$7(dropup);
      var acc = accountableKeyboardHeight(outerWindow);
      return viewportHeight - acc;
    };
    var updatePadding = function (contentBody, socket, dropup) {
      var greenzoneHeight = getGreenzone(socket, dropup);
      var deltaHeight = get$7(socket) + get$7(dropup) - greenzoneHeight;
      set$5(contentBody, 'padding-bottom', deltaHeight + 'px');
    };

    var fixture = Adt.generate([
      {
        fixed: [
          'element',
          'property',
          'offsetY'
        ]
      },
      {
        scroller: [
          'element',
          'offsetY'
        ]
      }
    ]);
    var yFixedData = 'data-' + resolve('position-y-fixed');
    var yFixedProperty = 'data-' + resolve('y-property');
    var yScrollingData = 'data-' + resolve('scrolling');
    var windowSizeData = 'data-' + resolve('last-window-height');
    var getYFixedData = function (element) {
      return safeParse(element, yFixedData);
    };
    var getYFixedProperty = function (element) {
      return get$b(element, yFixedProperty);
    };
    var getLastWindowSize = function (element) {
      return safeParse(element, windowSizeData);
    };
    var classifyFixed = function (element, offsetY) {
      var prop = getYFixedProperty(element);
      return fixture.fixed(element, prop, offsetY);
    };
    var classifyScrolling = function (element, offsetY) {
      return fixture.scroller(element, offsetY);
    };
    var classify = function (element) {
      var offsetY = getYFixedData(element);
      var classifier = get$b(element, yScrollingData) === 'true' ? classifyScrolling : classifyFixed;
      return classifier(element, offsetY);
    };
    var findFixtures = function (container) {
      var candidates = descendants(container, '[' + yFixedData + ']');
      return map$2(candidates, classify);
    };
    var takeoverToolbar = function (toolbar) {
      var oldToolbarStyle = get$b(toolbar, 'style');
      setAll(toolbar, {
        position: 'absolute',
        top: '0px'
      });
      set$8(toolbar, yFixedData, '0px');
      set$8(toolbar, yFixedProperty, 'top');
      var restore = function () {
        set$8(toolbar, 'style', oldToolbarStyle || '');
        remove$6(toolbar, yFixedData);
        remove$6(toolbar, yFixedProperty);
      };
      return { restore: restore };
    };
    var takeoverViewport = function (toolbarHeight, height, viewport) {
      var oldViewportStyle = get$b(viewport, 'style');
      register$2(viewport);
      setAll(viewport, {
        position: 'absolute',
        height: height + 'px',
        width: '100%',
        top: toolbarHeight + 'px'
      });
      set$8(viewport, yFixedData, toolbarHeight + 'px');
      set$8(viewport, yScrollingData, 'true');
      set$8(viewport, yFixedProperty, 'top');
      var restore = function () {
        deregister(viewport);
        set$8(viewport, 'style', oldViewportStyle || '');
        remove$6(viewport, yFixedData);
        remove$6(viewport, yScrollingData);
        remove$6(viewport, yFixedProperty);
      };
      return { restore: restore };
    };
    var takeoverDropup = function (dropup) {
      var oldDropupStyle = get$b(dropup, 'style');
      setAll(dropup, {
        position: 'absolute',
        bottom: '0px'
      });
      set$8(dropup, yFixedData, '0px');
      set$8(dropup, yFixedProperty, 'bottom');
      var restore = function () {
        set$8(dropup, 'style', oldDropupStyle || '');
        remove$6(dropup, yFixedData);
        remove$6(dropup, yFixedProperty);
      };
      return { restore: restore };
    };
    var deriveViewportHeight = function (viewport, toolbarHeight, dropupHeight) {
      var outerWindow = owner$2(viewport).dom.defaultView;
      var winH = outerWindow.innerHeight;
      set$8(viewport, windowSizeData, winH + 'px');
      return winH - toolbarHeight - dropupHeight;
    };
    var takeover = function (viewport, contentBody, toolbar, dropup) {
      var outerWindow = owner$2(viewport).dom.defaultView;
      var toolbarSetup = takeoverToolbar(toolbar);
      var toolbarHeight = get$7(toolbar);
      var dropupHeight = get$7(dropup);
      var viewportHeight = deriveViewportHeight(viewport, toolbarHeight, dropupHeight);
      var viewportSetup = takeoverViewport(toolbarHeight, viewportHeight, viewport);
      var dropupSetup = takeoverDropup(dropup);
      var isActive = true;
      var restore = function () {
        isActive = false;
        toolbarSetup.restore();
        viewportSetup.restore();
        dropupSetup.restore();
      };
      var isExpanding = function () {
        var currentWinHeight = outerWindow.innerHeight;
        var lastWinHeight = getLastWindowSize(viewport);
        return currentWinHeight > lastWinHeight;
      };
      var refresh = function () {
        if (isActive) {
          var newToolbarHeight = get$7(toolbar);
          var dropupHeight_1 = get$7(dropup);
          var newHeight = deriveViewportHeight(viewport, newToolbarHeight, dropupHeight_1);
          set$8(viewport, yFixedData, newToolbarHeight + 'px');
          set$5(viewport, 'height', newHeight + 'px');
          updatePadding(contentBody, viewport, dropup);
        }
      };
      var setViewportOffset = function (newYOffset) {
        var offsetPx = newYOffset + 'px';
        set$8(viewport, yFixedData, offsetPx);
        refresh();
      };
      updatePadding(contentBody, viewport, dropup);
      return {
        setViewportOffset: setViewportOffset,
        isExpanding: isExpanding,
        isShrinking: not(isExpanding),
        refresh: refresh,
        restore: restore
      };
    };

    var animator = create$1();
    var ANIMATION_STEP = 15;
    var NUM_TOP_ANIMATION_FRAMES = 10;
    var ANIMATION_RATE = 10;
    var lastScroll = 'data-' + resolve('last-scroll-top');
    var getTop = function (element) {
      var raw = getRaw(element, 'top').getOr('0');
      return parseInt(raw, 10);
    };
    var getScrollTop = function (element) {
      return parseInt(element.dom.scrollTop, 10);
    };
    var moveScrollAndTop = function (element, destination, finalTop) {
      return Future.nu(function (callback) {
        var getCurrent = curry(getScrollTop, element);
        var update = function (newScroll) {
          element.dom.scrollTop = newScroll;
          set$5(element, 'top', getTop(element) + ANIMATION_STEP + 'px');
        };
        var finish = function () {
          element.dom.scrollTop = destination;
          set$5(element, 'top', finalTop + 'px');
          callback(destination);
        };
        animator.animate(getCurrent, destination, ANIMATION_STEP, update, finish, ANIMATION_RATE);
      });
    };
    var moveOnlyScroll = function (element, destination) {
      return Future.nu(function (callback) {
        var getCurrent = curry(getScrollTop, element);
        set$8(element, lastScroll, getCurrent());
        var update = function (newScroll, abort) {
          var previous = safeParse(element, lastScroll);
          if (previous !== element.dom.scrollTop) {
            abort(element.dom.scrollTop);
          } else {
            element.dom.scrollTop = newScroll;
            set$8(element, lastScroll, newScroll);
          }
        };
        var finish = function () {
          element.dom.scrollTop = destination;
          set$8(element, lastScroll, destination);
          callback(destination);
        };
        var distance = Math.abs(destination - getCurrent());
        var step = Math.ceil(distance / NUM_TOP_ANIMATION_FRAMES);
        animator.animate(getCurrent, destination, step, update, finish, ANIMATION_RATE);
      });
    };
    var moveOnlyTop = function (element, destination) {
      return Future.nu(function (callback) {
        var getCurrent = curry(getTop, element);
        var update = function (newTop) {
          set$5(element, 'top', newTop + 'px');
        };
        var finish = function () {
          update(destination);
          callback(destination);
        };
        var distance = Math.abs(destination - getCurrent());
        var step = Math.ceil(distance / NUM_TOP_ANIMATION_FRAMES);
        animator.animate(getCurrent, destination, step, update, finish, ANIMATION_RATE);
      });
    };
    var updateTop = function (element, amount) {
      var newTop = amount + getYFixedData(element) + 'px';
      set$5(element, 'top', newTop);
    };
    var moveWindowScroll = function (toolbar, viewport, destY) {
      var outerWindow = owner$2(toolbar).dom.defaultView;
      return Future.nu(function (callback) {
        updateTop(toolbar, destY);
        updateTop(viewport, destY);
        outerWindow.scrollTo(0, destY);
        callback(destY);
      });
    };

    function BackgroundActivity (doAction) {
      var action = Cell(LazyValue.pure({}));
      var start = function (value) {
        var future = LazyValue.nu(function (callback) {
          return doAction(value).get(callback);
        });
        action.set(future);
      };
      var idle = function (g) {
        action.get().get(function () {
          g();
        });
      };
      return {
        start: start,
        idle: idle
      };
    }

    var scrollIntoView = function (cWin, socket, dropup, top, bottom) {
      var greenzone = getGreenzone(socket, dropup);
      var refreshCursor = curry(refresh, cWin);
      if (top > greenzone || bottom > greenzone) {
        moveOnlyScroll(socket, socket.dom.scrollTop - greenzone + bottom).get(refreshCursor);
      } else if (top < 0) {
        moveOnlyScroll(socket, socket.dom.scrollTop + top).get(refreshCursor);
      } else ;
    };

    var par$1 = function (asyncValues, nu) {
      return nu(function (callback) {
        var r = [];
        var count = 0;
        var cb = function (i) {
          return function (value) {
            r[i] = value;
            count++;
            if (count >= asyncValues.length) {
              callback(r);
            }
          };
        };
        if (asyncValues.length === 0) {
          callback([]);
        } else {
          each$1(asyncValues, function (asyncValue, i) {
            asyncValue.get(cb(i));
          });
        }
      });
    };

    var par = function (futures) {
      return par$1(futures, Future.nu);
    };

    var updateFixed = function (element, property, winY, offsetY) {
      var destination = winY + offsetY;
      set$5(element, property, destination + 'px');
      return Future.pure(offsetY);
    };
    var updateScrollingFixed = function (element, winY, offsetY) {
      var destTop = winY + offsetY;
      var oldProp = getRaw(element, 'top').getOr(offsetY);
      var delta = destTop - parseInt(oldProp, 10);
      var destScroll = element.dom.scrollTop + delta;
      return moveScrollAndTop(element, destScroll, destTop);
    };
    var updateFixture = function (fixture, winY) {
      return fixture.fold(function (element, property, offsetY) {
        return updateFixed(element, property, winY, offsetY);
      }, function (element, offsetY) {
        return updateScrollingFixed(element, winY, offsetY);
      });
    };
    var updatePositions = function (container, winY) {
      var fixtures = findFixtures(container);
      var updates = map$2(fixtures, function (fixture) {
        return updateFixture(fixture, winY);
      });
      return par(updates);
    };

    var VIEW_MARGIN = 5;
    var register = function (toolstrip, socket, container, outerWindow, structure, cWin) {
      var scroller = BackgroundActivity(function (y) {
        return moveWindowScroll(toolstrip, socket, y);
      });
      var scrollBounds = function () {
        var rects = getRectangles(cWin);
        return Optional.from(rects[0]).bind(function (rect) {
          var viewTop = rect.top - socket.dom.scrollTop;
          var outside = viewTop > outerWindow.innerHeight + VIEW_MARGIN || viewTop < -VIEW_MARGIN;
          return outside ? Optional.some({
            top: viewTop,
            bottom: viewTop + rect.height
          }) : Optional.none();
        });
      };
      var scrollThrottle = last(function () {
        scroller.idle(function () {
          updatePositions(container, outerWindow.pageYOffset).get(function () {
            var extraScroll = scrollBounds();
            extraScroll.each(function (extra) {
              socket.dom.scrollTop = socket.dom.scrollTop + extra.top;
            });
            scroller.start(0);
            structure.refresh();
          });
        });
      }, 1000);
      var onScroll = bind(SugarElement.fromDom(outerWindow), 'scroll', function () {
        if (outerWindow.pageYOffset < 0) {
          return;
        }
        scrollThrottle.throttle();
      });
      updatePositions(container, outerWindow.pageYOffset).get(identity);
      return { unbind: onScroll.unbind };
    };
    var setup = function (bag) {
      var cWin = bag.cWin;
      var ceBody = bag.ceBody;
      var socket = bag.socket;
      var toolstrip = bag.toolstrip;
      var contentElement = bag.contentElement;
      var keyboardType = bag.keyboardType;
      var outerWindow = bag.outerWindow;
      var dropup = bag.dropup;
      var outerBody = bag.outerBody;
      var structure = takeover(socket, ceBody, toolstrip, dropup);
      var keyboardModel = keyboardType(outerBody, cWin, body(), contentElement);
      var toEditing = function () {
        keyboardModel.toEditing();
        clearSelection();
      };
      var toReading = function () {
        keyboardModel.toReading();
      };
      var onToolbarTouch = function (_event) {
        keyboardModel.onToolbarTouch();
      };
      var onOrientation = onChange(outerWindow, {
        onChange: noop,
        onReady: structure.refresh
      });
      onOrientation.onAdjustment(function () {
        structure.refresh();
      });
      var onResize = bind(SugarElement.fromDom(outerWindow), 'resize', function () {
        if (structure.isExpanding()) {
          structure.refresh();
        }
      });
      var onScroll = register(toolstrip, socket, outerBody, outerWindow, structure, cWin);
      var unfocusedSelection = FakeSelection(cWin, contentElement);
      var refreshSelection = function () {
        if (unfocusedSelection.isActive()) {
          unfocusedSelection.update();
        }
      };
      var highlightSelection = function () {
        unfocusedSelection.update();
      };
      var clearSelection = function () {
        unfocusedSelection.clear();
      };
      var scrollIntoView$1 = function (top, bottom) {
        scrollIntoView(cWin, socket, dropup, top, bottom);
      };
      var syncHeight = function () {
        set$5(contentElement, 'height', contentElement.dom.contentWindow.document.body.scrollHeight + 'px');
      };
      var setViewportOffset = function (newYOffset) {
        structure.setViewportOffset(newYOffset);
        moveOnlyTop(socket, newYOffset).get(identity);
      };
      var destroy = function () {
        structure.restore();
        onOrientation.destroy();
        onScroll.unbind();
        onResize.unbind();
        keyboardModel.destroy();
        unfocusedSelection.destroy();
        input(body(), blur$1);
      };
      return {
        toEditing: toEditing,
        toReading: toReading,
        onToolbarTouch: onToolbarTouch,
        refreshSelection: refreshSelection,
        clearSelection: clearSelection,
        highlightSelection: highlightSelection,
        scrollIntoView: scrollIntoView$1,
        updateToolbarPadding: noop,
        setViewportOffset: setViewportOffset,
        syncHeight: syncHeight,
        refreshStructure: structure.refresh,
        destroy: destroy
      };
    };

    var create = function (platform, mask) {
      var meta = tag();
      var priorState = value();
      var scrollEvents = value();
      var iosApi = api$2();
      var iosEvents = api$2();
      var enter = function () {
        mask.hide();
        var doc = SugarElement.fromDom(document);
        getActiveApi(platform.editor).each(function (editorApi) {
          priorState.set({
            socketHeight: getRaw(platform.socket, 'height'),
            iframeHeight: getRaw(editorApi.frame, 'height'),
            outerScroll: document.body.scrollTop
          });
          scrollEvents.set({ exclusives: exclusive(doc, '.' + scrollable) });
          add$1(platform.container, resolve('fullscreen-maximized'));
          clobberStyles(platform.container, editorApi.body);
          meta.maximize();
          set$5(platform.socket, 'overflow', 'scroll');
          set$5(platform.socket, '-webkit-overflow-scrolling', 'touch');
          focus$3(editorApi.body);
          iosApi.set(setup({
            cWin: editorApi.win,
            ceBody: editorApi.body,
            socket: platform.socket,
            toolstrip: platform.toolstrip,
            dropup: platform.dropup.element,
            contentElement: editorApi.frame,
            outerBody: platform.body,
            outerWindow: platform.win,
            keyboardType: stubborn
          }));
          iosApi.run(function (api) {
            api.syncHeight();
          });
          iosEvents.set(initEvents(editorApi, iosApi, platform.toolstrip, platform.socket, platform.dropup));
        });
      };
      var exit = function () {
        meta.restore();
        iosEvents.clear();
        iosApi.clear();
        mask.show();
        priorState.on(function (s) {
          s.socketHeight.each(function (h) {
            set$5(platform.socket, 'height', h);
          });
          s.iframeHeight.each(function (h) {
            set$5(platform.editor.getFrame(), 'height', h);
          });
          document.body.scrollTop = s.scrollTop;
        });
        priorState.clear();
        scrollEvents.on(function (s) {
          s.exclusives.unbind();
        });
        scrollEvents.clear();
        remove$3(platform.container, resolve('fullscreen-maximized'));
        restoreStyles();
        deregister(platform.toolbar);
        remove$2(platform.socket, 'overflow');
        remove$2(platform.socket, '-webkit-overflow-scrolling');
        blur$1(platform.editor.getFrame());
        getActiveApi(platform.editor).each(function (editorApi) {
          editorApi.clearSelection();
        });
      };
      var refreshStructure = function () {
        iosApi.run(function (api) {
          api.refreshStructure();
        });
      };
      return {
        enter: enter,
        refreshStructure: refreshStructure,
        exit: exit
      };
    };

    var produce = function (raw) {
      var mobile = asRawOrDie$1('Getting IosWebapp schema', MobileSchema, raw);
      set$5(mobile.toolstrip, 'width', '100%');
      set$5(mobile.container, 'position', 'relative');
      var onView = function () {
        mobile.setReadOnly(mobile.readOnlyOnInit());
        mode.enter();
      };
      var mask = build$1(sketch(onView, mobile.translate));
      mobile.alloy.add(mask);
      var maskApi = {
        show: function () {
          mobile.alloy.add(mask);
        },
        hide: function () {
          mobile.alloy.remove(mask);
        }
      };
      var mode = create(mobile, maskApi);
      return {
        setReadOnly: mobile.setReadOnly,
        refreshStructure: mode.refreshStructure,
        enter: mode.enter,
        exit: mode.exit,
        destroy: noop
      };
    };

    function IosRealm (scrollIntoView) {
      var alloy = OuterContainer({ classes: [resolve('ios-container')] });
      var toolbar = ScrollingToolbar();
      var webapp = api$2();
      var switchToEdit = makeEditSwitch(webapp);
      var socket = makeSocket();
      var dropup = build(function () {
        webapp.run(function (w) {
          w.refreshStructure();
        });
      }, scrollIntoView);
      alloy.add(toolbar.wrapper);
      alloy.add(socket);
      alloy.add(dropup.component);
      var setToolbarGroups = function (rawGroups) {
        var groups = toolbar.createGroups(rawGroups);
        toolbar.setGroups(groups);
      };
      var setContextToolbar = function (rawGroups) {
        var groups = toolbar.createGroups(rawGroups);
        toolbar.setContextToolbar(groups);
      };
      var focusToolbar = function () {
        toolbar.focus();
      };
      var restoreToolbar = function () {
        toolbar.restoreToolbar();
      };
      var init = function (spec) {
        webapp.set(produce(spec));
      };
      var exit = function () {
        webapp.run(function (w) {
          Replacing.remove(socket, switchToEdit);
          w.exit();
        });
      };
      var updateMode$1 = function (readOnly) {
        updateMode(socket, switchToEdit, readOnly, alloy.root);
      };
      return {
        system: alloy,
        element: alloy.element,
        init: init,
        exit: exit,
        setToolbarGroups: setToolbarGroups,
        setContextToolbar: setContextToolbar,
        focusToolbar: focusToolbar,
        restoreToolbar: restoreToolbar,
        updateMode: updateMode$1,
        socket: socket,
        dropup: dropup
      };
    }

    var global$1 = tinymce.util.Tools.resolve('tinymce.EditorManager');

    var derive = function (editor) {
      var base = Optional.from(getSkinUrl(editor)).getOrThunk(function () {
        return global$1.baseURL + '/skins/ui/oxide';
      });
      return {
        content: base + '/content.mobile.min.css',
        ui: base + '/skin.mobile.min.css'
      };
    };

    var fireChange = function (realm, command, state) {
      realm.system.broadcastOn([formatChanged], {
        command: command,
        state: state
      });
    };
    var init = function (realm, editor) {
      var allFormats = keys(editor.formatter.get());
      each$1(allFormats, function (command) {
        editor.formatter.formatChanged(command, function (state) {
          fireChange(realm, command, state);
        });
      });
      each$1([
        'ul',
        'ol'
      ], function (command) {
        editor.selection.selectorChanged(command, function (state, _data) {
          fireChange(realm, command, state);
        });
      });
    };

    var fireSkinLoaded = function (editor) {
      return function () {
        var done = function () {
          editor._skinLoaded = true;
          editor.fire('SkinLoaded');
        };
        if (editor.initialized) {
          done();
        } else {
          editor.on('init', done);
        }
      };
    };

    var READING = 'toReading';
    var EDITING = 'toEditing';
    var renderMobileTheme = function (editor) {
      var renderUI = function () {
        var targetNode = editor.getElement();
        var cssUrls = derive(editor);
        if (isSkinDisabled(editor) === false) {
          var styleSheetLoader_1 = global$5.DOM.styleSheetLoader;
          editor.contentCSS.push(cssUrls.content);
          styleSheetLoader_1.load(cssUrls.ui, fireSkinLoaded(editor));
          editor.on('remove', function () {
            return styleSheetLoader_1.unload(cssUrls.ui);
          });
        } else {
          fireSkinLoaded(editor)();
        }
        var doScrollIntoView = function () {
          editor.fire('ScrollIntoView');
        };
        var realm = detect$1().os.isAndroid() ? AndroidRealm(doScrollIntoView) : IosRealm(doScrollIntoView);
        var original = SugarElement.fromDom(targetNode);
        attachSystemAfter(original, realm.system);
        var findFocusIn = function (elem) {
          return search(elem).bind(function (focused) {
            return realm.system.getByDom(focused).toOptional();
          });
        };
        var outerWindow = targetNode.ownerDocument.defaultView;
        var orientation = onChange(outerWindow, {
          onChange: function () {
            var alloy = realm.system;
            alloy.broadcastOn([orientationChanged], { width: getActualWidth(outerWindow) });
          },
          onReady: noop
        });
        var setReadOnly = function (dynamicGroup, readOnlyGroups, mainGroups, ro) {
          if (ro === false) {
            editor.selection.collapse();
          }
          var toolbars = configureToolbar(dynamicGroup, readOnlyGroups, mainGroups);
          realm.setToolbarGroups(ro === true ? toolbars.readOnly : toolbars.main);
          editor.setMode(ro === true ? 'readonly' : 'design');
          editor.fire(ro === true ? READING : EDITING);
          realm.updateMode(ro);
        };
        var configureToolbar = function (dynamicGroup, readOnlyGroups, mainGroups) {
          var dynamic = dynamicGroup.get();
          var toolbars = {
            readOnly: dynamic.backToMask.concat(readOnlyGroups.get()),
            main: dynamic.backToMask.concat(mainGroups.get())
          };
          return toolbars;
        };
        var bindHandler = function (label, handler) {
          editor.on(label, handler);
          return {
            unbind: function () {
              editor.off(label);
            }
          };
        };
        editor.on('init', function () {
          realm.init({
            editor: {
              getFrame: function () {
                return SugarElement.fromDom(editor.contentAreaContainer.querySelector('iframe'));
              },
              onDomChanged: function () {
                return { unbind: noop };
              },
              onToReading: function (handler) {
                return bindHandler(READING, handler);
              },
              onToEditing: function (handler) {
                return bindHandler(EDITING, handler);
              },
              onScrollToCursor: function (handler) {
                editor.on('ScrollIntoView', function (tinyEvent) {
                  handler(tinyEvent);
                });
                var unbind = function () {
                  editor.off('ScrollIntoView');
                  orientation.destroy();
                };
                return { unbind: unbind };
              },
              onTouchToolstrip: function () {
                hideDropup();
              },
              onTouchContent: function () {
                var toolbar = SugarElement.fromDom(editor.editorContainer.querySelector('.' + resolve('toolbar')));
                findFocusIn(toolbar).each(emitExecute);
                realm.restoreToolbar();
                hideDropup();
              },
              onTapContent: function (evt) {
                var target = evt.target;
                if (name$1(target) === 'img') {
                  editor.selection.select(target.dom);
                  evt.kill();
                } else if (name$1(target) === 'a') {
                  var component = realm.system.getByDom(SugarElement.fromDom(editor.editorContainer));
                  component.each(function (container) {
                    if (Swapping.isAlpha(container)) {
                      openLink(target.dom);
                    }
                  });
                }
              }
            },
            container: SugarElement.fromDom(editor.editorContainer),
            socket: SugarElement.fromDom(editor.contentAreaContainer),
            toolstrip: SugarElement.fromDom(editor.editorContainer.querySelector('.' + resolve('toolstrip'))),
            toolbar: SugarElement.fromDom(editor.editorContainer.querySelector('.' + resolve('toolbar'))),
            dropup: realm.dropup,
            alloy: realm.system,
            translate: noop,
            setReadOnly: function (ro) {
              setReadOnly(dynamicGroup, readOnlyGroups, mainGroups, ro);
            },
            readOnlyOnInit: function () {
              return readOnlyOnInit();
            }
          });
          var hideDropup = function () {
            realm.dropup.disappear(function () {
              realm.system.broadcastOn([dropupDismissed], {});
            });
          };
          var backToMaskGroup = {
            label: 'The first group',
            scrollable: false,
            items: [forToolbar('back', function () {
                editor.selection.collapse();
                realm.exit();
              }, {}, editor)]
          };
          var backToReadOnlyGroup = {
            label: 'Back to read only',
            scrollable: false,
            items: [forToolbar('readonly-back', function () {
                setReadOnly(dynamicGroup, readOnlyGroups, mainGroups, true);
              }, {}, editor)]
          };
          var readOnlyGroup = {
            label: 'The read only mode group',
            scrollable: true,
            items: []
          };
          var features = setup$3(realm, editor);
          var items = detect(editor, features);
          var actionGroup = {
            label: 'the action group',
            scrollable: true,
            items: items
          };
          var extraGroup = {
            label: 'The extra group',
            scrollable: false,
            items: []
          };
          var mainGroups = Cell([
            actionGroup,
            extraGroup
          ]);
          var readOnlyGroups = Cell([
            readOnlyGroup,
            extraGroup
          ]);
          var dynamicGroup = Cell({
            backToMask: [backToMaskGroup],
            backToReadOnly: [backToReadOnlyGroup]
          });
          init(realm, editor);
        });
        editor.on('remove', function () {
          realm.exit();
        });
        editor.on('detach', function () {
          detachSystem(realm.system);
          realm.system.destroy();
        });
        return {
          iframeContainer: realm.socket.element.dom,
          editorContainer: realm.element.dom
        };
      };
      return {
        getNotificationManagerImpl: function () {
          return {
            open: constant$1({
              progressBar: { value: noop },
              close: noop,
              text: noop,
              getEl: constant$1(null),
              moveTo: noop,
              moveRel: noop,
              settings: {}
            }),
            close: noop,
            reposition: noop,
            getArgs: constant$1({})
          };
        },
        renderUI: renderUI
      };
    };
    function Theme () {
      global$4.add('mobile', renderMobileTheme);
    }

    Theme();

}());
