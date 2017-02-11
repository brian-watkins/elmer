var _brian_watkins$elmer$Native_Helpers = function() {

  var getChildren = function(html, tagger) {
    var children = []
    for (var i = 0; i < html.children.length; i++) {
      var element = html.children[i]
      if (element.type == "text") {
        children.push(_brian_watkins$elmer$Elmer_Types$Text(element.text))
      } else {
        children.push(_brian_watkins$elmer$Elmer_Types$Node(constructHtmlNode(element, tagger)))
      }
    }

    return _elm_lang$core$Native_List.fromArray(children)
  }

  var getHtmlEvents = function(html, tagger) {
    var events = []
    if (html.facts.EVENT) {
      for (var eventType in html.facts.EVENT) {
        var decoder = html.facts.EVENT[eventType].decoder
        if (tagger) {
          decoder = A2(_elm_lang$core$Native_Json.map1, tagger, decoder)
        }

        events.push(A2(_brian_watkins$elmer$Elmer_Types$HtmlEvent, eventType, decoder))
      }
    }
    return _elm_lang$core$Native_List.fromArray(events)
  }

  var getFacts = function(facts) {
    var clonedFacts = JSON.parse(JSON.stringify(facts))
    delete clonedFacts.EVENT
    return clonedFacts
  }

  var constructHtmlNode = function(html, tagger) {
    var node = html;
    if (html.type == "tagger") {
      node = html.node
      tagger = html.tagger
    }

    return A4(_brian_watkins$elmer$Elmer_Types$HtmlNode,
      node.tag,
      JSON.stringify(getFacts(node.facts)),
      getChildren(node, tagger),
      getHtmlEvents(node, tagger)
    );
  }

  var asHtmlNode = function(html) {
    if (html.type == "text") {
      return _elm_lang$core$Maybe$Nothing;
    }

    return _elm_lang$core$Maybe$Just(constructHtmlNode(html))
  }

  var asIntention = function(intention) {
    if (intention.type == "leaf") {
      return _brian_watkins$elmer$Elmer_Platform$Leaf(asLeafData(intention))
    }

    if (intention.type == "map") {
      return _brian_watkins$elmer$Elmer_Platform$Tree(asTreeData(intention))
    }

    if (intention.type == "node") {
      return _brian_watkins$elmer$Elmer_Platform$Batch(asBatch(intention))
    }

    return _brian_watkins$elmer$Elmer_Platform$Unknown
  }

  var asLeafData = function(intention) {
    return A2(_brian_watkins$elmer$Elmer_Platform$LeafData,
      intention,
      intention.home
    );
  }

  var asTreeData = function(intention) {
    var tagger = intention.tagger
    var mappedIntention = intention.tree

    return (A2)(_brian_watkins$elmer$Elmer_Platform$TreeData,
      mappedIntention,
      tagger
    );
  }

  var asBatch = function(intention) {
    var intentions = _elm_lang$core$Native_List.toArray(intention.branches)
    return _elm_lang$core$Native_List.fromArray(intentions)
  }

  var intentionValue = function(intention) {
    return intention.value
  }

  var getHttpRequestBody = function(requestData) {
    var bodyType = requestData.body.ctor
    if (bodyType == "StringBody") {
      return _elm_lang$core$Maybe$Just(requestData.body._1)
    }

    return _elm_lang$core$Maybe$Nothing
  }

  var asHttpHeader = function(headerData) {
    return A2(_brian_watkins$elmer$Elmer_Types$HttpHeader,
      headerData._0,
      headerData._1)
  }

  var getHttpHeaders = function(requestData) {
    var headerData = _elm_lang$core$Native_List.toArray(requestData.headers)
    var headers = []

    headerData.forEach(function(data) {
      headers.push(asHttpHeader(data))
    })

    return _elm_lang$core$Native_List.fromArray(headers)
  }

  var asHttpRequest = function(request) {
    var requestData = request._0

    return A5(_brian_watkins$elmer$Elmer_Http$HttpRequest,
      requestData.method,
      requestData.url,
      getHttpHeaders(requestData),
      getHttpRequestBody(requestData),
      requestData.expect.responseToResult)
  }

  var toIntention = function(home, data) {
    return _elm_lang$core$Native_Platform.leaf(home)(data)
  }

  var swizzledFunctions = {}

  var restoreSwizzled = function(x) {
    for (var func in swizzledFunctions) {
      eval(func + " = swizzledFunctions[func]");
    }
    swizzledFunctions = {}

    return true
  }

  var findFunctionToSwizzle = function(fun) {
    var name = null;
    try {
      var re = /return ([\w$]+);/
      name = re.exec(fun.toString())[1]
    } catch (e) {}

    return name;
  }

  var swizzle = function(fun1, fun2) {
    var methodToSwizzle = findFunctionToSwizzle(fun1)

    if (!methodToSwizzle) {
      return false
    }

    swizzledFunctions[methodToSwizzle] = eval(methodToSwizzle)
    eval(methodToSwizzle + " = fun2")

    return true
  }

  return {
      asHtmlNode: asHtmlNode,
      asIntention: asIntention,
      intentionValue: intentionValue,
      asHttpRequest: asHttpRequest,
      toIntention: F2(toIntention),
      swizzle: F2(swizzle),
      restoreSwizzled: restoreSwizzled
  };

}();
