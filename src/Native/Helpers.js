var _bwatkinsPivotal$elmer$Native_Helpers = function() {

  var getMessageForEvent = function(decoder, value) {
    var valueObj = JSON.parse(value)
    return A2(_elm_lang$core$Native_Json.run, decoder, valueObj)
  }

  var getEvent = function(eventType, html) {
    if (html.facts.EVENT && html.facts.EVENT[eventType]) {
      return _elm_lang$core$Maybe$Just(A2(_bwatkinsPivotal$elmer$Elmer$HtmlEvent, eventType, html.facts.EVENT[eventType].decoder));
    }

    return _elm_lang$core$Maybe$Nothing;
  }

  var getTextChildren = function(html) {
    return html.children.filter(function(n) {
      return (n.type == "text");
    }).map(function(n) {
      return _bwatkinsPivotal$elmer$Elmer$Text(n.text);
    });
  }

  var getId = function(html) {
    return (html.facts.id ? _elm_lang$core$Maybe$Just(html.facts.id) : _elm_lang$core$Maybe$Nothing);
  }

  var getClasses = function(html) {
    if (html.facts.className) {
        var classes = html.facts.className.split(" ");
        return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_List.fromArray(classes));
    }

    return _elm_lang$core$Maybe$Nothing;
  }

  var getHtmlEvents = function(html) {
    var clickEvent = getEvent("click", html);
    var inputEvent = getEvent("input", html);

    if (inputEvent != _elm_lang$core$Maybe$Nothing || clickEvent != _elm_lang$core$Maybe$Nothing) {
      return _elm_lang$core$Maybe$Just(A2(_bwatkinsPivotal$elmer$Elmer$HtmlEvents, clickEvent, inputEvent));
    }

    return _elm_lang$core$Maybe$Nothing;
  }

  var getChildren = function(html) {
    return _bwatkinsPivotal$elmer$Elmer$HtmlElementList(_elm_lang$core$Native_List.fromArray(getTextChildren(html)));
  }

  var constructHtmlNode = function(html) {
    return A5(_bwatkinsPivotal$elmer$Elmer$HtmlNode,
      html.tag,
      getId(html),
      getClasses(html),
      getChildren(html),
      getHtmlEvents(html)
    );
  }

  var containsClass = function(classNames, selector) {
    if (classNames == undefined) return false

    var classes = classNames.split(" ").map(function(c) { return "." + c });

    for (var i = 0; i < classes.length; i++) {
      if (classes[i] == selector) {
        return true
      }
    }

    return false
  }

  var hasId = function(htmlId, selector) {
    return selector == "#" + htmlId;
  }

  var findHtmlNode = function(selector, html) {
    if (html.type == "node") {
      if (hasId(html.facts.id, selector) || containsClass(html.facts.className, selector)) {
        return _elm_lang$core$Maybe$Just(constructHtmlNode(html))
      }
      else {
        var foundNode = html.children.find(function(node) {
          return (findHtmlNode(selector, node) != _elm_lang$core$Maybe$Nothing);
        })
        if (foundNode != undefined) {
          return _elm_lang$core$Maybe$Just(constructHtmlNode(foundNode))
        }
      }
    }

    return _elm_lang$core$Maybe$Nothing;
  }

  return {
      findHtmlNode: F2(findHtmlNode),
      getMessageForEvent: F2(getMessageForEvent),
  };

}();
