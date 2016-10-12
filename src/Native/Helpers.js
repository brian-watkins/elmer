var _bwatkinsPivotal$elmer$Native_Helpers = function() {

  var getMessageForEvent = function(decoder, value) {
    var valueObj = JSON.parse(value)
    return A2(_elm_lang$core$Native_Json.run, decoder, valueObj)
  }

  var getChildren = function(html) {
    var children = []
    for (var i = 0; i < html.children.length; i++) {
      var element = html.children[i]
      if (element.type == "text") {
        children.push(_bwatkinsPivotal$elmer$Elmer$Text(element.text))
      } else {
        children.push(_bwatkinsPivotal$elmer$Elmer$Node(constructHtmlNode(element)))
      }
    }

    return _elm_lang$core$Native_List.fromArray(children)
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
    var events = []
    if (html.facts.EVENT) {
      for (var eventType in html.facts.EVENT) {
        events.push(A2(_bwatkinsPivotal$elmer$Elmer$HtmlEvent, eventType, html.facts.EVENT[eventType].decoder))
      }
    }
    return _elm_lang$core$Native_List.fromArray(events)
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
        for (var i = 0; i < html.children.length; i++) {
          var node = html.children[i]
          var foundNode = findHtmlNode(selector, node)
          if (foundNode != _elm_lang$core$Maybe$Nothing) {
            return foundNode
          }
        }
      }
    }

    return _elm_lang$core$Maybe$Nothing;
  }

  var runSimpleTask = function(task) {
    var errorCallback = task._0.callback
    var successCallback = task._0.task.callback
    var rootTask = task._0.task.task

    if (rootTask.ctor == '_Task_succeed') {
      return successCallback(rootTask.value).value
    }

    return errorCallback(rootTask.value).value
  }

  var runCommand = function(command) {
    if (command.type == 'leaf' && command.home == 'Task') {
      return runSimpleTask(command.value)
    }
  }

  return {
      findHtmlNode: F2(findHtmlNode),
      getMessageForEvent: F2(getMessageForEvent),
      runCommand: runCommand
  };

}();
