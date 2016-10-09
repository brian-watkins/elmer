var _bwatkinsPivotal$elmer$Native_Helpers = function() {

  var getMessageForEvent = function(decoder, value) {
    var valueObj = JSON.parse(value)
    return A2(_elm_lang$core$Native_Json.run, decoder, valueObj)
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
    var events = []
    if (html.facts.EVENT) {
      for (var eventType in html.facts.EVENT) {
        events.push(A2(_bwatkinsPivotal$elmer$Elmer$HtmlEvent, eventType, html.facts.EVENT[eventType].decoder))
      }
    }
    return _elm_lang$core$Native_List.fromArray(events)
  }

  var getChildren = function(html) {
    return _elm_lang$core$Native_List.fromArray(getTextChildren(html))
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
