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
    return A4(_bwatkinsPivotal$elmer$Elmer$HtmlNode,
      html.tag,
      JSON.stringify(html.facts),
      getChildren(html),
      getHtmlEvents(html)
    );
  }

  var asHtmlNode = function(html) {
    if (html.type == "node") {
      return _elm_lang$core$Maybe$Just(constructHtmlNode(html))
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

  var browserState = { location: _elm_lang$core$Maybe$Nothing }

  var runCommand = function(command) {
    if (command.type == 'leaf' && command.home == 'Task') {
      return _elm_lang$core$Maybe$Just(runSimpleTask(command.value))
    }
    else if (command.type == 'leaf' && command.home == 'Navigation') {
      browserState.location = _elm_lang$core$Maybe$Just(command.value._0)
    }

    return _elm_lang$core$Maybe$Nothing;
  }

  return {
      asHtmlNode: asHtmlNode,
      getMessageForEvent: F2(getMessageForEvent),
      runCommand: runCommand,
      browserState: browserState
  };

}();
