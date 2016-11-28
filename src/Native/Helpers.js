var _bwatkinsPivotal$elmer$Native_Helpers = function() {

  var getChildren = function(html, tagger) {
    var children = []
    for (var i = 0; i < html.children.length; i++) {
      var element = html.children[i]
      if (element.type == "text") {
        children.push(_bwatkinsPivotal$elmer$Elmer_Types$Text(element.text))
      } else {
        children.push(_bwatkinsPivotal$elmer$Elmer_Types$Node(constructHtmlNode(element, tagger)))
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

        events.push(A2(_bwatkinsPivotal$elmer$Elmer_Types$HtmlEvent, eventType, decoder))
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

    return A4(_bwatkinsPivotal$elmer$Elmer_Types$HtmlNode,
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

  var runTask = function(command) {
    var task = command.value

    if (task._0.ctor == '_Task_andThen') {
      return _elm_lang$core$Result$Ok(task._0.callback(task._0.task.value).value)
    }

    var errorCallback = task._0.callback
    var successCallback = task._0.task.callback
    var rootTask = task._0.task.task

    if (rootTask.ctor == '_Task_succeed') {
      return _elm_lang$core$Result$Ok(successCallback(rootTask.value).value)
    }

    var errorValue = rootTask.value
    if (errorValue.elmerError) {
        return _elm_lang$core$Result$Err(errorValue.elmerError)
    }

    return _elm_lang$core$Result$Ok(errorCallback(errorValue).value)
  }

  var asCommandData = function(command) {
    if (command.type == "leaf") {
      return _bwatkinsPivotal$elmer$Elmer_Runtime$LeafCommand(asLeafCommandData(command))
    }

    if (command.type == "map") {
      return _bwatkinsPivotal$elmer$Elmer_Runtime$MapCommand(asMapCommandData(command))
    }

    if (command.type == "node") {
      return _bwatkinsPivotal$elmer$Elmer_Runtime$BatchCommand(asBatchCommandData(command))
    }

    return _bwatkinsPivotal$elmer$Elmer_Runtime$NoCommand
  }

  var asLeafCommandData = function(command) {
    return A3(_bwatkinsPivotal$elmer$Elmer_Runtime$LeafCommandData,
      command,
      command.home,
      JSON.stringify(command.value)
    );
  }

  var asMapCommandData = function(command) {
    var tagger = command.tagger
    var mappedCommand = command.tree

    return (A2)(_bwatkinsPivotal$elmer$Elmer_Runtime$MapCommandData,
      mappedCommand,
      tagger
    );
  }

  var asBatchCommandData = function(command) {
    var commands = _elm_lang$core$Native_List.toArray(command.branches)
    return _elm_lang$core$Native_List.fromArray(commands)
  }

  var asHttpRequest = function(request) {
    var requestData = request._0

    return A3(_bwatkinsPivotal$elmer$Elmer_Http$HttpRequest,
      requestData.method,
      requestData.url,
      requestData.expect.responseToResult)
  }

  return {
      asHtmlNode: asHtmlNode,
      asCommandData: asCommandData,
      runTask: runTask,
      asHttpRequest: asHttpRequest,
  };

}();
