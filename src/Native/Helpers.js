var _bwatkinsPivotal$elmer$Native_Helpers = function() {

  var getChildren = function(html, tagger) {
    var children = []
    for (var i = 0; i < html.children.length; i++) {
      var element = html.children[i]
      if (element.type == "text") {
        children.push(_bwatkinsPivotal$elmer$Elmer$Text(element.text))
      } else {
        children.push(_bwatkinsPivotal$elmer$Elmer$Node(constructHtmlNode(element, tagger)))
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
          decoder = A2(_elm_lang$core$Native_Json.decodeObject1, tagger, decoder)
        }

        events.push(A2(_bwatkinsPivotal$elmer$Elmer$HtmlEvent, eventType, decoder))
      }
    }
    return _elm_lang$core$Native_List.fromArray(events)
  }

  var constructHtmlNode = function(html, tagger) {
    var node = html;
    if (html.type == "tagger") {
      node = html.node
      tagger = html.tagger
    }

    return A4(_bwatkinsPivotal$elmer$Elmer$HtmlNode,
      node.tag,
      JSON.stringify(node.facts),
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

  var runSimpleTask = function(task) {
    var errorCallback = task._0.callback
    var successCallback = task._0.task.callback
    var rootTask = task._0.task.task

    if (rootTask.ctor == '_Task_succeed') {
      return successCallback(rootTask.value).value
    }

    return errorCallback(rootTask.value).value
  }

  var runTask = function(command) {
    return runSimpleTask(command.value)
  }

  var asCommandData = function(command) {
    if (command.type == "leaf") {
      return _bwatkinsPivotal$elmer$Elmer_Runtime$LeafCommand(asLeafCommandData(command))
    }

    if (command.type == "map") {
      return _bwatkinsPivotal$elmer$Elmer_Runtime$MapCommand(asMapCommandData(command))
    }

    return _bwatkinsPivotal$elmer$Elmer_Runtime$NoCommand
  }

  var asLocationParser = function(parser) {
    return parser._0;
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

  return {
      asHtmlNode: asHtmlNode,
      asCommandData: asCommandData,
      runTask: runTask,
      asLocationParser: asLocationParser
  };

}();
