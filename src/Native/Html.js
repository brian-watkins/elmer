var _brian_watkins$elmer$Native_Html = function() {

  var getChildren = function(html, inheritedEvents, tagger) {
    var children = []
    for (var i = 0; i < html.children.length; i++) {
      var element = html.children[i]
      if (element.type == "text") {
        children.push(_brian_watkins$elmer$Elmer_Html_Types$Text(element.text))
      } else {
        children.push(_brian_watkins$elmer$Elmer_Html_Types$Element(constructHtmlElement(element, inheritedEvents, tagger)))
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

        events.push(A2(_brian_watkins$elmer$Elmer_Html_Types$HtmlEvent, eventType, decoder))
      }
    }
    return _elm_lang$core$Native_List.fromArray(events)
  }

  var getFacts = function(facts) {
    var clonedFacts = JSON.parse(JSON.stringify(facts))
    delete clonedFacts.EVENT
    return clonedFacts
  }

  var concatLists = function (list_1, list_2) {
    var array_1 = _elm_lang$core$Native_List.toArray(list_1);
    var array_2 = _elm_lang$core$Native_List.toArray(list_2);

    var all = array_1.concat(array_2);

    return _elm_lang$core$Native_List.fromArray(all);
  }

  var constructHtmlElement = function(html, inheritedEvents, tagger) {
    var node = html;
    if (html.type == "tagger") {
      node = html.node
      tagger = html.tagger
    }

    var events = getHtmlEvents(node, tagger);
    var eventsToInherit = concatLists(inheritedEvents, events);

    return A5(_brian_watkins$elmer$Elmer_Html_Types$HtmlElement,
      node.tag,
      JSON.stringify(getFacts(node.facts)),
      getChildren(node, eventsToInherit, tagger),
      inheritedEvents,
      events
    );
  }

  var asHtmlElement = function(html) {
    if (html.type == "text") {
      return _elm_lang$core$Maybe$Nothing;
    }

    var inheritedEvents = _elm_lang$core$Native_List.fromArray([])

    return _elm_lang$core$Maybe$Just(constructHtmlElement(html, inheritedEvents))
  }

  return {
      asHtmlElement: asHtmlElement
  };

}();
