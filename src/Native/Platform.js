var _brian_watkins$elmer$Native_Platform = function() {

  var asIntention = function(intention) {
    if (intention.type == "leaf") {
      return _brian_watkins$elmer$Elmer_Platform_Internal$Leaf(asLeafData(intention))
    }

    if (intention.type == "map") {
      return _brian_watkins$elmer$Elmer_Platform_Internal$Tree(asTreeData(intention))
    }

    if (intention.type == "node") {
      return _brian_watkins$elmer$Elmer_Platform_Internal$Batch(asBatch(intention))
    }

    return _brian_watkins$elmer$Elmer_Platform_Internal$Unknown
  }

  var asLeafData = function(intention) {
    return A2(_brian_watkins$elmer$Elmer_Platform_Internal$LeafData,
      intention,
      intention.home
    );
  }

  var asTreeData = function(intention) {
    var tagger = intention.tagger
    var mappedIntention = intention.tree

    return (A2)(_brian_watkins$elmer$Elmer_Platform_Internal$TreeData,
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

  var toIntention = function(home, data) {
    return _elm_lang$core$Native_Platform.leaf(home)(data)
  }

  return {
      asIntention: asIntention,
      intentionValue: intentionValue,
      toIntention: F2(toIntention)
  };

}();
