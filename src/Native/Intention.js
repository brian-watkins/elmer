var _brian_watkins$elmer$Native_Intention = function() {

  var asIntention = function(intention) {
    if (intention.type == "leaf") {
      return _brian_watkins$elmer$Elmer_Runtime_Intention$Leaf(asLeafData(intention))
    }

    if (intention.type == "map") {
      return _brian_watkins$elmer$Elmer_Runtime_Intention$Tree(asTreeData(intention))
    }

    if (intention.type == "node") {
      return _brian_watkins$elmer$Elmer_Runtime_Intention$Batch(asBatch(intention))
    }

    return _brian_watkins$elmer$Elmer_Runtime_Intention$Unknown
  }

  var asLeafData = function(intention) {
    return A2(_brian_watkins$elmer$Elmer_Runtime_Intention$LeafData,
      intention,
      intention.home
    );
  }

  var asTreeData = function(intention) {
    var tagger = intention.tagger
    var mappedIntention = intention.tree

    return (A2)(_brian_watkins$elmer$Elmer_Runtime_Intention$TreeData,
      mappedIntention,
      tagger
    );
  }

  var asBatch = function(intention) {
    return intention.branches
  }

  var intentionValue = function(intention) {
    if (intention.type == "map") {
      return intentionValue(intention.tree)
    }

    return intention.value
  }

  //Looks like we can't call this directly in the elm code
  //But the rest of Intention.js could probably be moved into elm and just
  //use Native.Value
  var toIntention = function(home, data) {
    return _elm_lang$core$Native_Platform.leaf(home)(data)
  }

  return {
      asIntention: asIntention,
      intentionValue: intentionValue,
      toIntention: F2(toIntention)
  };

}();
