var _brian_watkins$elmer$Native_Platform = function() {

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
      asIntention: asIntention,
      intentionValue: intentionValue,
      toIntention: F2(toIntention),
      swizzle: F2(swizzle),
      restoreSwizzled: restoreSwizzled
  };

}();
