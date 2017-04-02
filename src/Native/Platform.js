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

  var countCalls = function(name, funcName) {
    return function () {
      spies[name].calls += 1
      return swizzledFunctions[funcName](arguments[0])
    }
  };

  var spies = {}

  var spy = function(name, fun) {
    var methodToSpyOn = findFunctionToSwizzle(fun)

    if (!methodToSpyOn) {
      return false
    }

    spies[name] =
      { name: name
      , calls: 0
      }

    swizzledFunctions[methodToSpyOn] = eval(methodToSpyOn)
    eval(methodToSpyOn + " = countCalls(name, methodToSpyOn)")

    return true;
  }

  var spyData = function(name) {
    var data = spies[name]

    if (data) {
      var spyData = (A2)(_brian_watkins$elmer$Elmer_Platform_Internal$Spy,
        data.name,
        data.calls
      );

      return _elm_lang$core$Maybe$Just(spyData);
    }

    return _elm_lang$core$Maybe$Nothing;
  }

  var clearSpies = function() {
    spies = {};

    return true;
  }

  return {
      asIntention: asIntention,
      intentionValue: intentionValue,
      toIntention: F2(toIntention),
      swizzle: F2(swizzle),
      restoreSwizzled: restoreSwizzled,
      spy: F2(spy),
      spyData: spyData,
      clearSpies: clearSpies
  };

}();
