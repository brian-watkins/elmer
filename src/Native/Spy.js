var _brian_watkins$elmer$Native_Spy = function() {

  var findSpyFunction = function(fun) {
    var name = null;
    try {
      var re = /return ([\w$]+);/
      name = re.exec(fun.toString())[1]
    } catch (e) {}

    return name;
  }

  var createSpyCall = function(spyValue) {
    return function () {
      spyValue.calls += 1
      return spyValue.fake.apply(this, arguments)
    }
  }

  var create = function(name, fun) {
    var functionToSpyOn = findSpyFunction(fun)

    if (!functionToSpyOn) {
      return _brian_watkins$elmer$Elmer_Spy_Internal$Error({ name: name, calls: -1 });
    }

    var spyValue =
      { name: name
      , calls: 0
      , functionName: functionToSpyOn
      , original: eval(functionToSpyOn)
      , fake: eval(functionToSpyOn)
      }

    return activate(spyValue)
  }

  var deactivate = function (spyValue) {
    eval(spyValue.functionName + " = spyValue.original")

    return _brian_watkins$elmer$Elmer_Spy_Internal$Inactive(spyValue)
  }

  var activate = function(spyValue) {
    var spyValueCopy = {
      name: spyValue.name,
      calls: spyValue.calls,
      functionName: spyValue.functionName,
      original: spyValue.original,
      fake: spyValue.fake
    }

    eval(spyValue.functionName + " = createSpyCall(spyValueCopy)")

    return _brian_watkins$elmer$Elmer_Spy_Internal$Active(spyValueCopy)
  }

  var registerFake = function(spyValue, fakeFun) {
    spyValue.fake = fakeFun

    return _brian_watkins$elmer$Elmer_Spy_Internal$Active(spyValue);
  }

  var calls = function(spyValue) {
    var spyCalls = (A2)(_brian_watkins$elmer$Elmer_Spy_Internal$Calls,
      spyValue.name,
      spyValue.calls
    );

    return spyCalls;
  }

  return {
      create: F2(create),
      deactivate: deactivate,
      activate: activate,
      calls: calls,
      registerFake: F2(registerFake)
  };

}();
