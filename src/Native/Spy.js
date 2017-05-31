var _brian_watkins$elmer$Native_Spy = function() {

  var findSpyFunction = function(fun) {
    var name = null;
    try {
      var re = /return ([\w$]+);/
      name = re.exec(fun.toString())[1]
    } catch (e) {}

    return name;
  }

  var create = function(name, fun) {
    var functionToSpyOn = findSpyFunction(fun)

    if (!functionToSpyOn) {
      return _brian_watkins$elmer$Elmer_Spy_Internal$Error({ name: name, calls: [] });
    }

    var spyValue =
      { name: name
      , calls: []
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
      calls: spyValue.calls.slice(),
      functionName: spyValue.functionName,
      original: spyValue.original,
      fake: spyValue.fake
    }

    eval(spyValueCopy.functionName + " = createSpyCall(spyValueCopy, spyValueCopy.fake)")

    return _brian_watkins$elmer$Elmer_Spy_Internal$Active(spyValueCopy)
  }

  var recordCall = function(spyValue, functionArguments) {
    var args = []
    for (var p in functionArguments) {
      var arg = functionArguments[p]
      var argValue = null;

      switch (typeof arg) {
        case "string":
          argValue = _brian_watkins$elmer$Elmer_Spy_Internal$StringArg(arg)
          break

        case "number":
          if (Number.isInteger(arg)) {
            argValue = _brian_watkins$elmer$Elmer_Spy_Internal$IntArg(arg)
          } else {
            argValue = _brian_watkins$elmer$Elmer_Spy_Internal$FloatArg(arg)
          }
          break

        case "boolean":
          argValue = _brian_watkins$elmer$Elmer_Spy_Internal$BoolArg(arg)
          break

        case "object":
          argValue = _brian_watkins$elmer$Elmer_Spy_Internal$TypedArg(_elm_lang$core$Native_Utils.toString(arg))
          break

        case "function":
          argValue = _brian_watkins$elmer$Elmer_Spy_Internal$FunctionArg
          break
      }

      args.push(argValue)
    }

    spyValue.calls.push(args)
  }

  var createSpyCall = function(spyValue, targetFunction) {
    if (targetFunction.func && targetFunction.arity) {
      return createWrappedSpyCall(spyValue, targetFunction)
    }

    var wrapper = function () {
      recordCall(spyValue, arguments)
      return targetFunction.apply(this, arguments)
    }

    return wrapper;
  }

  var createWrappedSpyCall = function(spyValue, targetFunction) {
    var innerSpy = createSpyCall(spyValue, targetFunction.func)

    var wrapper = function () {
      var factory = eval("F" + targetFunction.arity)
      return factory(innerSpy).apply(this, arguments)
    }

    wrapper.arity = targetFunction.arity
    wrapper.func = innerSpy

    return wrapper;
  }

  var registerFake = function(spyValue, fakeFun) {
    spyValue.fake = fakeFun

    return _brian_watkins$elmer$Elmer_Spy_Internal$Active(spyValue);
  }

  var calls = function(spyValue) {
    var callList = spyValue.calls.map(function (c) {
      return _elm_lang$core$Native_List.fromArray(c)
    })

    var spyCalls = (A2)(_brian_watkins$elmer$Elmer_Spy_Internal$Calls,
      spyValue.name,
      _elm_lang$core$Native_List.fromArray(callList)
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
