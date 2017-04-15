var _brian_watkins$elmer$Native_Spy = function() {

  var findSpyFunction = function(fun) {
    var name = null;
    try {
      var re = /return ([\w$]+);/
      name = re.exec(fun.toString())[1]
    } catch (e) {}

    return name;
  }

  var createSpyCall = function(name, funcName) {
    return function () {
      spies[name].calls += 1
      return spies[name].fake.apply(this, arguments)
    }
  }

  var spies = {}

  var spy = function(name, fun) {
    var functionToSpyOn = findSpyFunction(fun)

    if (!functionToSpyOn) {
      return _elm_lang$core$Maybe$Nothing;
    }

    spies[name] =
      { name: name
      , calls: 0
      , functionName: functionToSpyOn
      , original: eval(functionToSpyOn)
      , fake: eval(functionToSpyOn)
      }

    eval(functionToSpyOn + " = createSpyCall(name, functionToSpyOn)")

    return _elm_lang$core$Maybe$Just(name);
  }

  var registerFake = function(name, fakeFun) {
    spies[name].fake = fakeFun

    return _elm_lang$core$Maybe$Just(name);
  }

  var callsForSpy = function(name) {
    var data = spies[name]

    if (!data) {
      return _elm_lang$core$Maybe$Nothing;
    }

    var spyCalls = (A2)(_brian_watkins$elmer$Elmer_Spy_Internal$Calls,
      data.name,
      data.calls
    );

    return _elm_lang$core$Maybe$Just(spyCalls);
  }

  var clearSpies = function() {
    for (var spyName in spies) {
      eval(spies[spyName].functionName + " = spies[spyName].original")
    }

    spies = {}

    return true;
  }

  return {
      spy: F2(spy),
      callsForSpy: callsForSpy,
      clearSpies: clearSpies,
      registerFake: F2(registerFake)
  };

}();
