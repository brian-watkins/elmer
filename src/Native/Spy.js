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

  var install = function(name, fun) {
    var functionToSpyOn = findSpyFunction(fun)

    if (!functionToSpyOn) {
      return _brian_watkins$elmer$Elmer_Spy_Internal$Error({ name: name });
    }

    var spyValue =
      { name: name
      , calls: 0
      , installer: fun
      , functionName: functionToSpyOn
      , original: eval(functionToSpyOn)
      , fake: eval(functionToSpyOn)
      }

    eval(functionToSpyOn + " = createSpyCall(spyValue)")

    return _brian_watkins$elmer$Elmer_Spy_Internal$Active(spyValue);
  }

  var uninstall = function (spyValue) {
    eval(spyValue.functionName + " = spyValue.original")

    _brian_watkins$elmer$Elmer_Spy_Internal$Uninstalled(spyValue.installer)
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
      install: F2(install),
      uninstall: uninstall,
      calls: calls,
      registerFake: F2(registerFake)
  };

}();
