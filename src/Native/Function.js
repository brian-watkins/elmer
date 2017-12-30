var _brian_watkins$elmer$Native_Function = function() {

  var globalIdentifier = function(fun) {
    var name = null;
    try {
      var re = /return ([\w$]+);/
      name = re.exec(fun.toString())[1]
    } catch (e) {}

    return name;
  }

  var fakeFunctions = {}

  var activate = function(name, func) {
    fakeFunctions[name] = {
      impl: func,
      calls: []
    }

    return func
  }

  var deactivate = function (name) {
    var calls = fakeFunctions[name].calls

    delete fakeFunctions[name]

    return calls
  }

  var active = function(name) {
    return fakeFunctions[name] ? fakeFunctions[name].impl : null
  }

  var storeArg = function(name, arg, currentCall) {
    var callList = fakeFunctions[name].calls
    var callId = currentCall

    if (callId === undefined) {
      callId = callList.length
      callList[callId] = []
    }

    callList[callId].push(arg)

    return callId
  }

  var recordable = function(name, func, currentCall) {
    return function() {
      var callId = storeArg(name, arguments[0], currentCall)

      var next = func.apply(this, arguments)
      if (typeof(next) !== "function") {
        return next
      }

      return recordable(name, next, callId)
    }
  }

  return {
    recordable: F2(recordable),
    globalIdentifier: globalIdentifier,
    active: active,
    activate: F2(activate),
    deactivate: deactivate
  }

}();
