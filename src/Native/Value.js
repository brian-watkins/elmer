var _brian_watkins$elmer$Native_Value = function() {

  var global = function(name) {
    return eval(name)
  }

  var cast = function(value) {
    return value
  }

  var nativeType = function(value) {
    var type = typeof(value)
    if (type === "number") {
      if (Number.isInteger(value)) {
        type = "int"
      } else {
        type = "float"
      }
    }
    return type
  }

  var assign = function(name, value) {
    eval(name + " = value")
    return value
  }

  return {
      cast: cast,
      global: global,
      nativeType: nativeType,
      assign: F2(assign)
  };

}();
