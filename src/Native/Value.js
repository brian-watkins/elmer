var _brian_watkins$elmer$Native_Value = function() {

  var global = function(name) {
    return eval(name)
  }

  var cast = function(value) {
    return value
  }

  return {
      cast: cast,
      global: global
  };

}();
