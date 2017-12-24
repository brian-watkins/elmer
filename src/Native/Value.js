var _brian_watkins$elmer$Native_Value = function() {

  var global = function(name) {
    return eval(name)
  }

  return {
      decode: _elm_lang$core$Json_Decode$decodeValue,
      global: global
  };

}();
