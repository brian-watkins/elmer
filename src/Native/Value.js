var _brian_watkins$elmer$Native_Value = function() {

  var decode = function(decoder, value) {
    return A2(_elm_lang$core$Json_Decode$decodeValue, decoder, value)
  }

  return {
      decode: F2(decode)
  };

}();
