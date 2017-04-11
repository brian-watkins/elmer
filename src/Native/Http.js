var _brian_watkins$elmer$Native_Http = function() {

  var getHttpRequestBody = function(requestData) {
    var bodyType = requestData.body.ctor
    if (bodyType == "StringBody") {
      return _elm_lang$core$Maybe$Just(requestData.body._1)
    }

    return _elm_lang$core$Maybe$Nothing
  }

  var asHttpHeader = function(headerData) {
    return A2(_brian_watkins$elmer$Elmer_Http_Internal$HttpHeader,
      headerData._0,
      headerData._1)
  }

  var getHttpHeaders = function(requestData) {
    var headerData = _elm_lang$core$Native_List.toArray(requestData.headers)
    var headers = []

    headerData.forEach(function(data) {
      headers.push(asHttpHeader(data))
    })

    return _elm_lang$core$Native_List.fromArray(headers)
  }

  var asHttpRequestHandler = function(request) {
    var requestData = request._0

    var httpRequest = A4(_brian_watkins$elmer$Elmer_Http_Internal$HttpRequest,
      requestData.method,
      requestData.url,
      getHttpHeaders(requestData),
      getHttpRequestBody(requestData)
    );

    return A2(_brian_watkins$elmer$Elmer_Http_Internal$HttpRequestHandler,
      httpRequest,
      requestData.expect.responseToResult
    );
  }

  return {
      asHttpRequestHandler: asHttpRequestHandler
  };

}();
