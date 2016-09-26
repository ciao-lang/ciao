var LPdoc = (function() {
  var LPdoc = {};

  var w = new Worker("lpdoc-worker.js");

  w.onmessage = function(event) {
    LPdoc.done(event.data);
  };

  LPdoc.do = function(text) {
    w.postMessage(text);
  };

  return LPdoc;
}());
