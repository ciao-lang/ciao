// Encapsulated Ciao engine
var CIAOENGINE = {
  run: function(inmod) {
    var Module = inmod;
    // Make FS visible
    if (!Module['preRun']) Module['preRun'] = [];
    Module['preRun'].push(function() { Module['FS'] = FS; });
