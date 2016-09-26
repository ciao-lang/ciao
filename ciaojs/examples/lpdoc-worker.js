importScripts("ciao/ciao.js");
importScripts("ciao/ciao.bundle.js");
importScripts("ciao/core.bundle.js");
importScripts("ciao/builder.bundle.js");
importScripts("ciao/lpdoc.bundle.js");

this.onmessage = function(event) {
  var path = "/draft.pl";
  var text = event.data;
  Ciao.getFS().writeFile(path, text, {encoding: 'utf8'});
  var out = Ciao.query(null, "doc_cmd('"+path+"', [], gen(html))");
  //
  if (out.sols.length == 1 && out.sols[0] == "success([])") {
    console.log('[Processed in '+ out.time + ' ms]');
    var str = Ciao.getFS().readFile("/draft.html/draftintro.html", {encoding: 'utf8'});
    postMessage(str);
  } else {
    console.log(JSON.stringify(out));
  }
};

Ciao.init("ciao/", (function () {
    Ciao.run("internals:$bootversion");
    console.log('[Engine loaded in '+
                Ciao.stats.runtime_engine_load +
                ' ms, boot in ' +
                Ciao.stats.runtime_boot + ' ms]');
    console.log('Loading LPdoc... Please wait');
    postMessage('<div class="status">Loading LPdoc... Please wait</div>');
    out = Ciao.query("true", "use_module(lpdoc(docmaker))");
    postMessage('<div class="status">LPdoc ready!</div>');
    console.log(JSON.stringify(out));
}));

