/*
 *  ciao.js
 *
 *  High-level interface to Ciao compiled to asm.js (via Emscripten)
 *
 *  Copyright (C) 2016 Jose F. Morales
 */

var Ciao = (function() {
  var Module; // Engine compiled through Emscripten
  var FS;     // Emscripten FS

  var prev_time = null;
  function now() { return (new Date()).getTime(); }
  function startTimer() { prev_time = now(); }
  function checkTimer() { return (now() - prev_time); } /* milliseconds */

  var Ciao = {};
  Ciao.stats = {};    // Statistics
  Ciao.bundle = {};   // Bundle map
  Ciao.requires = []; // Required bundles
  Ciao.prefixURL = "";

  Ciao.bind_funcs = function() {
    Ciao.init = Module.cwrap('ciaojs_init', 'number', ['string']);
    Ciao.boot = Module.cwrap('ciaojs_boot', 'number', []);
    Ciao.run = Module.cwrap('ciaojs_run', 'number', ['string']);
  };

  function mkdir_noerr(path) {
    try { FS.mkdir(path); } catch(e) { /* ignore errors */ };
  }

  Ciao.preload_file = function(bundle,kind,path) {
    var srcdir;
    var srcurl;
    if (kind == 'bundlereg') {
      srcdir = bundle.regdir;
      srcurl = Ciao.prefixURL;
    } else {
      srcdir = bundle.srcdir;
      srcurl = Ciao.prefixURL + bundle.srcurl;
    }
    var spath = (srcdir+'/'+path).split('/');

    /* Create directory path */
    var len = spath.length - 1;
    var dir = "";
    if (len > 0) {
      dir += spath[0];
      mkdir_noerr(dir);
      for (var i = 1; i < len; i++) {
	dir += "/"+spath[i];
	mkdir_noerr(dir);
      }
    }
    var mod = spath[len];

    /* Create preloaded file */
    function create(ext) {
      FS.createPreloadedFile(dir, mod+ext, srcurl+'/'+path+ext, true, false);
    }
    if (kind == 'mod') {
      create(".pl");
      create(".po");
      create(".itf");
    } else if (kind == 'src') {
      create(".pl");
    } else {
      create("");
    }
  }

  /* --------------------------------------------------------------------------- */
  /* Initializing Ciao.js */

  Ciao.init = function(prefixURL, onready) {
    /* Start the engine with hooks for initialization */
    Module = {};
    Module.noExitRuntime = true;
    Module.memoryInitializerPrefixURL = prefixURL;
    Ciao.prefixURL = prefixURL;
    //Module.filePackagePrefixURL = prefixURL;
    Module.preRun = [];
    Module.onRuntimeInitialized = function() {
      Ciao.bind_funcs();
      startTimer();
      Ciao.init(Ciao.bootfile);
      /* Boot the engine and execute ciaojs:main/0 (which exits with a
         live runtime) */
      Ciao.boot();
      Ciao.stats.runtime_boot = checkTimer();
      onready();
    };
    /* Preload binary and data */
    Module.preRun.push(function() {
      FS = Module['FS'];
      for (var i = 0; i < Ciao.requires.length; i++) {
	Ciao.bundle[Ciao.requires[i]].preload();
      }
      FS.createPreloadedFile('/', Ciao.bootfile, Ciao.prefixURL + Ciao.bootfile, true, false);
    });
    /* Begin execution of asmjs compiled engine (this calls other hooks) */
    startTimer();
    Module.preRun.push(function() {
      Ciao.stats.runtime_engine_load = checkTimer();
    });
    CIAOENGINE.run(Module);
  };

  /* --------------------------------------------------------------------------- */

  Ciao.bootfile='ciaojs';

  /* See ciaojs:query_fs/0 */
  Ciao.query = function(template, goal) {
    var query;
    if (template === null) {
      query = 'notmpl(('+goal+')).';
    } else {
      query = 'tmpl(('+template+'),('+goal+')).';
    }
    FS.writeFile('/.ciaojs-in.pl', query, {encoding: 'utf8'});
    startTimer();
    Ciao.run("ciaojs:query_fs");
    var time = checkTimer();
    var out = FS.readFile('/.ciaojs-out.pl', { encoding: 'utf8' })
    /* Cannonical output ensure that we do not have line breaks inside a solution */ 
    return { sols: out.trim().split("\n"), time: time };
  };

  /* --------------------------------------------------------------------------- */

  /* Get Emscripten FS */
  Ciao.getFS = function() { return FS; };

  /* --------------------------------------------------------------------------- */

  /* Save `text` into `path` and do use_module/1 */
  Ciao.use_module_from_string = function(path, text) {
    FS.writeFile(path, text, {encoding: 'utf8'});
    return Ciao.query(null, "use_module('"+path+"')");
  };

  return Ciao;
}());
