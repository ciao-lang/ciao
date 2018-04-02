/*
 *  ciao-actmod.js
 *
 *  JavaScript client for Ciao active modules (see actmod_http.pl)
 *
 *  Copyright (C) 2017 Jose F. Morales
 */

/* --------------------------------------------------------------------------- */

var rpc_actmod = '';
var rpc_url_prefix = '';
var rpc_show_status = (function(msg) {});
var rpc_retry_timeout = 500;
var rpc_retry_limit = 10;

/* Client-side commands (from JS) */
var run_cmd = {};

/* --------------------------------------------------------------------------- */

var rpc_run = (function() {
  function get_rpc_answer(e) {
    return JSON.parse(e.target.responseText);
  }

  var cached_rpc = {};
  var rpc_retry_count = 0;

  /* Do a RPC call (consuming all intermedicate calls) */
  function rpc_run(actmod, cmd, data, opts, done) {
    rpc_retry_count = 0;
    rpc_step(actmod, cmd, data, opts, (ans) => {
      rpc_next(ans['cont'], 0, done);
    });
  }

  /* One-step RPC. Result is:
     - { not_ready: ... } : if the server is not ready
     - { cont: [...] } : an array of commands for the
       continuation, which may be local commands or commands of a remote
       active module

     Each command has the form:
     - { cmd: Name, data: Data }, where
       the optional 'remote' field can be:
         - normal_step: using POST HTTP method
         - cached_step: caching locally this step (using GET HTTP method and local cache)
         - nosideff_step: nosideff step (using GET HTTP method)
  */
  /* TODO: allow 'async' for not 'remote' */
  function rpc_step(actmod, cmd, data, opts, next) {
    if (opts['method'] == 'cached_get') {
      var query_str = get_query_str(actmod, cmd, data);
      if (cached_rpc[query_str] === undefined) {
	rpc_GET(actmod, query_str, function(a) {
	  if (a['not_ready'] !== undefined) {
	    retry_rpc(a['not_ready'], actmod, cmd, data, opts, next);
	  } else {
	    cached_rpc[query_str] = a;
	    next(a);
	  }
	});
      } else {
	next(cached_rpc[query_str]);
      }
    } else if (opts['method'] == 'get') {
      rpc_GET(actmod, get_query_str(actmod, cmd, data), function(a) {
	if (a['not_ready'] !== undefined) {
	  retry_rpc(a['not_ready'], actmod, cmd, data, opts, next);
	} else {
	  next(a);
	}
      });
    } else {
      rpc_POST(actmod, get_form_data(actmod, cmd, data), function(a) {
	if (a['not_ready'] !== undefined) {
	  retry_rpc(a['not_ready'], actmod, cmd, data, opts, next);
	} else {
	  next(a);
	}
      });
    }
  }

  /* Retry the one-step RPC */
  function retry_rpc(msg, actmod, cmd, data, opts, next) {
    rpc_retry_count += 1;
    if (rpc_retry_count > rpc_retry_limit) {
      /* Limit excedeed, abort */
      rpc_show_status(msg + ". Aborted (retry limit excedeed)");
      setTimeout(function() { 
	rpc_show_status(null);
      }, rpc_retry_timeout);
    } else {
      /* Try again after a timeout */
      rpc_show_status(msg + ". Retrying in " + rpc_retry_timeout + " ms...");
      setTimeout(function() { 
	rpc_show_status(null);
	rpc_step(actmod, cmd, data, opts, next);
      }, rpc_retry_timeout);
    }
  }

  // xhr.responseType = 'json';
  // xhr.responseType = 'text';
  // xhr.setRequestHeader('Accept', 'application/json');

  // response.setHeader( "Pragma", "no-cache" );
  // response.setHeader( "Cache-Control", "no-cache" );
  // response.setDateHeader( "Expires", 0 );   

  /* Do a remote call with GET method (i.e. do not change server
   * state, may be cached) */
  function rpc_GET(actmod, query_str, next) {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', rpc_url_prefix + '/' + actmod + '?' + query_str, true);
    xhr.onload = function(e) {
      next(get_rpc_answer(e));
    };
    xhr.send(null);
  }

  /* Do a remote call with POST method (i.e. may have side-effects on the server) */
  function rpc_POST(actmod, data, next) {
    var xhr = new XMLHttpRequest();
    xhr.open("POST", rpc_url_prefix + '/' + actmod);
    xhr.onload = function(e) {
      next(get_rpc_answer(e));
    };
    xhr.send(data);
  }

  /* TODO: use promises, better for stack */
  function rpc_next(cont, i0, done) {
    if (cont === undefined) return;
    for (var i = i0; i < cont.length; i++) {
      var n = cont[i];
      var mt = n['remote'];
      if (mt !== undefined) { /* remote */
	var opts;
	switch(mt) {
	case 'cached_step': opts = { method: 'cached_get' }; break;
	case 'nosideff_step': opts = { method: 'get' }; break;
	default: opts = {}; break;
	};
	var rest;
	if (i == cont.length - 1) {
	  rest = done;
	} else {
	  /* Keep rest of cont for continuation */
	  rest = (res) => {
	    rpc_next(cont, i + 1, done);
	  };
	}
	rpc_run(rpc_actmod, n['cmd'], n['data'], opts, rest);
	return;
      } else { /* try local */
	run_cmd[n['cmd']](n['data']);
      }
    }
    done(true);
  }

  /* Create URI string (for XHR through GET) */
  function get_query_str(actmod, cmd, data) {
    return ('actmod='+encodeURIComponent(actmod)+
            '&cmd='+encodeURIComponent(cmd)+
            '&data='+encodeURIComponent(JSON.stringify(data)));
  }

  /* Create FormData object (for XHR through POST) */
  function get_form_data(actmod, cmd, data) {
    var formData = new FormData();
    var blob1 = undefined;
    var old_blob1 = undefined;
    formData.append("actmod", actmod);
    formData.append("cmd", cmd);
    if (data['blob1'] !== undefined) { /* pass in form instead (it can be large) */
      blob1 = data['blob1'];
      old_blob1 = data['blob1']; delete data['blob1']; /* TODO: copy 'data' instead? */
    }
    if (data['file'] instanceof File) { /* pass as file/blob */
      // TODO: remove from data? 
      blob1 = data['file'];
    }
    if (blob1 !== undefined) {
      formData.append("blob1", blob1);
    }
    formData.append("data", JSON.stringify(data));
    if (old_blob1) {
      data['blob1'] = old_blob1; /* TODO: copy 'data' instead? */
    }
    return formData;
  }

  return rpc_run;
})();

