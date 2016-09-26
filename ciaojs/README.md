# Ciao.js - Ciao compiled to Javascript (using Emscripten)

# Requirements

 - Recent Ciao version (with support for the LINUXx86_JS platform,
   which is implemented by Emscripten)

 - Emscripten

# Preparing Emscripten

Get latest emscripten from:

http://kripken.github.io/emscripten-site/docs/getting_started/downloads.html

We recommend the Emscripten SDK. After download, it can be installed with:
```
$ ./emsdk install latest
$ ./emsdk activate latest
```

On recent versions of Mac OS X, you may need to create a symbolic link to `python2`:
```
$ ln -sf /usr/bin/python2.7 /usr/local/bin/python2
```

To use `emsdk` you must update your environment:
```
$ source ./emsdk_sh
```

# Build instructions

Build the engine and support bytecode binaries:
```
./emciao.sh build
```

Install:
```
./emciao.sh install
```

# Ciao JavaScript API

The API implements a high level interface to Ciao engine, which is
able to run queries and collect solutions. See `ciaojs.pl` for
internal details.

See `examples/` for usage examples. Use the script `./runme.sh` to
prepare and start a test server.

# Caveats

 - Currently Emscripten code cannot easily suspend the execution (at
   least not with some cost).

 - The engine does function pointer casting, which requires the following
   options `-s ALIASING_FUNCTION_POINTERS=0 -s EMULATE_FUNCTION_POINTER_CASTS=1`
   in `emcc`. This may generate less optimal code.

