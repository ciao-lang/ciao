:- module(compressed_bytecode,
        [compressLZ/1,
         copyLZ/1],
         [assertions]).

:- doc(title, "Compression and Uncompression of Bytecode.").

:- doc(author, "Oscar Portela Arjona").

% (defined in engine/io_basic.c)
:- impl_defined(compressLZ/1).
:- impl_defined(copyLZ/1).

