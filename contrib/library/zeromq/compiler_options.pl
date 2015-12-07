% This is necessary, because on 64 bit machines I had to add these
% options to CFLAGS and CXXFLAGS to force compilation of ZMQ into 32
% bits (with g++ multilib installed).  Therefore, for ciao to link to
% the code, one has to force 32 bits compilation.

%:- extra_compiler_opts('LINUXx86_64m32', ['-m32', '-march=i686']).

:- extra_compiler_opts(['-I./zmq_2.2.0_32/include']).
:- extra_linker_opts(['-L./zmq_2.2.0_32/lib']).
