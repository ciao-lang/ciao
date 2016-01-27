:- module(foreign_interface_properties, [], [assertions, regtypes]).

:- push_prolog_flag(multi_arity_warnings,off).

:- doc(title, "Foreign Language Interface Properties").

:- doc(module, "The foreign language interface uses some
   properties to specify linking regimes, foreign files to be
   compiled, types of data available, memory allocation policies,
   etc.").

:- doc(summary, "The foreign language interface uses some
   properties to specify linking regimes, foreign files to be
   compiled, types of data available, memory allocation policies, etc.
   These are all the properties available and a brief explanation of
   their meaning.  Some of them can depend on the operating system and
   architecture, and can be selected stating it with an atom which
   represents that.  See @ref{Foreign Language Interface Guidelines
   and Usage} for a longer explanation and some examples.").

:- doc(author,"Jose F. Morales").
:- doc(author,"Manuel Carro").

:- doc(doinclude,use_foreign_source/1).

:- true decl use_foreign_source(Files) : atm_or_atm_list #
   "@var{Files} is the (list of) foreign file(s) that will be linked
   with the glue-code file. If the file(s) do(es) not have extension, then
   the '.c' extension will be automatically added".

:- doc(doinclude,use_foreign_source/2).

:- true decl use_foreign_source(OsArch, Files) : atm * atm_or_atm_list
   # "@var{Files} are the OS and architecture dependant foreign files.
   This allows compiling and linking different files depending on the
   O.S. and architecture.".

:- doc(doinclude,use_foreign_library/1).

:- true decl use_foreign_library(Libs) : atm_or_atm_list # "@var{Libs}
   is the (list of) external library(es) needed to link the C files.
   Only the short name of the library (i.e., what would follow the
   @tt{-l} in the linker is needed.".

:- doc(doinclude,use_foreign_library/2).

:- true decl use_foreign_library(OsArch,Libs) : atm * atm_or_atm_list
# "@var{Libs} are the OS and architecture dependant libraries.".

:- doc(doinclude,extra_compiler_opts/1).

:- true decl extra_compiler_opts(Opts) : atm_or_atm_list # "@var{Opts}
   is the list of additional compiler options (e.g., optimization
   options) that will be used during the compilation.".

:- doc(doinclude,extra_compiler_opts/2).

:- true decl extra_compiler_opts(OsArch,Opts) : atm * atm_or_atm_list
   # "@var{Opts} are the OS and architecture dependant additional
   compiler options.".

:- doc(doinclude,use_compiler/1).

:- true decl use_compiler(Compiler) : atm # "@var{Compiler} is the
   compiler to use in this file.  When this option is used, the
   default (Ciao-provided) compiler options are not used; those
   specified in @pred{extra_compiler_options} are used instead.".

:- doc(doinclude,use_compiler/2).

:- true decl use_compiler(OsArch, Compiler) : atm * atm #
   "@var{Compiler} is the compiler to use in this file when compiling
   for the architecture @var{OsArch}.  The option management is the
   same as in @pred{use_compiler/2}.".

:- doc(doinclude,extra_linker_opts/1).

:- true decl extra_linker_opts(Opts) : atm_or_atm_list # "@var{Opts}
   is the list of additional linker options that will be used during
   the linkage.".

:- doc(doinclude,extra_linker_opts/2).

:- true decl extra_linker_opts(OsArch,Opts) : atm * atm_or_atm_list #
   "@var{Opts} are the OS and architecture dependant additional linker
   options.".

:- doc(doinclude,use_linker/1).

:- true decl use_linker(Linker) : atm # "@var{Linker} is the linker to
   use in this file. When this option is used, the default
   (Ciao-provided) linker options are not used; those specified in
   @pred{extra_linker_options/1} are used instead.".

:- doc(doinclude,use_linker/2).

:- true decl use_linker(OsArch, Linker) : atm * atm # "@var{Compiler}
   is the linker to use in this file when compiling for the
   architecture @var{OsArch}.  The option management is the same as in
   @pred{use_compiler/2}.".

:- doc(doinclude,foreign_inline/2).

:- true decl foreign_inline(Term,Text) : predname * string #
   "@var{Term} is a predicate name.  @var{Text} is a source C code
   that define the predicate @var{Term}.  @var{Term} is present for
   future use with the analyzers.  Example of this can be viewed in
   the hrtimer library.".

:- true decl foreign_inline(Text) : string # "This usage
   of foreign_inline is to add globally the C source code that are in
   @var{Text}.".

:- export(any_term/1).
:- regtype any_term(X) # "@var{X} is any term. The foreign interface
   passes it to C functions as a general term.".
any_term(_).

:- export(address/1).
:- regtype address(Address) # "@var{Address} is a memory address.".
address('$address'(Address)) :-
	int(Address).

:- export(null/1).
:- regtype null(Address) # "@var{Address} is a null adress.".
null('$address'(0)).

 %% :- true prop byte(Byte) + regtype # "@var{Byte} is a byte.".
 %% :- true comp byte(T) + sideff(free).

 %% It seems this definition is not a regular type (and saying it is a
 %% property does not help): 
 %% byte(Byte) :- 
 %%         int(Byte), 
 %%         0 =< Byte,
 %%         Byte =< 255.

:- export(c_short/1).
:- regtype c_short(X) # "@var{X} is an integer in the range of C @tt{short}.".
c_short(B):- int(B).

:- export(c_int/1).
:- regtype c_int(X) # "@var{X} is an integer in the range of C @tt{int}.".
c_int(B):- int(B).

:- export(c_long/1).
:- regtype c_long(X) # "@var{X} is an integer in the range of C @tt{long}.".
c_long(B):- int(B).

:- export(c_ushort/1).
:- regtype c_ushort(X) # "@var{X} is an integer in the range of C @tt{unsigned short}.".
c_ushort(B):- int(B).

:- export(c_uint/1).
:- regtype c_uint(X) # "@var{X} is an integer in the range of C @tt{unsigned int}.".
c_uint(B):- int(B).

:- export(c_ulong/1).
:- regtype c_ulong(X) # "@var{X} is an integer in the range of C @tt{unsigned long}.".
c_ulong(B):- int(B).

:- export(c_uintptr/1).
:- regtype c_uintptr(X) # "@var{X} is an integer in the range of C @tt{uintptr_t}.".
c_uintptr(B):- int(B).

:- export(c_size/1).
:- regtype c_size(X) # "@var{X} is an integer in the range of C @tt{size_t}.".
c_size(B):- int(B).

:- export(c_float/1).
:- regtype c_float(X) # "@var{X} is an integer in the range of C @tt{float}.".
c_float(B):- num(B).

:- export(c_double/1).
:- regtype c_double(X) # "@var{X} is an integer in the range of C @tt{double}.".
c_double(B):- num(B).

:- export(c_int8/1).
:- regtype c_int8(X) # "@var{X} is an integer in the range of C @tt{int8_t}.".
c_int8(B):- int(B).

:- export(c_int8/1).
:- regtype c_int8(X) # "@var{X} is an integer in the range of C @tt{int8_t}.".
c_int8(B):- int(B).

:- export(c_int8/1).
:- regtype c_int8(X) # "@var{X} is an integer in the range of C @tt{int8_t}.".
c_int8(B):- int(B).

:- export(c_int16/1).
:- regtype c_int16(X) # "@var{X} is an integer in the range of C @tt{int16_t}.".
c_int16(B):- int(B).

:- export(c_int32/1).
:- regtype c_int32(X) # "@var{X} is an integer in the range of C @tt{int32_t}.".
c_int32(B):- int(B).

:- export(c_int64/1).
:- regtype c_int64(X) # "@var{X} is an integer in the range of C @tt{int64_t}.".
c_int64(B):- int(B).

:- export(c_uint8/1).
:- regtype c_uint8(X) # "@var{X} is an integer in the range of C @tt{uint8_t}.".
c_uint8(B):- int(B).

:- export(c_uint16/1).
:- regtype c_uint16(X) # "@var{X} is an integer in the range of C @tt{uint16_t}.".
c_uint16(B):- int(B).

:- export(c_uint32/1).
:- regtype c_uint32(X) # "@var{X} is an integer in the range of C @tt{uint32_t}.".
c_uint32(B):- int(B).

:- export(c_uint64/1).
:- regtype c_uint64(X) # "@var{X} is an integer in the range of C @tt{uint64_t}.".
c_uint64(B):- int(B).

:- export(c_uint8_list/1).
:- regtype c_uint8_list(List) # "@var{List} is a list of @regtype{c_uint8/1}.".
c_uint8_list(List) :- list(List,c_uint8).

:- export(c_int_list/1).
:- regtype c_int_list(List) # "@var{List} is a list of @regtype{c_int/1}.".
c_int_list(List) :- list(List,c_int).

:- export(c_double_list/1).
:- regtype c_double_list(List) # "@var{List} is a list of @regtype{c_double/1}.".
c_double_list(List) :- list(List,c_double).

:- export(size_of/3).
:- prop size_of(Name,ListVar,SizeVar)
 # "For predicate @var{Name}, the size of the argument of type
    @regtype{c_uint8_list/1}, @var{ListVar}, is given by the argument of type
    integer @var{SizeVar}.".
size_of(_,_,_).

:- export(do_not_free/2).
:- prop do_not_free(Name,Var)
 # "For predicate @var{Name}, the C argument passed to (returned from) the
    foreign function will not be freed after calling the foreign function.".
do_not_free(_,_).

:- export(ttr/3).
:- prop ttr(Name,Var,TTr)
 # "For predicate @var{Name}, the C argument will be translated using @tt{TTr}
    as term translator.".
ttr(_,_,_).

:- export(returns/2).
:- prop returns(Name,Var)
 # "The result of the foreign function that implements the Prolog predicate
    @pred{Name} is unified with the Prolog variable @var{Var}. Cannot be
    used without @prop{foreign/1} or @prop{foreign/2}.".
returns(_,_).

:- export(needs_ciao_ctx/1).
:- prop needs_ciao_ctx(Name)
 # "The foreign function which implementes the predicate @pred{Name} needs 
    a @tt{ciao_ctx} as its first argument.".
needs_ciao_ctx(_).

:- export(foreign/1).
:- prop foreign(Name)
 # "The Prolog predicate @pred{Name} is implemented using the foreign
    function @tt{Name}.".
foreign(_).

:- export(foreign/2).
:- prop foreign(PrologName,ForeignName)
 # "The Prolog predicate @pred{PrologName} is implemented using the foreign
    function @tt{ForeignName}.".
foreign(_,_).

:- export(foreign_low/1).
:- prop foreign_low(Name)
 # "The Prolog predicate @pred{Name} is implemented using the function
    @tt{Name}.  The implementation is not a common C one, but it
    accesses directly the internal Ciao Prolog data structures and
    functions, and therefore no glue code is generated for it.".
foreign_low(_).

:- export(foreign_low/2).
:- prop foreign_low(PrologName,ForeignName)
 # "The Prolog predicate @pred{PrologName} is implemented using the
    function @tt{ForeignName}.  The same considerations as above
    example are to be applied.".
foreign_low(_,_).

:- pop_prolog_flag(multi_arity_warnings).

:- doc(bug, "The @tt{size_of/3} property has an empty definition").

:- doc(bug, "The range of integer types is missing.").
:- doc(bug, "@tt{ciao_fits_in_X()} is implemented only for a few ctypes.").
:- doc(bug, "@tt{X_list/1} type is implemented only for a few ctypes.").
:- doc(bug, "Creation if @tt{c_int64} in 32-bit build is broken (we need a MakeInteger32 and MakeInteger64)").

:- doc(bug, "The @tt{size_of/3} property has an empty definition").
