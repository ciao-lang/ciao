:- module(foreign_interface_properties, [
	int_list/1,
	double_list/1,
	byte_list/1,
	byte/1,
	null/1,
	address/1,
	any_term/1,
	foreign_low/1,
	foreign_low/2,
	size_of/3,
	foreign/1,
	foreign/2,
	returns/2,
	do_not_free/2,
	needs_state/1,
	ttr/3
	], [assertions, regtypes]).

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

:- regtype any_term(X) # "@var{X} is any term. The foreign interface
   passes it to C functions as a general term.".

any_term(_).

:- regtype address(Address) # "@var{Address} is a memory address.".

address('$address'(Address)) :-
	int(Address).

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

:- regtype byte(B) # "@var{Byte} is a byte.".

byte(B):- int(B).

 %% :- regtype in_byte_limits(Byte) # 
 %%    "The value @var{Byte} is within the limits of a byte.".
 %% 
 %% in_byte_limits(0).
 %% ...
 %% in_byte_limits(255).

:- regtype byte_list(List)
 # "@var{List} is a list of bytes.".

byte_list(List) :- list(List,byte).


:- regtype int_list(List)
 # "@var{List} is a list of integers.".

int_list(List) :- list(List,int).


:- regtype double_list(List)
 # "@var{List} is a list of numbers.".

double_list(List) :- list(List,num).


:- prop size_of(Name,ListVar,SizeVar)
 # "For predicate @var{Name}, the size of the argument of type
    @regtype{byte_list/1}, @var{ListVar}, is given by the argument of type
    integer @var{SizeVar}.".

size_of(_,_,_).


:- prop do_not_free(Name,Var)
 # "For predicate @var{Name}, the C argument passed to (returned from) the
    foreign function will not be freed after calling the foreign function.".

do_not_free(_,_).


:- prop ttr(Name,Var,TTr)
 # "For predicate @var{Name}, the C argument will be translated ussing @tt{TTr}
    as term translator.".

ttr(_,_,_).


:- prop returns(Name,Var)
 # "The result of the foreign function that implements the Prolog predicate
    @pred{Name} is unified with the Prolog variable @var{Var}. Cannot be
    used without @prop{foreign/1} or @prop{foreign/2}.".

returns(_,_).


:- prop needs_state(Name)
 # "The foreign function which implementes the predicate @pred{Name} needs 
    a @tt{ciao_state} as its first argument.".

needs_state(_).


:- prop foreign(Name)
 # "The Prolog predicate @pred{Name} is implemented using the foreign
    function @tt{Name}.".

foreign(_).


:- prop foreign(PrologName,ForeignName)
 # "The Prolog predicate @pred{PrologName} is implemented using the foreign
    function @tt{ForeignName}.".

foreign(_,_).


:- prop foreign_low(Name) # "The Prolog predicate @pred{Name} is
implemented using the function @tt{Name}.  The implementation is not
a common C one, but it accesses directly the internal Ciao Prolog data
structures and functions, and therefore no glue code is generated for
it.".

foreign_low(_).


:- prop foreign_low(PrologName,ForeignName) # "The Prolog predicate
@pred{PrologName} is implemented using the function
@tt{ForeignName}.  The same considerations as above example
are to be applied.".

foreign_low(_,_).


:- pop_prolog_flag(multi_arity_warnings).

:- doc(bug, "The @tt{size_of/3} property has an empty definition").

:- doc(bug, "The @tt{byte/1} property has an empty definition.
A possible right definition is commented.").
