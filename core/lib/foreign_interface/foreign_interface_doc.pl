:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Foreign Language Interface").

:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel Carro").
:- doc(copyright, "The CLIP Group, 2001-2002").

:- doc(summary, "The foreign interface module provides predicates
for building automatically the shared object necessary for accessing C
functions as Ciao predicates. Regular C functions do not have
their data in the internal format required by Ciao, and thus an
intermediate translation is necessary. Besides, run-time errors (such
as wrong instantiation states) must be handled and, finally, C code
must be compiled into a dynamically loadable object code form, which
is OS-dependent.").

:- doc(module, "Ciao includes a high-level, flexible way to
interface C and Prolog, based on the use of assertions to declare what
are the expected types and modes of the arguments of a Prolog
predicate, and which C files contain the corresponding code.  To this
end, the user provides:

@begin{itemize}
@item A set of C files, or a precompiled shared library,
@item A Ciao module defining whith predicates are  implemented
  in the C files and the types and modes of their arguments,  and   
@item an (optional) set of flags required for the compilation of the
files.
@end{itemize}

The Ciao compiler analyzes the Prolog code written by the user
and gathers this information in order to generate automatically C
\"glue\" code implementing the data translation between Prolog and C,
and to compile the C code into dynamically loadable C object files,
which are linked automatically when needed.

@section{Declaration of Foreign Sources}

The compilation and dynamic linking of foreign sources is controled by
the following declarations:
@begin{itemize}

@item @prop{use_foreign_source/1} specifies the foreign C (or
C++) source file(s).

@item @prop{use_foreign_library/1} specifies the external library(es)
to be linked with the foreign sources.

@item @prop{use_compiler/1} specifies the compiler to use.

@item @prop{extra_compiler_opts/1} specifies the additional compiler options. 

@item @prop{use_linker/1}  specifies the linker to use.

@item @prop{extra_linker_opts/1} specifies the additional linker options. 
@end{itemize}

More details about those options can be found in @ref{Foreign Language
Interface Properties}.

The compilation of the foreign sources together with the glue code
is decomposed into the two phases:

@begin{itemize}
@item @bf{The Compilation phase}:
Each C (or C++) file @tt{<SrcFile>} is compiled into an object file
@tt{<ObjFile>} by a command of the form:
@begin{verbatim}
$ <Compiler> <CompilerOpts> -c <ExtraCompilerOptions> -o <ObjectFile> <SrcFile>
@end{verbatim}
where 
@tt{<Compiler>} is the compiler command, 
@tt{<CompilerOpts>} are some Ciao specific compiler options, 
and @tt{<ExtraCompilerOptions>} are the additional compiler options
specified by the declaration(s) @prop{extra_compiler_opts/1}.


@item @bf{The Linking phase}: 
All the object files @tt{<ObjectFiles>} obtained by the compilation
phase are linked together with the external libray(es) into a dynamic library
@tt{<SOLib>} by a command of the form:
@begin{verbatim} 
$ <Linker> <CompilerOpts> -o <SOLib> <ObjectFiles> <ExternalLibs> <ExtraLinkerOptions>
@end{verbatim}
where 
@tt{<Linker>} is the linker command, 
@tt{<LinkerOpts>} are some Ciao specific linker options,
@tt{<ExternalLibs>} are the foreign libraries in order they are declared,
and @tt{<ExtraLinkerOptions>} are the additional linker options 
specified by the declaration(s) @prop{extra_linker_opts/1}.  


@end{itemize}

Note that for the linking phase the order of libraries may be
important.  See
@href{https://gcc.gnu.org/onlinedocs/gcc/Link-Options.html} or
@href{https://sourceware.org/binutils/docs-2.24/ld/Options.html#Options}
for more details.

Once the foreign sources are compiled and linked together, the
resulting library is dynamically loading using semantics of the
RTLD_LOCAL flag of POSIX dlopen() function (See dlopen man page for
additional details).  In other word the symbols defined in this
dynamic library are not made available to resolve references in
subsequently foreign loaded libraries. In particular the foreign
sources cannot depend on a previously loaded foreign sources.

@section{Declaration of Types}

Each predicate implemented as a foreign C function must have
accompanying declarations in the Ciao associated file stating
the types and modes of the C function.  A sample declaration for
@tt{prolog_predicate} which is implemented as
@tt{foreign_function_name} is:


   @begin{verbatim} 
     :- true pred prolog_predicate(m1(Arg1), ... mN(ArgN)) :: 
                  type1 * ... * typeN + 
                  (foreign(foreign_function_name), returns(ArgR)).
   @end{verbatim}

@noindent
where @tt{m1}, ..., @tt{mN} and @tt{type1}, ..., @tt{typeN} are
respectively the modes and types of the arguments.
@tt{foreign_function_name} is the name of the C function
implementing @pred{prolog_predicate/N}, and the result of this
function is unified with @tt{ArgR}, which must be one of @tt{Arg1}
... @tt{ArgN}.

This notation can be simplified in several ways.  If the name of the
foreign function is the same as the name of the Ciao predicate,
@tt{foreign(foreign_function_name)} can be replaced by
@tt{foreign}. @tt{returns(ArgR)} specifies that the result of the
function corresponds to the @tt{ArgR} argument of the Ciao
predicate. If the foreign function does not return anything (or if its
value is ignored), then @tt{returns(ArgR)} must be removed. Note that
@tt{returns} cannot be used without @tt{foreign}.  A simplified,
minimal form is thus:

   @begin{verbatim} 
     :- true pred prolog_predicate(m1(Arg1), ... mN(ArgN)) :: 
                  type1 * ... * typeN + foreign.
   @end{verbatim}

@section{Equivalence between Ciao and C types}

The automatic translation between Ciao and C types is defined for some
simple but useful Prolog types (like atoms and strings) and for a
collection of reserved Prolog types (@concept{ctype}s) that reflect C
types.

The names (and meaning) of the types known for performing that
translation are to be found in @ref{Foreign Language Interface
Properties}; they are also summarized below (Prolog types are on the
left, and the corresponding C types on the right):

   @begin{itemize}

   @item @tt{c_short}, @tt{c_ushort}, @tt{c_int}, @tt{c_uint},
     @tt{c_long}, @tt{c_ulong}, @tt{c_uintptr}, @tt{c_size} <->
     @tt{short}, @tt{unsigned short}, @tt{int}, @tt{unsigned int},
     @tt{long}, @tt{unsigned long}, @tt{uintptr_t}, @tt{size_t}

     (be aware the ranges of all types depends on the C data model, which differs in 32 and 64-bits).

   @item @tt{c_int8}, @tt{c_uint8}, @tt{c_int16}, @tt{c_uint16},
     @tt{c_int32}, @tt{c_uint32}, @tt{c_int64}, @tt{c_uint64} <->
     @tt{int8_t}, @tt{uint8_t}, @tt{int16_t}, @tt{uint16_t},
     @tt{int32_t}, @tt{uint32_t}, @tt{int64_t}, @tt{uint64_t}

   @item @tt{atm} <-> @tt{char *} (with trailing zero)
   @item @tt{string} <-> @tt{char *} (with trailing zero)

   @item @tt{X_list} <-> @tt{T *} (an array of some ctype @tt{X} with C type @tt{T}, with associated length)
   @item @tt{address} <-> @tt{void *}
   @end{itemize}

Strings, atoms, and lists of bytes are passed to (and from) C as
dynamically (@tt{ciao_malloc}) created arrays of characters
(bytes). Those arrays are freed by Ciao upon return of the
foreign function unless the property @prop{do_not_free/2} is specified
(see examples below).  This caters for the case in which the C files
save in a private state (either by themselves, or by a library
function being called by them) the values passed on from Prolog.  The
type @regtype{X_list/1} requires an additional property,
@prop{size_of/2}, to indicate which argument represents its size.

Empty lists of bytes and integers are converted into C @tt{NULL}
pointers, and vice versa.  Empty strings (@tt{[]}) and null atoms
(\'\') are converted into zero-length, zero-ended C strings
(@em{\"\"}).  C @tt{NULL} strings and empty arrays (i.e., arrays
with zero length) are transformed into the empty list or the null atom
(@tt{''}).

Most of the work is performed by the predicates in the @ref{Foreign
Language Interface Builder}, which can be called explicitly by the
user.  Doing that is not usually needed, since the Ciao
Compiler takes care of building glue code files an of compiling and
linking whatever is necessary.

The translation to be performed is solely defined by the types of the
arguments in the Ciao module (i.e., no inspection of the corresponding
C file is done).

@begin{alert}
An incorrect selection of types may generate wrong C prototypes. Those
problems are silently ignored during compilation.
@end{alert}

@section{Equivalence between Ciao and C modes}

The (prefix) @tt{+/1} ISO mode (or, equivalently, the in/1 mode)
states that the corresponding Prolog argument is ground at the time of
the call, and therefore it is an input argument in the C part; this
groundness is automatically checked upon entry.  The (prefix) @tt{-/1}
ISO mode (or, equivalently, the go/1 mode) states that Prolog expects
the C side to generate a (ground) value for that argument.  Arguments
with output mode should appear in C functions as pointers to the
corresponding base type (as it is usual with C), i.e., an argument
which is an integer generated by the C file, declared as

@begin{verbatim}
:- true pred get_int(go(ThisInt)) :: c_int + foreign
@end{verbatim}

or as

@begin{verbatim}
:- true pred get_int(-ThisInt) :: c_int + foreign
@end{verbatim}

should appear in the C code as

@begin{verbatim}
void get_int(int *thisint)
@{
        ....
@}
@end{verbatim}

Note the type of the (single) argument of the function.  Besides, the
return value of a function can always be used as an output argument,
just by specifying to which Prolog arguments it corresponds, using the
@tt{foreing/1} property.  The examples below illustrate this point,
and the use of several assertions to guide the compilation.

@section{Custom access to Prolog from C}

Automatic type conversions does not cover all the possible cases.  
When the automatic type conversion is not enough (or if the user, for
any reason, does not want to go through the automatic conversion), it
is possible to instruct Ciao not to make implicit type
conversion.  The strategy in that case is to pass the relevant
argument(s) with a special type (a @tt{ciao_term}) which can represent
any term which can be built in Prolog.  Operations to construct,
traverse, and test this data abstraction from C are provided.  The
prototypes of these operations are placed on the
@tt{\"ciao_prolog.h\"} file, under the @tt{include} subdirectory of
the installation directory (the Ciao compiler knowns where it
has been installed, and gives the C compiler the appropriate flags).
This @em{non direct correspondence} mode is activated whenever a Ciao
Prolog type unknown to the foreign interface (i.e., none of these in
@ref{Foreign Language Interface Properties}) or the type @tt{any_term}
(which is explicitly recognised by the foreign language interface) is
found.  The latter is preferred, as it is much more informative, and
external tools, as the the @concept{CiaoPP} preprocessor, can take
advantage of them.

@subsection{Term construction}

All term construction primitives return an argument of type
@tt{ciao_term}, which is the result of constructing a term.  All Ciao
Prolog terms can be built using the interface operations
@tt{ciao_var()}, @tt{ciao_structure()}, @tt{ciao_X()}, etc.  There
are, however, variants and specialized versions of these operations
which can be freely intermixed.  Using one version or another is a
matter of taste and convenience.  We list below the prototypes of the
primitives in order of complexity.

Throughout this section, @bf{true}, when referred to a boolean value,
correspond to the integer value @tt{1}, and @bf{false} correspond to
the integer value @tt{0}, as is customary in C boolean expressions.
These values also available as the (predefined) constants
@tt{ciao_true} and @tt{ciao_false}, both of type @tt{ciao_bool}.

@begin{itemize} 
@item @tt{ciao_term ciao_var();}

  Returns a fresh, unbound variable.

@item @tt{ciao_term ciao_mk_X(T i);}

  Creates a term of ctype @tt{X} from the value of @tt{i}, of the
  corresponding C type @tt{T}.

@item @tt{ciao_term ciao_put_number_chars(char *number_string);}

  It converts @tt{number_string} (which must a string representing a
  syntactically valid number) into a @tt{ciao_term}.

@item @tt{ciao_term ciao_atom(char *name);}

  Creates an atom whose printable name is given as a C string.

@item @tt{ciao_term ciao_structure_a(char *name, int arity, ciao_term *args);}

  Creates a structure with name @tt{name} (i.e., the functor name ),
  arity @tt{arity} and the components of the array @tt{args} as arguments:
  @tt{args[0]} will be the first argument, @tt{args[1]} the second,
  and so on.  The @tt{args} array itself is not needed after the term is
  created, and can thus be a variable local to a procedure.  An atom
  can be represented as a 0-arity structure (with
  @tt{ciao_structure(name, 0)}), and a list cell can be constructed
  using the @tt{'.'/2} structure name.  The @tt{_a} suffix stands for
  @em{array}.

@item @tt{ciao_term ciao_structure(char *name, int arity, ...);}

  Similar to ciao_structure_a, but the C arguments after the arity are
  used to fill in the arguments of the structure.

@item @tt{ciao_term ciao_list(ciao_term head, ciao_term tail);}

  Creates a list from a @tt{head} and a @tt{tail}. It is equivalent to
  @tt{ciao_structure(\".\", 2, head, tail)}.

@item @tt{ciao_term ciao_empty_list();}

 Creates an empty list. It is equivalent to @tt{ciao_atom(\"[]\")}.

@item @tt{ciao_term ciao_listn_a(int len, ciao_term *args);}

  Creates a list with 'len' elements from the array @tt{args}.  The
  @em{nth} element of the list (starting at 1) is @tt{args[n-1]}
  (starting at zero).

@item @tt{ciao_term ciao_listn(size_t length, ...);}

  Like @tt{ciao_listn_a()}, but the list elements appear explicitly as
  arguments in the call.

@item @tt{ciao_term ciao_dlist_a(size_t len, ciao_term *args, ciao_term base);}

  Like @tt{ciao_listn_a}, but a difference list is created. @tt{base}
  will be used as the tail of the list, instead of the empty list.

@item @tt{ciao_term ciao_dlist(size_t length, ...);}

  Similar to @tt{ciao_dlist_a()} with a variable number of arguments.
  The last one is the tail of the list.

@item @tt{ciao_term ciao_copy_term(ciao_term src_term);}

  Returns a new copy of the @tt{term}, with fresh variables (as
  @tt{copy_term/2} does).

@end{itemize} 


@subsection{Testing the Type of a Term}

A @tt{ciao_term} can contain @em{any} Prolog term, and its
implementation is opaque to the C code.  Therefore the only way to
know reliably what data is passed on is using explicit functions to
test term types.  Below, @tt{ciao_bool} is a type defined in
@tt{\"ciao_prolog.h\"} which can take the values 1 (for @bf{true}) and
0 (for @bf{false}).

@begin{itemize} 
@item @tt{ciao_bool ciao_is_variable(ciao_term term);}

  Returns true if @tt{term} is currently an uninstantiated variable.

@item @tt{ciao_bool ciao_is_number(ciao_term term);}

  Returns true if @tt{term} is an integer (of any length) or a floating
  point number.

@item @tt{ciao_bool ciao_is_integer(ciao_term term);}

  Returns true if @tt{term} is instantiated to an integer.

@item @tt{ciao_bool ciao_fits_in_X(ciao_term term);}

  Returns true if @tt{term} is instantiated to a value representable
  by ctype @tt{X} (e.g., 10000 is not representable by @tt{c_int8} but
  it is for @tt{c_int32}).

@item @tt{ciao_bool ciao_is_atom(ciao_term atom);}

  Returns true if @tt{term} is an atom. 

@item @tt{ciao_bool ciao_is_list(ciao_term term);}

  Returns true if @tt{term} is a list (actually, a @tt{cons} cell).

@item @tt{ciao_bool ciao_is_empty_list(ciao_term term);}

  Returns true if @tt{term} is the atom which represents the empty
  list (i.e., @tt{[]}).

@item @tt{ciao_bool ciao_is_structure(ciao_term term);}

  Returns true if @tt{term} is a structure of any arity.  This
  includes atoms (i.e., structures of arity zero) and lists, but
  excludes variables and numbers.

@end{itemize} 


@subsection{Term navigation}

The functions below can be used to recover the value of a
@tt{ciao_term} into C variables, or to inspect Prolog structures.

@begin{itemize} 

@item @tt{int ciao_get_X(ciao_term term); }

  Converts @tt{term} to the corresponding C type for the ctype
  @tt{X}. For integer types, @tt{ciao_is_integer(term)} must hold.
  For floating point, @tt{ciao_is_number(term)} must hold.

@item @tt{char *ciao_get_number_chars(ciao_term term);}

  It converts @tt{ciao_term} (which must be instantiated to a number)
  into a C string representing the number in the current radix.  The
  string returned is a copy, which must (eventually) be explicitly
  deallocated by the user C code using the operation @tt{ciao_free()}

@item @tt{char *ciao_atom_name(ciao_term atom);}

  Returns the name of the atom.  The returned string @em{is the one
  internally used by Ciao}, and should not be deallocated, changed or
  altered in any form. The advantage of using it is that it is fast,
  as no data copying is needed.

@item @tt{char *ciao_atom_name_dup(ciao_term atom);}

  Obtains a @bf{copy} of the name of the atom.  The string can be
  modified, and the programmer has the responsibility of deallocating
  it after being used.  Due to the copy, it is slower than calling
  @tt{char *ciao_atom_name()}.

@item @tt{ciao_term ciao_list_head(ciao_term term);}

  Extracts the head of the list @tt{term}. Requires @tt{term} to be a
  list.

@item @tt{ciao_term ciao_list_tail(ciao_term term);}

  Extracts the tail of the list @tt{term}. Requires @tt{term} to be a
  list.

@item @tt{char *ciao_structure_name(ciao_term term);} 

  Extracts the name of the structure @tt{term}.  Requires @tt{term} to
  be a structure.

@item @tt{int ciao_structure_arity(ciao_term term);}

  Extracts the arity of the structure @tt{term}.

  Requires @tt{term} to be a structure.

@item @tt{ciao_term ciao_structure_arg(ciao_term term, int n);}

  Extracts the @em{nth} argument of the structure @tt{term}.  It
  behaves like @tt{arg/3}, so the first argument has index 1. Requires
  @tt{term} to be a structure.

@end{itemize} 

@subsection{Testing for Equality and Performing Unification}

Variables of type @tt{ciao_term} cannot be tested directly for
equality: they are (currently) implemented as a sort of pointers which
may be aliased (two different pointers may refer to the same object).
The interface provides helper functions for testing term equality and
to perform unification of terms.

@begin{itemize}
@item @tt{ciao_bool ciao_unify(ciao_term x, ciao_term y);}

  Performs the unification of the terms @tt{x} and @tt{y}, and returns
  true if the unification was successful.  This is equivalent to
  calling the (infix) Prolog predicate @tt{=/2}.  The bindings are
  trailed and undone on backtracking.

@item @tt{ciao_bool ciao_equal(ciao_term x, ciao_term y);}

  Performs equality testing of terms, and returns true if the test was
  successful.  This is equivalent to calling the (infix) Prolog
  predicate @tt{==/2}.  Equality testing does not modify the terms
  compared.

@end{itemize} 


@subsection{Raising Exceptions}

The following functions offers a way of throwing @concept{exceptions}
from C that can be caught in Prolog with @tt{catch/3}.  The term that
reaches Prolog is exactly the same which was thrown by C.  The
execution flow is broken at the point where
@tt{ciao_raise_exception()} is executed, and it returns to Prolog.

@begin{itemize} 

@item @tt{void ciao_raise_exception(ciao_term ball);}

 Raises an exception an throws the term @tt{ball}.

@end{itemize} 


@subsection{Creating and disposing of memory chunks}

Memory to be used solely by the user C code can be reserved/disposed
of using, e.g., the well-known @tt{malloc()}/@tt{free()} functions (or
whatever other functions the user may have available).  However,
memory explicitly allocated by Ciao and passed to C code, or
allocated by C code and passed on to Ciao (and subject to
garbage collection by it) should be allotted and freed (when
necessary) by using the functions:

@begin{itemize} 

@item @tt{void *ciao_malloc(int size);}

@item @tt{void ciao_free(void *pointer);}

@end{itemize} 

@noindent 
whose behavior is similar to @tt{malloc()}/@tt{free()}, but
which will cooordinate properly with Ciao's internal memory
management.


@subsection{Calling Prolog from C}

It is also possible to make arbitraty calls to Prolog predicates from
C.  There are two basic ways of make a query, depending on whether
only one solution is needed (or if the predicate to be called is known
to generate only one solution), or if several solutions are required.

When only one solution is needed @tt{ciao_commit_call} obtains it (the
solution obtained will obviously be the first one) and discards the
resources used for finding it:

@begin{itemize}

@item @tt{ciao_bool ciao_commit_call(char *name, int arity, ...);}

  Makes a call to a predicate and returns true or false depending on
  whether the query has succedeed or not.  In case of success, the
  (possibly) instantiated variables are reachable from C.

@item @tt{ciao_bool ciao_commit_call_term(ciao_term goal);}

  Like @tt{ciao_commit_call()} but uses the previously built term
  @tt{goal} as goal.

@end{itemize}

If more than one solution is needed, it is necessary to use the
@tt{ciao_query} operations.  A consult begins with a
@tt{ciao_query_begin} which returns a @tt{ciao_query} object.
Whenever an additional solution is required, the @tt{ciao_query_next}
function can be called. The query ends by calling @tt{ciao_query_end}
and all pending search branches are pruned.

@begin{itemize} 

@item @tt{ciao_query *ciao_query_begin(char *name, int arity, ...);}

  The predicate with the given name, arity and arguments (similar to
  the @tt{ciao_structure()} operation) is transformed into a
  @tt{ciao_query} object which can be used to make the actual query.

@item @tt{ciao_query *ciao_query_begin_term(ciao_term goal);}

  Like ciao_query_begin but using the term @tt{goal} instead.

@item @tt{ciao_bool ciao_query_ok(ciao_query *query);}

  Determines whether the query may have pending solutions.  A false
  return value means that there are no more solutions; a true return
  value means that there are more possible solutions.

@item @tt{void ciao_query_next(ciao_query *query);}

  Ask for a new solution.

@item @tt{void ciao_query_end(ciao_query *query);}

  Ends the query and frees the used resources.

@end{itemize} 

@section{Examples}

@subsection{Mathematical functions}

In this example, the standard mathematical library is accessed to
provide the @em{sin}, @em{cos}, and @em{fabs} functions.  Note that the
library is specified simply as

@begin{verbatim}
:- use_foreign_library([m]).
@end{verbatim}

The foreign interface adds the @tt{-lm} at compile time.  Note also
how some additional options are added to optimize the compiled code
(only glue code, in this case) and mathematics (only in the case of
Linux in an Intel processor).

@bf{File} @em{math.pl}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/math/math.pl}
@end{verbatim}

@subsection{Addresses and C pointers}

The @tt{address} type designates any pointer, and provides a means to
deal with C pointers in Prolog without interpreting them whatsoever.
The C source file which implements the operations accessed from Prolog
is declared with the

@begin{verbatim}
:- use_foreign_source(objects_c).
@end{verbatim}

directive.

@bf{File} @em{objects.pl}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/addresses/objects.pl}
@end{verbatim}

@bf{File} @em{objects_c.c}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/addresses/objects_c.c}
@end{verbatim}

@subsection{Lists of bytes and arrays}

A list of bytes (c.f., a list of ints) corresponds to a byte array in
C.  The length of the array is associated to that of the list using
the property @tt{size_of/2}.  The returned array @bf{is freed by Ciao
Prolog} upon its recepction, unless the @tt{do_not_free/1} property is
specified (see later).  Conversely, a list of natural numbers in the
range 0 to 255 can be passed to C as an array.

@bf{File} @em{byte_lists.pl}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/byte_lists/byte_lists.pl}
@end{verbatim}

@bf{File} @em{bytes_op.c}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/byte_lists/bytes_op.c}
@end{verbatim}

@subsection{Lists of integers}

@bf{File} @em{int_lists.pl}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/int_lists/int_lists.pl}
@end{verbatim}

@bf{File} @em{ints_op.c}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/int_lists/ints_op.c}
@end{verbatim}

@subsection{Strings and atoms}

A C string can be seen as an array whose end is denoted by the
trailing zero, and therefore stating its length is not needed.  Two
translations are possible into Ciao: as a Prolog string (list
of bytes, with no trailing zero) and as an atom.  These are selected
automatically just by choosing the corresponding type (look at the
examples below).

Note how the @tt{do_not_free/1} property is specified in the
@pred{a_string/1} predicate: the string returned by C is static, and
therefore it should not be freed by Prolog.


@bf{File} @em{strings_and_atoms.pl}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/strings_and_atoms/strings_and_atoms.pl}
@end{verbatim}

@bf{File} @em{str_op.c}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/strings_and_atoms/str_op.c}
@end{verbatim}

@subsection{Arbitrary Terms}

This example shows how data Prolog can be passed untouched to C code,
and how it can be manipulated there.


@bf{File} @em{any_term.pl}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/any_term/any_term.pl}
@end{verbatim}

@bf{File} @em{any_term_c.c}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/any_term/any_term_c.c}
@end{verbatim}

@subsection{Exceptions}

The following example defines a predicate in C that converts a list of
codes into a number using @tt{strtol()}. If this conversion fails, then
a exception is raised.

	
@bf{File} @em{exceptions_example.pl}:
	
@begin{verbatim}
@includeverbatim{foreign_interface/examples/exceptions/exceptions_example.pl}
@end{verbatim}

@bf{File} @em{exceptions_c.c}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/exceptions/exceptions_c.c}
@end{verbatim}

@subsection{Testing number types and using unbound length integers}

Unbound length integers (and, in general, any number) can be converted
to/from @tt{ciao_terms} by using strings.  The following examples show
two possibilities: one which tries to be as smart as possible
(checking whether numbers fit into a machine int or not), and being
lazy and simpler -and probably slower.

@bf{File} @em{bigints.pl}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/bignums/bigints.pl}
@end{verbatim}

@bf{File} @em{bigints_c.c}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/bignums/bigints_c.c}
@end{verbatim}

" 

% TODO: Document C functions from Prolog?
 %% :- doc(doinclude,"ciao_term ciao_var();").
 %% :- doc("ciao_term ciao_var();", "Returns a fresh, unbound variable.").

|| "
@subsection{Interfacing with C++}


Ciao code can be interfaced easily with C++ using this interface. The
basic idea is to write C functions (functions prefixed by 'extern
\"C\"') within the C++ code to make the bridge between calls from
Ciao to C++. Then, C++ objects can be casted as addresses.  Because
the foreign interface assumes that the foreign source is classical C,
C++ source files should be declared with their extension.


@bf{File} @em{cc_stack.pl}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/c++/cc_stack.pl}
@end{verbatim}

@bf{File} @em{cc_stack.cc}:

@begin{verbatim}
@includeverbatim{foreign_interface/examples/c++/cc_stack.cc}
@end{verbatim}
").
