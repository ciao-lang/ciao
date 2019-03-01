:- module(_, [detect_c_headers/1], [assertions, dcg]).

:- doc(title, "C header detection").

:- doc(summary, "Detect if a C header can be found by the C compiler
   (under the include path)").

:- use_module(library(lists), [append/3]).
:- use_module(library(stream_utils), [string_to_file/2]).
:- use_module(library(foreign_compilation), [compiler_and_opts/2]).
:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(port_reify), [once_port_reify/2]).

:- pred detect_c_headers(+list(filename)) # "Succeeds if all the
	c header files in the argument are valid.".

detect_c_headers(Headers) :-
	headers_includes(Headers, Src, []),
	mktemp_in_tmp('headertmpXXXXXX', FileBase),
	atom_concat(FileBase, '.c', FileName),
	string_to_file(Src, FileName),
	compiler_and_opts(Compiler, Opts),
	append(Opts, ['-E', FileName], Args),
	once_port_reify(
            process_call(path(Compiler), Args,
                         [stderr(null), stdout(null), status(0)]), Port),
	del_file_nofail(FileName),
	Port = success.

headers_includes([]) --> [].
headers_includes([Header|Headers]) -->
	"#include <", emit_atom(Header), ">\n",
	headers_includes(Headers).

emit_atom(X, S, S0) :-
	atom_codes(X, Cs),
	append(Cs, S0, S).

