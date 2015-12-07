:- module(unittest_utils, [assert_from_file/2],
	[assertions, hiord, unittestdecls]).

:- doc(title,"Testing support lib (generic)").

:- use_module(library(unittest/unittest_base), [read_data/2]).

:- doc(author, "Edison Mera").

:- meta_predicate assert_from_file(?, pred(1)).
:- pred assert_from_file/2 : sourcename * callable.
assert_from_file(File, AssertMethod) :-
	open(File, read, SI),
	repeat,
	(
	    read_data(SI, Term) ->
	    AssertMethod(Term),
	    fail
	;
	    !,
	    close(SI)
	).
