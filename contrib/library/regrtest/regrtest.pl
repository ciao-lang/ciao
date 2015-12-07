:- module(regrtest,
          [run_regr_tests_in_module/2],
          [assertions, dcg, regtypes, fsyntax]).

:- doc(title, "Regression testing library").

:- doc(author, "Jose F. Morales").
:- doc(author, "Nataliia Stulova").

:- doc(module, "
This module implements bacis facilities for regression testing. For a
module with regression test queries in it (see @lib{regrtestdecls}),
this library provides a tests execution mechanism and test results
manipulations.
").

:- doc(usage, "
The library can be used from the Ciao shell by

@begin{verbatim}
?- use_module(library(regrtest)).

yes.
?- run_regr_tests_in_module('full/path/to/test/module.pl',Action).
@end{verbatim}

where action is one of:

@begin{itemize}
@item @tt{check} : run tests and temporarily save results in the
      auto-rewritable @tt{module.regr-out} file;
@item @tt{save} : save test results file in @tt{module.regr-out-saved}
      file;
@item @tt{briefcompare} : check whether current and saved test output files
      differ;
@item @tt{compare} : see the differences in the current and saved test
      output files in the diff format;
@item @tt{view_output} : view current test output.
@end{itemize}

").

:- doc(bug, "This module will eventually replace @lib{octesting}").

:- use_module(library(lists),              [reverse/2, append/3]).
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(library(compiler/c_itf),     [defines_module/2,
                                            compute_base_name/4]).
:- use_module(library(process),            [process_call/3]).
:- use_module(library(compiler),           [make_po/1]).
:- use_module(library(strings),            [get_line/1, write_string/1]).
:- use_module(library(read),               [read/2]).
:- use_module(library(file_utils),         [output_to_file/2]).
:- use_module(library(system),             [copy_file/2]).
:- use_module(library(system_extra),       [del_file_nofail/1]).

:- doc(section, "unittest-like code").

:- pred run_regr_tests_in_module(File,Option) : sourcename * test_option.
run_regr_tests_in_module(File, Option) :-
        compute_base_name(File,Base,_,_Dir),
        do_test(Option, Base).

:- doc(section, "octesting-based code").

% TODO: in practice the regression may have some identifier (like the
%       OS, architecture, commit id, etc., ; briefcompare and compare may
%       work with two identifiers; think how to add it.

:- regtype test_option(Opt) # "@var{Opt} is a testing option".

test_option := check | save | briefcompare | compare | view_output.

:- pred do_test(Option, Test) :: test_option * ground
        # "Apply the @var{Option} on test @var{Test}.".

do_test(check, Base) :-
	output_to_file(test_run(Base, _Data), ~test_outname(Base)),
	do_test(briefcompare, Base).
do_test(save, Base) :-
	Saved = ~test_outname_saved(Base),
	input('Really save? (yes/no) ', Answer),
	( Answer = "yes" ->
	    del_file_nofail(Saved),
	    copy_file(~test_outname(Base), Saved),
	    display('Saved'), nl
	; display('Not saved'), nl
	).
do_test(briefcompare, Base) :-
	( process_call(path(diff),
	               ['-q', ~test_outname(Base), ~test_outname_saved(Base)],
		       [status(0)]) ->
	    display('[OK]'), nl
	; display('[?]'), nl
	).
do_test(compare, Base) :-
	process_call(path(diff),
	             [~test_outname(Base), ~test_outname_saved(Base)],
		     [status(0)]).
do_test(view_output, Base) :-
	process_call(path(cat), [~test_outname(Base)], [status(0)]).

% TODO: test_file(Base) is the file that contains the test entries,
%       generalize so that tests can be optionally specified in a different
%       file (e.g., foo_tests.pl)
test_file(Base)          := X :- atom_concat(Base, '.pl'            , X).
test_outname(Base)       := X :- atom_concat(Base, '.regr-out'      , X).
test_outname_saved(Base) := X :- atom_concat(Base, '.regr-out-saved', X).

input(Question, Answer) :-
	display(Question), nl, flush_output,
	get_line(Answer).

:- multifile test_run/2.
test_run(Base,_) :- run_test_queries(Base).

%***********************************************************************
%                  running tests and processing output                 *
%***********************************************************************
run_test_queries(Base) :-
        make_po(~test_file(Base)),
        read_mod_queries(Base,Queries),
        Queries2 = [use_module(~test_file(Base))|Queries],
        process_call(path(ciaosh),
                     [],
                     [stdin(terms(Queries2)),
                      stderr(stdout),
                      stdout(string(Out))
                     ]),
        % TODO: this postprocessing of the output should be optional
        % (it makes sense only for treating Ciao errors)
        clean_output(OutClean,Out,[]),
        write_string(OutClean).

read_mod_queries(Base,Queries) :-
        atom_concat(Base, '_queries.pl', OutFile),
        open(OutFile, read, IO),
        read_mod_queries_(IO,Queries),
        close(IO).

read_mod_queries_(IStream,[Q|Queries]) :-
        read(IStream,Q),
        Q \== end_of_file,
        !,
        read_mod_queries_(IStream,Queries).
read_mod_queries_(_IStream,[]).

% clean_output(Out,In,Unparsed)
clean_output(Cs) -->
        { fsR(bundle_src(ciao),Root),atom_codes(Root,RootPath) },
        clean_output_(Cs, RootPath).

clean_output_("\n\n{In " || Cs    , RootPath) --> "{In ", !,
        clean_filename(Cs,Cs0,RootPath),
        clean_output_(Cs0,RootPath).
clean_output_(Cs                  , RootPath) --> "Ciao ", !,
        clean_ciaovers(Cs,Cs0),
        clean_output_(Cs0, RootPath).
clean_output_("expanded in " || Cs, RootPath) --> "expanded in ",!,
        clean_filename(Cs,Cs0,RootPath),
        clean_output_(Cs0,RootPath).
clean_output_("\n" || Cs          , RootPath) --> "\n\n\n",!,
        clean_output_(Cs,RootPath).
clean_output_([C|Cs]              , RootPath) --> [C], !,
        clean_output_(Cs, RootPath).
clean_output_([]                  ,_RootPath) --> [].

clean_filename(Cs, Cs0, RootPath) -->
        ( cs(RootPath) ->
            { Cs = "$CIAOROOT" || FileName }
        ; { Cs = FileName }
        ),
        clean_filename_(FileName, Cs0).

clean_filename_(".pl" || Cs, Cs)  --> ".pl", !.
clean_filename_([C|Cs]     , Cs0) --> [C],
        clean_filename_(Cs,Cs0).

cs([C|Cs]) --> [C], cs(Cs).
cs([]    ) --> [].

clean_ciaovers(Cs,Cs)  --> "]", !.
clean_ciaovers(Cs,Cs0) --> [_], clean_ciaovers(Cs,Cs0).
