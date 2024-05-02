:- module(_, [], [hiord, datafacts]).

:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(engine(io_basic), [display/1, nl/0]).
:- use_module(library(write), [print/1]).
:- use_module(library(system), [copy_file/2]).
:- use_module(engine(stream_basic), [flush_output/0]).
:- use_module(library(stream_utils), [get_line/1, string_to_file/2]).
:- use_module(library(process), [process_call/3]).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(system), [delete_file/1, get_tmp_dir/1]).
:- use_module(library(assertions/assrt_lib), [assertion_body/7]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(write), [numbervars/3]).

:- use_module(library(unittest/unittest_db)).

% ---------------------------------------------------------------------------

% TODO: copied from octesting, merge

:- export(save_output/1).
save_output(Modules) :-
    input('Really save output for all modules? (yes/no) ', Answer),
    ( Answer = "yes" ->
        save_output_(Modules),
        display('Saved'), nl
    ; display('Not saved'), nl
    ).
% TODO: options for asking for comfirmation once, once per module,
% once per test. Ask again if none of those

save_output_([]).
save_output_([M|Ms]) :-
    save_output__(M),
    save_output_(Ms).

save_output__(M) :-
    file_test_output(M,saved,SavedOutput),
    file_test_output(M,new,Output),
    del_file_nofail(SavedOutput),
    copy_file(Output, SavedOutput),
    file_test_input(M,saved,SavedInput),
    file_test_input(M,new,Input),
    del_file_nofail(SavedInput),
    copy_file(Input, SavedInput).

% ---------------------------------------------------------------------------

% TODO: warn and fail if .testout is incomplete becasue unittest was
% only run for a subset of the tests (using filter options). It is not
% trivial to merge .testout with .testoutsaved due to the fact that
% test ids depend on locators. The current solution we have for
% comparing them for regrassion could be used here, but it is a kludge
% right now

input(Question, Answer) :-
    display(Question), nl, flush_output,
    get_line(Answer).

% ---------------------------------------------------------------------------
%! # brief compare

:- export(brief_compare/2).
brief_compare([], 0).
brief_compare([M|Ms], ReturnStatus) :-
    brief_compare_(M, MStatus),
    ( MStatus=1 -> ReturnStatus=1 ; ReturnStatus=ReturnStatus0 ),
    brief_compare(Ms, ReturnStatus0).

brief_compare_(M, 0) :-
    compare_tests(M, brief), !, % there are no differences
    display('Module '), display(M), display(': OK.'), nl.
brief_compare_(M, 1) :- % there are differences
    display('Module '), display(M), display(': There are differences.'), nl.

% ---------------------------------------------------------------------------
%! # compare

:- export(compare/1).
compare([]).
compare([M|Ms]) :-
    compare_(M),
    compare(Ms).

compare_(M) :-
    display('Differences in module '), display(M), display(':'), nl,
    retractall_fact(there_were_differences),
    compare_tests(M, show), % TODO: better messages
    (there_were_differences -> true ; display('None'), nl).

:- data there_were_differences/0.

there_are_differences :-
    ( there_were_differences -> true ; assertz_fact(there_were_differences) ).

% ---------------------------------------------------------------------------
%! # compare tests

compare_tests(Module, Mode) :-
    get_saved_db(Module),
    get_new_db(Module),
    compare_tests_(Module, Mode).

get_saved_db(Module) :-
    cleanup_test_db,
    load_test_input(Module, saved),
    load_test_output(Module, saved),
    cleanup_saved_test_results,
    test_results_db_to_saved_test_results_db.

get_new_db(Module) :-
    cleanup_test_db,
    load_test_input(Module, new),
    load_test_output(Module, new).

difference(brief,_) :- fail.
difference(show,Diff) :-
    message(user, Diff),
    there_are_differences.

compare_tests_(Module, Mode) :-
    retract_fact(test_db(NewTestId, Module, F, A, _, _, Body, loc(_, LB, LE))), !,
    assertion_body(_,_,_,_,_,Comment,Body),
    ( saved_test_db(SavedTestId, Module, F, A, _, _, Body2, _),
      assertion_body(_,_,_,_,_,Comment2,Body2),
      same_test(Comment, Body, Comment2, Body2),
      retract_fact(saved_test_db(SavedTestId, Module, _, _, _, _, _, _)) ->
        test_description(F,A,Comment,LB,LE,TestMsg),
        compare_test_results(NewTestId, SavedTestId, Mode, TestMsg)
    ;
        test_description(F,A,Comment,LB,LE,TestMsg),
        difference(Mode, ['\t', 'New test: ', [](TestMsg), '.\n'])
    ),
    compare_tests_(Module, Mode).
compare_tests_(Module, Mode) :-
    retract_fact(saved_test_db(_TestId, Module, F, A, _, _, Body, loc(_, LB, LE))), !,
    assertion_body(_,_,_,_,_,Comment,Body),
    test_description(F,A,Comment,LB,LE,TestMsg),
    difference(Mode, ['\t', 'Missing test: ', [](TestMsg), '.\n']),
    compare_tests_(Module, Mode).
compare_tests_(_,_).
% TODO: allow locators to vary, make test id depend on test content
% and not locator (e.g., unique id per predicate, plus optional
% user-provided id or number of assertion of that pred)

compare_test_results(NewTestId, SavedTestId, Mode, TestMsg) :-
    retract_fact(test_output_db(NewTestId, t_res(Result, Stdout, Stderr))), !,
    ( retract_fact(saved_test_output_db(SavedTestId, t_res(SavedResult, SavedStdout, SavedStderr))) ->
        compare_test_result(SavedResult, Result, Mode, TestMsg),
        compare_test_stdouterr(Stdout, Stderr, SavedStdout, SavedStderr, Mode, TestMsg)
    ;
        result_message(Result, ResultText),
        difference(Mode, ['\t', 'New result in test ', [](TestMsg), ': ', ResultText, '.\n'])
    ),
    compare_test_results(NewTestId, SavedTestId, Mode, TestMsg).
compare_test_results(NewTestId, SavedTestId, Mode, TestMsg) :-
    retract_fact(saved_test_output_db(SavedTestId, t_res(Result, _, _))), !,
    result_message(Result, ResultText),
    difference(Mode, ['\t', 'Missing result in test ', [](TestMsg), ': ', ResultText, '.\n']),
    compare_test_results(NewTestId, SavedTestId, Mode, TestMsg).
compare_test_results(_,_,_,_).

compare_test_result(X,X,_,_) :- !.
compare_test_result(Result1,Result2,Mode,TestMsg) :-
    result_message(Result1, Result1Text),
    result_message(Result2, Result2Text),
    difference(Mode, ['\t', 'Different results in test ', [](TestMsg), ': ', Result1Text, ' and ', Result2Text, '.\n']).

result_message(Result, Result) :-  % TODO: result description
    numbervars(Result, 0, _).

compare_test_stdouterr(Stdout, Stderr, SavedStdout, SavedStderr, brief,_) :- !,
    Stdout = SavedStdout,
    Stderr = SavedStderr.
compare_test_stdouterr(Stdout, Stderr, SavedStdout, SavedStderr, show, TestMsg) :- !,
    % comparing output
    ( Stdout=SavedStdout -> true
    ; there_are_differences,
      message(user, ['Stdout differences in test ', [](TestMsg), ':', '\n']),
      show_diff(SavedStdout, Stdout)
    ),
    % comparing error
    ( Stderr=SavedStderr -> true
    ; there_are_differences,
      message(user, ['Stderr differences in test ', [](TestMsg), ':', '\n']),
      show_diff(SavedStderr, Stderr)
    ).

show_diff(SavedOutput, Output) :-
    string_to_tmp_file(SavedOutput, 'saved', SavedOutputFile),
    string_to_tmp_file(Output, 'new', OutputFile),
    process_call(path(diff),
                 ['-U', '2', SavedOutputFile, '--label', 'saved', OutputFile, '--label', 'new'],
                 [status(_)]),
    delete_file(OutputFile),
    delete_file(SavedOutputFile).

% ---------------------------------------------------------------------------

:- data saved_test_output_db/2.
:- data saved_test_db/8.

cleanup_saved_test_results :-
    retractall_fact(saved_test_output_db(_,_)),
    retractall_fact(saved_test_db(_,_,_,_,_,_,_,_)).

test_results_db_to_saved_test_results_db :-
    test_db(A, B, C, D, E, F, G, H),
    assertz_fact(saved_test_db(A, B, C, D, E, F, G, H)),
    fail.
test_results_db_to_saved_test_results_db :-
    test_output_db(A,B),
    assertz_fact(saved_test_output_db(A,B)),
    fail.
test_results_db_to_saved_test_results_db.

% ---------------------------------------------------------------------------

string_to_tmp_file(Str, Name, TmpFile) :-
    get_tmp_dir(TmpDir),
    path_concat(TmpDir, Name, TmpFile),
    string_to_file(Str, TmpFile).

:- export(test_description/6).
test_description(F,A,Comment,LB,LE,TestMsg) :-
    ( Comment = "" -> CommentMsg=[]
    ; CommentMsg = ['"', $$(Comment), '"']
    ),
    TestMsg = ['(', F, '/', A, ' ', [](CommentMsg), ' in lines ', LB, '-', LE, ')'].

% TODO: document this feature
same_test("[" || Comment1, _, "[" || Comment2, _) :- !,
    same_name(Comment1, Comment2).
same_test(_, Body, _, Body).

same_name("]" || _, "]" || _) :- !.
same_name([C|Cs1], [C|Cs2]) :-
    same_name(Cs1, Cs2).

% TODO: save output and error together, or even save separately
% output, error, and output+error

% TODO: save testout-saved files in a different directory, like .po,
% .asr, .itf, etc. regr_db repo?

