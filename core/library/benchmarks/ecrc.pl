:- module(ecrc, 
        [
            main/1,
	    benchmark_usage/1,
            just_benchmarks/0,
            generate_human_file/0,
            generate_machine_file/0,
            send_info_to_developers/0,
            arithm_average/2,
            geom_average/2
        ], 
        [assertions,basicmodes,regtypes,unittestdecls]).

:- use_module(library(aggregates)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(dec10_io)).
:- use_module(library(terms)).
:- use_package(hiord).
:- use_module(library(hiordlib)).
:- use_module(library(getopts)).

%% ECRC:s benchmarks from Prolog Digest 1986
%% fixed and automated for industrial SICStus
%% by Thomas Sjöland SICS 91 07 18

%% Adapted for Ciao Prolog by MCL.  Main changes:
%% * Syntax & modules adaptad to Ciao Prolog
%% * Size of benchmarks & number of repetitions adjusted to modern
%%   computer's speed
%% * 'void' loop to heat up caches & several repetitions of each benchmark.
%% * Write out more descriptive names of benchmarks
%% * Corrected non-determinism found in some loop drivers!
%% * Driver loops are now failure-driven
%% * Added pretty printing + results are asserted + printed in fact form

:- doc(title, "Analytic benchmarks").
:- doc(subtitle, "Determining the performance of Prolog systems").

:- doc(author, "Manuel Carro (adapted to Ciao Prolog)").

:- doc(copyright, "Manuel Carro, the Clip Lab, and the original authors").

:- doc(summary, "A set of analytic benchmarks to measure the
performance of a Prolog system.").

:- doc(module, "This module provides a set of analytic benchmarks
which try to isolate and measure the speed of certain very common
operations in a Prolog system.  The benchmarks come from a variety of
sources, mostly within former ECRC (please look at the comments in the
source code) and were posted by Jean-Claude Syre on 24th June 1986 to
the Prolog Digest.  Further packaging was done by Thoma Sjoland and
SICS.  They were adapted to Ciao Prolog by Manuel Carro, by:

@begin{itemize}

@item Changing the syntax to Ciao Prolog, when needed, including
extensive use of higher-order programming for the benchmarking loops.

@item Adapting the size of the benchmarks and the number of
repetitions to fit modern computers and Prolog compilation.

@item Changing the format of the output and recording performance data
in the incore database.

@item Changing benchmarking loops to be failure-driven.

@item Adding a void initial dry run to heat up the caches.

@end{itemize}

The comments corresponding to the @bf{original} programs follow.  They
have been largely unchanged, except to reflect changes in the program
interface necessary to perform the modularization and to adapt them to
Ciao Prolog.  Of course the number of repeated calls was changed.  The
original comments are still in the source files.

@section{Testing Calls}

This is the one you always dreamed to test! Like all benchmarks, it
uses a loop calling the actual benchmark program. The benchmark
program consists of a sequence of 200 predicates having no arguments,
no choice points, NOTHING. 200 is chosen to have sufficient accuracy
in measuring the execution time.

The results show the effect of pure calls, and the Klips performance
can be called the peak performance of the prolog system. Note
that the peak performance has very little significance to classify
the overall performance of a Prolog system.

@section{Testing non-deterministic behavior}

This program contains a series of 3 different benchmark predicates.

The predicate @pred{choice_point/1} tests calls invoking the creation
of a choice point, i.e. a branch point where the execution will
possibly come back to in case of backtracking. It does NOT
backtrack. Two versions are proposed, one with and the other without
arguments.

We then present two predicates to evaluate the mechanism of
backtracking during execution. Both predicates create one choice_point
and then backtrack 20 times on every loop iteration
step. @pred{baktrak1/1} exhibits a kind of backtracking called
@em{deep}, while @pred{baktrak2/1} deals with @em{shallow}
backtracking.  Both are worth being tried, whatever your particular
Prolog System is.

@section{Testing environment handling}

The creation and deletion of environments are an important feature in
prolog machines. The following program attempts to evaluate that.  A
usual condition to have environments is that the clause is made of
several goals. Thus there will be calls in the clause creating
environments, and some work to set the parameters of each call. Three
arguments per goal were chosen because this number comes close to the
average number of arguments of a predicate and to the average number
of permanent variables in an environment.  The arguments were arranged
in different orders for every goal, because we did not want to measure
the merits of register transfer optimisations.  Note that these
transfers exist, so the results cannot be compared with those given by
the program which creates choice points (generally slower).

Another version, @pred{envir0ar/1}, with 0 argument in each call, can
also be usefully tried

@section{Testing indexing mechanisms}

We give only one test for indexing, i.e. the selection of a clause due
to the type of an argument. This program does not test the merits of
indexing on an argument other than the first one.  It does not test
for multiple indexing either. It does not show the inefficiency which
occurs if 2 choice points per clause are created. This may happen
e.g. in Warren's indexing scheme.

Each of these tests would require an extra benchmark program.  The
program given below tests the main point in indexing. Right now we
think it is not worth adding all this complexity to the benchmarks, in
order to measure all the details in indexing.  Therefore we give only
this single test.

@section{Testing unification}

We have 6 programs to evaluate the process of unification in
the Prolog system:

@begin{itemize}

@item Test of list construction via unification.

@item Test of list matching unification.

@item Test of structure construction via unification This program is
equivalent to construct_list, except that it uses the standard
structure representation instead of the simplified list notation.

@item Test of structure matching via unification.  This predicate
matches a list of 100 elements in structure notation.

@item Test to match a nested structure.  This predicate tests the
(compiled) unification of a complex structure.

@item Test of general unification of 2 complex structures.  This
predicate tests general unification.  We call it general unification,
because it cannot be analysed at compile time. Therefore this kind of
unification cannot be compiled and, even in a compiled system, it must
be handled at run time, exactly as by an interpreter.  This is done by
a general procedure for unification.  The name of the benchmark
therefore does not reflect that the unification is general,
i.e. including all Prolog types (e.g. it does not contain variables),
but it reflects the use of the procedure for general unification as
opposed to specific, compiled unification.

@end{itemize}

@em{Manuel Carro: note that in this case the term \"Logical Inference\"
is a bit contrived, since by design some of these (head) unifications
are very more compled, naturally being slower and giving slow KLIPS results.}

@section{Testing dereferencing}

Program to benchmark the dereferencing speed.  It constructs a list
containing 500 variables which are then bound together. Since
different systems use different strategies for binding variables on
the global stack, the whole is made for two lists and the long
variable chain is created only in one of them.

@em{Manuel Carro: different results in this benchmark are not likely
to affect larger, general programs.  It is a well-known fact that n
programs tend not to generate long dereferencing chains.  Empirical
measurements show that dereference chains of length greater than three
are extremely rare.  So a suboptimal / optimal behavior in this test
is not likely to affect greatly the overall speed of a system.}

@section{Testing the cut}


It seems almost impossible to isolate the cut operator in a simple
test program. However, the cut-testing program in this benchmark set
contains a lot of cut at exec time. It may be regarded as a partial
test of cut, and may be worthwhile for some software implementations
of Prolog.  @pred{cuttest/1} calls the cutit11 predicate, which
performs 100 calls to a predicate cutt1 where a cut operator appears
in the second clause. Having indexing makes the evaluation of the cut
more accurate, so please indicate in our result whether or not your
Prolog system uses indexing, to clarify the comparison with others.


@section{Assorted small programs}

Here we deal with prolog programs that do something, while being still
small but representative of some well-known Prolog computations. This
set should be augmented by other programs, some of them might come
from your ideas.

Some of the following programs were taken from the Berkeley paper by
Peter Van Roy \"A Prolog Compiler for the PLM\".  Other programs were
kindly donated by the following ECRC members: Helmut Simonis, Mehmet
Dincbas, Micha Meier and Pascal Van Hentenryck.

The programs have been statically analysed and they represent fairly
standard programs as far as the statistical averages are
concerned. That is the arity of most clauses is 2 or 3 and there are
usually 2 or 3 clauses per predicate.  The programs range from fairly
trivial programs like fibonacci series to problems such as Hamiltonian
graph traversal.

Also, some more programs have been added since the last release and
some corrections have been made. Most of the writes were removed in
order to reduce i/o activity.

The programs added were symbolic differentiation (from Warren's paper)
and a quick sort algorithm using difference lists. The last addition
is a bit of a rogue: its a naive reverse, where one can enter the list
length. The list gets constructed and then gets reversed.

We are grateful to Saumya Debray from Stony Brook and others for
comments, suggestions, feedback and useful inputs.

These benchmarks were run on a VAX 785 with 8 Meg of memory, under 4.2
BSD Unix. The interpreter was C-Prolog version 1.5.

This entire file (without mail/net headers) contains 584 lines.

@begin{verbatim}
Name      |      Call by      |  # of Inferences  | KLips
          |                   |  (one iteration)  | (C-Prolog)
----------+-------------------+-------------------+-----------
fib       | fibonacci(1).     |        4932       |   2.0
----------+-------------------+-------------------+-----------
map       | map(200).         |          68       |   1.3
----------+-------------------+-------------------+-----------
mham      | mham(1).          |      493824       |   1.7
----------+-------------------+-------------------+-----------
mutest    | mutest(1).        |        1366       |   2.3
----------+-------------------+-------------------+-----------
quicksort | qs(10).           |         601       |   1.9
----------+-------------------+-------------------+-----------
queens    | qu(10).           |         684       |   1.7
----------+-------------------+-------------------+-----------
query     | query(1).         |        2294       |   0.9
----------+-------------------+-------------------+-----------
sym_diff  | differen(150).    |          71       |   1.5
----------+-------------------+-------------------+-----------
diff_lists| diff(50).         |         608       |   2.1
----------+-------------------+-------------------+-----------
nrev  10  | nrev.             |          66       |   2.0
----------+-------------------+-------------------+-----------
nrev  30  | nrev.             |         496       |   2.5
----------+-------------------+-------------------+-----------
nrev  50  | nrev.             |        1326       |   2.5
----------+-------------------+-------------------+-----------
nrev 100  | nrev.             |        5151       |   2.5
----------+-------------------+-------------------+-----------
nrev 150  | nrev.             |       11476       |   2.5
----------+-------------------+-------------------+-----------
nrev 200  | nrev.             |       20301       |   2.5
----------+-------------------+-------------------+-----------
@end{verbatim}

").

:- doc(filetype, module).

:- doc(bug, "The actual logical inferences each benchmark does has 
                 to be checked.").

:- use_module(library(prolog_sys)).
:- use_module(benchmark_utilities).

:- push_prolog_flag(unused_pred_warnings, no).
:- use_module(library(benchmarks/boresea)).
:- use_module(library(benchmarks/choice)).
:- use_module(library(benchmarks/envir)).
:- use_module(library(benchmarks/index)).
:- use_module(library(benchmarks/unif)).
:- use_module(library(benchmarks/deref)).
:- use_module(library(benchmarks/cut)).
:- pop_prolog_flag(unused_pred_warnings).
:- use_module(library(benchmarks/small_programs)).

:- use_module(library(benchmarks/results)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred main(+Flags) : list(benchmark_usage) # "Main entry point.
Execute all benchmarks and report on the performance obtained.  This
makes it easy to run the set of benchmarks as an executable.  Its
behavior regarding printing gathered data can be controlled with the
list of flags passed as argument.  Data is @bf{always} asserted and
available to other programs through the @pred{dump_benchmark_data/0}
and @pred{access_benchmark_data/8} predicates.".

main(Args):-
        getopts(Args, [ 'estimation', 'no-human', 'no-machine', 
                        'send-info', 'base-file-name'/1], _, Rest),
       ( 
           Rest \== [] ->
           format("Unknown options: ~w~n", Rest),
           fail
   ;
           getopts(Args, [estimation], Estim, Args0),
           (Estim = [_] -> estimate_time(Ti, Est, BenchElaps); true),
           just_benchmarks,
           (
               getopts(Args0, ['base-file-name'/1], 
                      ['base-file-name'(FName)], Args1) ->
               atom_concat(FName, '.txt', FNameHuman),
               atom_concat(FName, '.pl', FNameProlog)
           ;
               FNameProlog = user_output, 
               FNameHuman = user_output,
               Args1 = Args0
           ),
           ( cl_option(Args1, 'no-human') -> 
             true ; tell(FNameHuman), generate_human_file, told ),
           ( cl_option(Args1, 'no-machine') ->
             true ; tell(FNameProlog), generate_machine_file, told ),
           ( cl_option(Args1, 'send-info') ->
              send_info_to_developers ; true),
           (Estim = [_] -> recalibrate(Ti, Est, BenchElaps) ; true)
       ).

:- pred generate_human_file # "Print to standard output a
human-readable report of the information gathered by running
@pred{just_benchmarks/0}.".

generate_human_file:-
        get_general_data(Compiler, CompOptions, 
                         CompVersion,
                         Host, OS, CPU, Arch, MHz, Date),
        format("
C Compiler:       ~w
Compiler options: ~w
Compiler version: ~w
Host:             ~w
O.S.:             ~w
CPU:              ~w
Architecture:     ~w
MHZ:              ~w
Date:             ~w~n", 
        [Compiler, CompOptions, 
         CompVersion,
         Host, OS, CPU, Arch, MHz, Date]),
        get_section(Section),
          format("~n~w~n", [Section]),
          format("=====================================~n", []),
          get_bench(Section, BenchmarkDesc, EntryPoint),
            format("[~w]~n", [BenchmarkDesc]),
            get_timings(EntryPoint, OT, ELT, BT, KLIPS),
              format("  Overall / Empty / Benchmark: ~5f / ~5f / ~5f~n", 
                     [OT,ELT,BT]),
              format("  KLIPS:   ~5f~n", [KLIPS]),
        fail.


%% Average KLIPS: we have per-benchmark KLIPS stored, but we cannot
%% average them: they must be weighted with the time taken by every
%% benchmark. 

generate_human_file:-
        findall((KLSP, Time), get_timings(_,_,_, Time, KLSP), AllTimings),
        arithm_average(AllTimings, AAVG),
        format("~nArithmetical average KLIPS: ~5f~n", AAVG),
        geom_average(AllTimings, GAVG),
        format("~nGeometrical average KLIPS: ~5f~n", GAVG),
        fail.
generate_human_file.

arithm_average(List, AAVG):- 
        foldl(List, (0, 0), 
             (_((KLips, BenchTime), (KIn, TimeIn), (KOut, TimeOut)) :- 
                 TimeOut is TimeIn + BenchTime, 
                 KOut is KIn + BenchTime*KLips), 
             (LipSum, TimeSum)),
        AAVG is LipSum / TimeSum.

geom_average(List, GAVG):- 
        foldl(List, (0, 0), 
             (_((KLips, BenchTime), (KIn, TimeIn), (KOut, TimeOut)) :- 
                 TimeOut is TimeIn + BenchTime, 
                 KOut is KIn + BenchTime * log(KLips)), 
                 (LipLogProd, TimeSum)),
        GAVG is exp(LipLogProd/TimeSum).

:- pred generate_machine_file # "Print to standard output a
machine-readable report of the information gathered by running
@pred{just_benchmarks/0}.".


generate_machine_file:-
        format("~n~n%~70c~n",[0'-]),
        get_general_data(Compiler, CompOptions, 
                         CompVersion,
                         Host, OS, CPU, Arch, MHz, Data),
        show(general_data(Compiler, CompOptions, 
                          CompVersion,
                          Host, OS, CPU, Arch, MHz, Data)),
        get_section(A), show(section(A)), fail.
generate_machine_file:-
        get_bench(A,B,C), show(benchmark(A,B,C)), fail.
generate_machine_file:-
        get_timings(A,B,C,D,E), show(timing(A,B,C,D,E)), fail.
generate_machine_file:-
        format("~n~n%~70c~n",[0'-]).

show(Term):-
        displayq(Term),
        display(.),
        nl.
        

mail_to('mcarro@clip.dia.fi.upm.es').

:- pred send_info_to_developers # "Send a message to the Ciao
developers with a report of the information gathered by running
@pred{just_benchmarks/0}.".


send_info_to_developers:-
        (
            member(MailProgram, 
               ['/bin/mail', '/usr/bin/mail', '/sbin/mail', '/usr/sbin/mail']),
            file_exists(MailProgram, 1) ->
            mktemp_in_tmp('ecrcXXXXXX', Filename),
            tell(Filename),
            generate_human_file,
            format("~n~n%----------------------------------------------~n~n",
                   []),
            generate_machine_file,
            told,
            mail_to(Dest),
            atom_concat([MailProgram, 
                         ' -s "Ciao performance information" ', 
                         Dest, ' < ', Filename],
                        MailCommand),
            system(MailCommand)
        ;
            format(user_error,
            "Tried to send info to developers, but no mail program found!~n",
            [])
        ).
            


% This estimation is a rough one.  If it is _too_ rough, it asks the
% user to recalibrate it.  The idea is: we take the total time in some
% architecture and we relate it with the time used to perform the nrev
% benchmark.  Then we measure, in the actual computer, the time the
% nrev benchmark actually took, and we use it to give an estimation of
% the total time.  After the execution we check the accuracy of the
% prediction.  
%
% Since we want to give real time (it is a prediction for whoever is
% running the benchmark), we have to use walltime instead of runtime 

calibration_time(1.88).
total_time(228.78).

estimate_time(InitialSec, Estimation, ElapsedSec):-
        format(user_error, "Estimating total benchmarking time... ", []),
        flush_output(user_error),
        run_calibration,               % Warm up cache, raise processor speed
        run_calibration,               % Warm up cache, raise processor speed
        statistics(walltime, [InitialMS, _]),
        InitialSec is InitialMS / 1000,
        run_calibration,
        run_calibration,
        statistics(walltime, [AfterMS, _]),
        AfterSec is AfterMS / 1000,
        ElapsedSec is AfterSec - InitialSec,
        total_time(OtherEstimation),
        calibration_time(Calib),
        Estimation is OtherEstimation*ElapsedSec/Calib,
        format(user_error, " ~f seconds.~n", [Estimation]),
        flush_output(user_error).
        

run_calibration:-
        list50(List),
        append(List, List, List2),
        append(List2, List2, List4),
        append(List4, List4, List8),
        run_bench(1000, qsort(List8, _, []), 0, 0, no),
        fail.
run_calibration.

allowed_error(0.1).

recalibrate(InitialSec, Estimation, BenchElapsed):-
        statistics(walltime, [ActualEndMS, _]),
%        display(final(ActualEndMS)), nl,
        ActualEndSec is ActualEndMS / 1000,
        ActualElapsedSec is ActualEndSec - InitialSec,
        allowed_error(RelErr),
        (
            Estimation * RelErr > abs(ActualElapsedSec - Estimation) ->
            true
        ;
            format(user_error, 
                   "~n~n~nExcessive (> ~f) estimation error~n", [RelErr]),
            format(user_error, "
Suggestion: update the benchmark sources using

calibration_time(~f).
total_time(~f).~n", [BenchElapsed, ActualElapsedSec])).
  


:- doc(doinclude, benchmark_usage/1).

:- prop benchmark_usage(?Flag) :: atm + regtype # "Options which
determine what this module should do with the execution results when
called through the @pred{main/1} entry point (i.e., if compiled to an
executable).  It is defined as

@includedef{benchmark_usage/1}

with the following meaning:

@begin{itemize}

@item @tt{'--no-human'}: do @bf{not} dump human-readable data.

@item @tt{'--no-machine'}: do @bf{not} dump data as a series of facts
(which is a machine-readable format) which can be saved to a file and
later read back in Prolog.

@item @tt{'--send-info'}: send a mail to the Ciao developers with the
information gathered plus a terse description of the machine (O.S.,
architecture, CPU type and speed).  The existence of a suitable user
command to send mail is expected.  No message is sent otherwise.  No
sensible (personal, etc.) information is gathered or sent.

@item @tt{--base-file-name @em{file-name}}: use @em{file-name} as a
base to generate file with the reports this module generates.  The
machine-oriented file will have the @tt{.pl} extension and the
human-oriented file will have the @tt{.txt} extension.

@end{itemize}

The options aboce can be used when calling @pred{main/1} predicate or
as command-line options for an executable built from this file.  Note
that the default options @bf{print} available data both in
human-readable and machine-readable formats.".

benchmark_usage('--estimation').
benchmark_usage('--no-machine').
benchmark_usage('--no-human').
benchmark_usage('--send-info').
benchmark_usage('--base-file-name').


:- pred just_benchmarks # "Run the set of benchmarks in this program
and save the speed information gathered.  They can be later accessed
using the predicates @pred{generate_machine_file/0} or
@pred{generate_human_file/0}.".

just_benchmarks :- 
        clean_benchmark_data,
 (
        perform_test('Measure LIPS using simple calls',
                     ['Chained calls to 0-arity clauses'-boresea(200000)])
 ;
        perform_test('Choice point manipulation',
        [
            'Choice points'-choice_point(600000),
            'Zero-arity choice points'-choice_point0ar(1000000),
            'Deep backtracking'-baktrak1(500000),
            'Shallow backtracking'-baktrak2(1000000)
        ])
 ;
        perform_test('Environments',
        [
            'Environment creation'-envir(100000),
            'Empty environment creation'-envir0ar(200000)
        ])
 ;
        perform_test('Indexing', 
        [
            'Call clauses with different indexing properties'-index(500000)
        ])
 ;

        perform_test('Unification benchmarks',
        [
            'List construction'-construct_list(100000),
            'List matching'-match_list(100000),
            'Structure construction'-construct_structure(100000),
            'Structure matching'-match_structure(100000),
            'Nested structure matching'-match_nested_structure(500000),
            'Unification of complex structures'-general_unification(200000)
        ])
 ;
        perform_test('Measuring efficiency of dereferencing', 
                     ['Dereferencing'-deref(50000)])
 ;
        perform_test('Measuring efficiency of cut', 
        [
            'Cut on the second clause of a 3-clause predicate'-cuttest(100000)
        ])
 ;
        perform_test('Assorted small programs',
        [
            'Fibonacci'-fibonacci(3000),
            'Map colouring'-map(1600),
            'Hamiltonian paths'-mham(20),
            'MU theorem'-mutest(10000),
            'Quicksort'-qs(30000),
            '4-Queens'-qu(1500),
            'Querying a database'-query(4000),
            'Symbolic differentiation'-differen(100000),
            'Difference lists'-diff(30000),
            'Naive reverse'-nrev(40000)
       ])
 ).

just_benchmarks.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

perform_test(Name, Calls):-
        add_section(Name),
        do_calls(Calls, Name),
        fail.


do_calls([], _Section).
do_calls([Explanation-C|Cs], Section):- !,
        functor(C, GoalName, _),
        add_bench(Section, Explanation, GoalName),
        call(C),
        do_calls(Cs, Section).


