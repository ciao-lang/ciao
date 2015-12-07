:- module(getopts, [getopts/4, cl_option/2],
          [assertions,basicmodes,regtypes]).


:- use_module(library(lists)).

:- doc(title, "Parse and return command-line options").

:- doc(author, "Manuel Carro").

:- pred getopts(+Arguments, +Opts, ?Matched, ?Rest) ::
        list(atom) * list(spec) * list(term) * list(term) # 

"@concept{Ciao Prolog} parses the command-line arguments of its
executables and passes them as a list of atoms to the @pred{main/1}
predicate.  Thus, a shell invocation such as 

@code{./my_program -file input.txt -file input2.txt --output_file out.txt -create-dir --decode --unsorte} 

makes @pred{main/1} receive an argument such as 

@code{['-file', 'input.txt', '-file', 'input2.txt', '--output_file', 'out.txt', '-create-dir', '--decode', '--unsorte']}


@pred{getopts/4} can be used to parse such an command-line option
list.  passed in the @var{Arguments} parameter.  @var{Opts} is a list
of expected options, each option being an @em{option spec}, i.e., a
term of the form @tt{atom/arity}.  For every @tt{atom} a command-line
option of the form @tt{'--atom'} or @tt{'-atom'} is expected, with
@tt{arity} arguments following it.  An @tt{arity} of zero can be
omitted.  For each matched option spec, the list @var{Matched} will
contain a term of the form @tt{atom(Arg1, Arg2, ..., Argn)}, where
@code{n = arity}.  The list @var{Rest} will contain the unmatched
element in @var{Arguments}.  

@var{Rest} will respect the relative order of the elements in
@var{Arguments}.  The matching elements in @var{Matched} appear in the
same order as the options in @var{Opts}, and for every option in
@var{Opts}, its matches appear in the order as they came in
@var{Arguments}.

Assuming @var{Arguments} is @code{['-file', 'input.txt', '-file',
'input2.txt', '--output_file', 'out.txt', '-create-dir', '--decode',
'--unsorte']}, some possible uses of @pred{getopts/4} follow.

@begin{itemize}
@item Check that a simple option has been selected:
@begin{verbatim}
?- getopts(Args, ['create-dir'], M, R).
Args = ...
M = ['create-dir'],
R = ['-file','input.txt','-file','input2.txt','--output_file',
     'out.txt','--decode','--unsorte']
@end{verbatim}

@item Which argument was given to an option expecting an additional value?
@begin{verbatim}
1 ?- getopts(Args, [output_file/1], M, R).
Args = ...
M = [output_file('out.txt')],
R = ['-file','input.txt','-file','input2.txt','-create-dir',
     '--decode','--unsorte'] 

1 ?- getopts(Args, [output_file/1], [output_file(F)], R).
Args = ..
F = 'out.txt',
R = ['-file','input.txt','-file','input2.txt','-create-dir',
     '--decode','--unsorte'] 
@end{verbatim}

@item Extract options (and associated values) which can appear several times.
@begin{verbatim}
1 ?- getopts(Args, [file/1], M, R).
Args = ...
M = [file('input.txt'),file('input2.txt')],
R = ['--output_file','out.txt','-create-dir','--decode',
     '--unsorte'] 
@end{verbatim}

@item  Was decoding selected?
@begin{verbatim}
1 ?- getopts(Args, [decode], [_], R).
Args = ...
R = ['-file','input.txt','-file','input2.txt','--output_file',
     'out.txt', '-create-dir','--unsorte']
@end{verbatim}

@item Was encoding selected?
@begin{verbatim}
1 ?- getopts(Args, [encoding], [_], R).
no
@end{verbatim}

@item Was decoding @bf{not} selected?
@begin{verbatim}
1 ?- getopts(Args, [decode], [], R).
no
@end{verbatim}

@item Are all the options passed to the program legal options?  If this is not the case, which option(s) is/are not legal?
@begin{verbatim}
1 ?- getopts(Args, [file/1, output_file/1, 'create-dir', 
                    encode, decode, unsorted], _, R).
Args = ...
R = ['--unsorte'] ? 
@end{verbatim}
@end{itemize}

The complexity of @pred{getopts/1} is currently @em{O(La x Lo)}, where
@em{La} is the length of the argument list and @em{Lo} is the length
of the option list.

".

getopts(Args, Opts, Matched, Rest):-
        getopts_(Opts, Args, Matched, Rest).

getopts_([], Args, [], Args).
getopts_([Option|Opts], Args, Matched, Rest):-
        process_one_option(Args, Option, Matched0, Args0),
        getopts_(Opts, Args0, Matched1, Rest),
        append(Matched0, Matched1, Matched).


process_one_option([], _Option, [], []).
process_one_option([Arg|Args], Spec, Matched, Unmatched):-
        (Spec = Name/Arity ; Spec = Name, Arity = 0), !,
        (
            (atom_concat('-',  Name, Arg);
             atom_concat('--', Name, Arg)) ->
             functor(OptFunct, Name, Arity), 
             construct_option(1, Arity, Args, RestArgs, OptFunct),
             Matched = [OptFunct|RestMatches],
             process_one_option(RestArgs, Spec, RestMatches, Unmatched)
            
        ;
            Unmatched = [Arg|RestUnmatched],
            process_one_option(Args, Spec, Matched, RestUnmatched)
        ).
        
construct_option(Current, Arity, Args, Args, _OptFunctor):-
        Current > Arity, !.
construct_option(Current, Arity, [A|As], Rest, OptFunct):-
        Current =< Arity,
        arg(Current, OptFunct, A),
        Next is Current + 1,
        construct_option(Next, Arity, As, Rest, OptFunct).


:- pred cl_option(+Arguments, +Option) :: list(atom) * spec # "Check
that @var{Option} is an option in @var{Arguments}.".

cl_option(Args, Option):- getopts(Args, [Option], [_|_], _).


:- doc(doinclude, spec/1).

:- prop spec(Spec) + regtype # "@var{Spec} is @code{AtomName/Arity}" .

spec(At/Ar):- atm(At), nnegint(Ar).
