:- module(compiler_output, [main/1], [assertions]).

:- doc(title,"Print out WAM code").

:- doc(author,"Manuel Carro").

:- doc(module, "This program prints to standard output a symbolic
   form of the Wam code the compiler generates for a given source
   file.

   @section{Usage (compiler_output)}

   @begin{verbatim}
   @includefact{usage_text/1}
   @end{verbatim}

   ").

%:- use_module(library(dec10_io)).
:- use_module(library(read)).
:- use_module(library(compiler), [make_wam/1]).
:- use_module(library(system)).
:- use_module(library(file_utils)).

message:-
	usage_text(T),
        display_string(T).

usage_text("
        Print WAM code for a .pl file

        Usage: compiler_output <file.pl>
").

no_file(F):-
        nl,
        display('File '),
        display(F),
        display(' not found!'),
        message.

main([File]):- 
        !,
        (
	    file_exists(File) ->
	    make_wam([File]),
	    atom_concat(Base, '.pl', File),
	    atom_concat(Base, '.wam', WamFile),
	    file_to_string(WamFile, String),
	    display_string(String)
        ;
            no_file(File)
        ).
main(_):- message.
