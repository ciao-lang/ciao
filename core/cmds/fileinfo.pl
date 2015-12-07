%% #!/usr/bin/env ciao-shell 
% -*- mode: ciao; -*-

%% Ciao syntax
:- use_package([assertions]).  

%% ISO Compat
:- use_module(library(read)).  
:- use_module(library(fastrw)).  
:- use_module(library(format)).  
:- use_module(library(aggregates)).  

%% Ciao libraries
:- use_module(library(compiler/c_itf)).
:- use_module(library(assertions/assrt_lib)).
:- use_module(library(assertions/assrt_write)).

:- doc(title,"Printing the declarations and code in a file").

:- doc(author,"Manuel Hermenegildo").

:- doc(module,"A simple program for @concept{printing assertion
   information} (@concept{predicate declarations}, @concept{property
   declarations}, @concept{type declarations}, etc.) and
   @concept{printing code-related information} (@concept{imports},
   @concept{exports}, @concept{libraries used}, etc.)  on a file. The
   file should be a single Ciao or Prolog source file. It uses the
   Ciao compiler's pass one to do it. This program is specially useful
   for example for checking what assertions the @concept{assertion
   normalizer} is producing from the original assertions in the file
   or to check what the compiler is actually seeing after some of the
   syntactic expansions (but before goal translations). 

   @section{Usage (fileinfo)}

   @begin{verbatim}
   @includefact{usage_text/1}
   @end{verbatim}

   @section{More detailed explanation of options (fileinfo)}

   @includefact{option_text/1}
").

:- multifile library_directory/1.
:- dynamic library_directory/1.

main(Args) :-
	handle_args(Args).

handle_args(['-h']) :-
	usage.
handle_args(['-asr', File]) :-
	print_asr(File).
handle_args(IArgs) :-
	(  IArgs = ['-v'|Args]
	-> prolog_flag(verbose_compilation,_,on)
	;  prolog_flag(verbose_compilation,_,off),
	   Args=IArgs ),
	( Args = [Opt1,Opt2,Main], 
	  Opt1 = '-m' 
	; Args = [Opt2,Main] ),
	( Opt2 = '-a' ; Opt2 = '-c' ; Opt2 = '-f' ; Opt2 = '-e' ),
	!,
	(  prolog_flag(verbose_compilation,on,on)
	-> format("{Printing info for ~w}~n",[Main])
	;  true ),
	get_code_and_related_assertions(Main,M,Base,_Suffix,_Dir),
	(  Opt1 == '-m' 
	-> DM = M, DBase = Base
	;  true),
	handle_options(Opt2,DM,DBase),
	true.
handle_args(Args) :-
	format("error: invalid arguments ~w~n",[Args]),
	usage.

usage :-
	usage_text(Text),
        format(user_error,"Usage: ~s~n",[Text]).

usage_text("
    fileinfo -asr <filename.asr> 
       : pretty prints the contents of <filename.asr> 

    fileinfo [-v] [-m] <-a|-f|-c|-e> <filename>
    -v : verbose output (e.g., lists all files read)
    -m : restrict info to current module
    -a : print assertions 
    -f : print code and interface (imports/exports, etc.)
    -c : print code only
    -e : print only errors - useful to check syntax of assertions in file

    fileinfo -h
       : print this information
").

option_text("
   @begin{itemize} 

   @item If the @tt{-a} option is selected, @tt{fileinfo} prints the
   assertions (only code-oriented assertions -- not comment-oriented
   assertions) in the file @em{after normalization}. If the @tt{-f}
   option is selected @tt{fileinfo} prints the file interface, the
   declarations contained in the file, and the actual code. The
   @tt{-c} option prints only the code. If the @tt{-e} option is
   selected @tt{fileinfo} prints only any sintactic and import-export
   errors found in the file, including the assertions.

   @item @tt{filename} must be the name of a Prolog or Ciao source
   file.

   @item If the @tt{-m} option is selected, only the information
   related to the current module is printed.

   @item The @tt{-v} option produces verbose output. This is very
   useful for debugging, since all the files accessed during assertion
   normalization are listed.

   @item In the @tt{-asr} usage, @apl{fileinfo} can be used to print
   the contents of a @tt{.asr} file in human-readable form.

   @end{itemize}
").

handle_options('-a',M,_Base) :-
	!,
	prolog_flag(write_strings, Old, on),
	print_assertions(M),
	set_prolog_flag(write_strings, Old).
handle_options('-f',M,Base) :-
	!,
	print_gathered_module_data(M,Base).
handle_options('-c',M,Base) :-
	!,
	print_code_only(M,Base).
handle_options('-e',_M,_Base).

print_gathered_module_data(_M,Base) :-
	set_prolog_flag(write_strings, on),
	format("{Printing all code info~n",[]),

	forall((defines_module(Base,DefMod),
          format("~w defines module ~w~n",[Base,DefMod]))),

	forall((exports(Base,F,A,T,Met),
           format("~w exports ~w/~w (~w) meta=~w~n",[Base,F,A,T,Met]))),

	forall((def_multifile(Base,F,A,Mo),
           format("~w defines multifile ~w/~w as ~w~n",[Base,F,A,Mo]))),

	forall((defines(Base,F,A,T,Met),
           format("~w defines ~w/~w (~w) meta=~w~n",[Base,F,A,T,Met]))),

	forall((decl(Base,Decl),
           format("~w has itf-exported new declaration ~w~n",[Base,Decl]))),

	forall((uses_file(Base,File),
           format("~w uses ~w~n",[Base,File]))),

	forall((adds(Base,File),
           format("~w does ensure_loaded of user file ~w~n",[Base,File]))),

	forall((imports_pred(Base,M2,F,A,DefType,Met,EndFile),
		% M2\==builtin,M2\==internals,
           format("~w imports ~w/~w of type ~w from ~w (~w) meta=~w~n",
	   [Base,F,A,DefType,M2,EndFile,Met]))),

	forall((imports_all(Base,M2), 
           format("~w imports all from ~w~n",[Base,M2]))),

	forall((includes(Base,File), 
           format("~w includes ~w~n",[Base,File]))),

	forall((loads(Base,Path),
           format("~w loads ~w as compilation module~n",[Base,Path]))),

	forall((clause_read(Base,Head,Body,VNs,Source,LB,LE),
           format("~w (~w-~w):~n ~w :- ~w.~nDictionary:~w~n",
                  [Source,LB,LE,Head,Body,VNs]))),

        format("}~n",[]).

print_code_only(_M,Base) :-
	forall((clause_read(Base,Head,Body,VNs,Source,LB,LE),
           format("~w (~w-~w):~n ~w :- ~w.~nDictionary:~w~n",
                  [Source,LB,LE,Head,Body,VNs]))).

forall(G) :-
	( call(G),
	  fail
	;
	  true ).

print_asr(File) :-
	prolog_flag(write_strings, _, on),
	read_asr_file(File).

read_asr_file(File) :-
        current_input(CI),
        open(File, read, Stream),
        set_input(Stream),
	read(Version),
	format("Normalizer version: ~w~n",[Version]),
        read_asr_data_loop,
        set_input(CI),
        close(Stream).

read_asr_data_loop :-
	(  fast_read(X)
	-> process_assrt(X),
	   read_asr_data_loop
	;  true ).

process_assrt(assertion_read(PD,_M,Status,Type,Body,Dict,_S,_LB,_LE)) :- 
	!,
	write_assertion(PD,Status,Type,Body,Dict,status).
process_assrt(clause_read(_Base,H,B,Dict,_S,_LB,_LE)) :- 
	!,
	unify_vars(Dict),
	format("Clause ~w :- ~w.\n",[H,B]).
process_assrt(X) :- 
	format("*** Warning: ~w is not an assertion~n",[X]).

unify_vars([]).
unify_vars([N=V|Dict]):-
	V='$VAR'(N),
	unify_vars(Dict).
