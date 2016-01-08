:- module(bundle_flags, [], [assertions, dcg, fsyntax]).

:- doc(title, "Bundle configuration flags").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "@cindex{bundle flag} Bundle flags define parameters of
   the bundle configuration. These are similar to @concept{prolog
   flag}s but qualified by its bundle (i.e., @tt{Bundle:Name} is the
   flag @tt{Name} on bundle @tt{Bundle})").

:- use_module(library(system), [file_exists/1]).
:- use_module(library(streams), [open_output/2, close_output/1]).
:- use_module(library(read), [read/2]).

% ===========================================================================

% Restore the saved configuration (if the file exists)
% TODO: Move somewhere else?
:- initialization(restore_all_bundle_flags).

% ===========================================================================
% Database of bundle flags

% bundle_flag_(Name,Bundle,Value)
:- data bundle_flag_/3.

% ===========================================================================
% Bundle flags (get/set configuration values)

:- export(current_bundle_flag/2).
:- pred current_bundle_flag(Flag, Value) # "Current value @var{Value}
   of bundle flag @var{Flag}".

current_bundle_flag(Bundle:Name, Value) :-
	bundle_flag_(Name, Bundle, Value).

:- export(set_bundle_flag/2).
:- pred set_bundle_flag(Flag, Value) # "Set bundle flag @var{Flag}
   value to @var{Value}".

set_bundle_flag(Bundle:Name, Value) :-
	retractall_fact(bundle_flag_(Name, Bundle, _)),
	assertz_fact(bundle_flag_(Name, Bundle, Value)).

:- export(get_bundle_flag/2).
:- pred get_bundle_flag(Flag, Value) # "Like
   @pred{current_bundle_flag/2} but throws exception if the @var{Flag}
   is not found".

get_bundle_flag(Bundle:Name, Value) :-
	bundle_flag_(Name, Bundle, Value0),
	!,
	Value = Value0.
get_bundle_flag(Bundle:Name, _Value) :-
	throw(error(unknown_bundle_flag(Bundle,Name), get_bundle_flag/2-1)).

:- export(clean_bundle_flags/0).
:- pred clean_bundle_flags # "Clean all bundle flags".

clean_bundle_flags :-
	retractall_fact(bundle_flag_(_, _, _)).

% ===========================================================================

:- doc(bug, "Need file locks during bundle flags, reuse (a minimal
   version of) persdb?").
:- doc(bug, "Use fastrw, like in bundle registries, to minimize dependencies").

% :- use_module(engine(internals), [reload_bundleregs/0]).
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(ciaobld(config_common), [local_bldid/1]).

% TODO: use one file per bundle?
bundle_flags_file := ~fsR(builddir(~local_bldid)/'ciao.config_saved').

:- export(restore_all_bundle_flags/0).
:- pred restore_all_bundle_flags # "Restore the bundle configuration
   values from persistent store (filesystem)".

restore_all_bundle_flags :-
	% reload_bundleregs, % TODO: Hack! make initialization explicit and use the right order
	restore_bundle_flags.
 
restore_bundle_flags :-
	clean_bundle_flags,
	%
	FlagsFile = ~bundle_flags_file,
	( file_exists(FlagsFile) ->
	    open(FlagsFile, read, Stream),
	    restore_bundle_flags_(Stream, FlagsFile),
	    close(Stream)
	; % Do nothing if no configuration exists yet
	  true
	).

restore_bundle_flags_(Stream, FlagsFile) :-
	read(Stream, R),
	( R = end_of_file ->
	    true
	; nonvar(R), R = bundle_flag(Bundle, Name, Value) ->
%	    display(user_error, read_bundle_flag(Name, Value)), nl(user_error),
	    assertz_fact(bundle_flag_(Name, Bundle, Value)),
	    restore_bundle_flags_(Stream, FlagsFile)
	; throw(error(corrupt_bundle_flags_entry(R), restore_bundle_flags(FlagsFile)))
	).

:- use_module(library(system), [delete_file/1]).

:- export(reset_all_bundle_flags/0).
:- pred reset_all_bundle_flags # "Reset current configuration values
   (useful to ignore saved values)".

reset_all_bundle_flags :-
	clean_bundle_flags,
	% Delete config file (so that it is not reloaded later)
	FlagsFile = ~bundle_flags_file,
	( file_exists(FlagsFile) -> delete_file(FlagsFile) ; true ).

% ===========================================================================

:- export(save_bundle_flags/0).
:- pred save_bundle_flags # "Save the bundle configuration values to
   persistent store (filesystem)".

save_bundle_flags :-
	FlagsFile = ~bundle_flags_file,
	open_output(FlagsFile, Output),
	write_bundle_flags,
	close_output(Output).

write_bundle_flags :-
	( % (failure-driven loop)
	  current_bundle_flag(Bundle:Name, Value),
	    displayq(bundle_flag(Bundle, Name, Value)),
	    display('.'),
	    nl,
	    fail
	; true
	).


