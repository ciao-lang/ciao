:- module(bundle_flags, [], [assertions, dcg, fsyntax]).

:- doc(title, "Bundle configuration flags").
:- doc(author, "Jose F. Morales").
:- doc(author, "The Ciao Development Team").

:- doc(module, "@cindex{bundle flag} Bundle flags define parameters of
   the bundle configuration. These are similar to @concept{prolog
   flag}s but qualified by its bundle (i.e., @tt{Bundle:Name} is the
   flag @tt{Name} on bundle @tt{Bundle})").

:- use_module(engine(data_facts)).
:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(stream_utils), [open_output/2, close_output/1]).
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

:- export(del_bundle_flag/1).
:- pred del_bundle_flag(Flag) # "Remove bundle flag @var{Flag}".

del_bundle_flag(Bundle:Name) :-
	retractall_fact(bundle_flag_(Name, Bundle, _)).

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

:- export(clean_bundle_flags/1).
:- pred clean_bundle_flags(Bundle) # "Clean bundle flags for @var{Bundle}".

clean_bundle_flags(Bundle) :-
	retractall_fact(bundle_flag_(_, Bundle, _)).

% ===========================================================================

:- doc(bug, "Need file locks during bundle flags, reuse (a minimal
   version of) persdb?").
:- doc(bug, "Use fastrw, like in bundle registries, to minimize dependencies").

:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(engine(internals), ['$bundle_id'/1, '$bundle_regfile'/2]).

:- export(bundle_flags_file/2).
% Obtain the path to the bundlecfg for the specified (registered) bundle
bundle_flags_file(Bundle) := Path :-
	( '$bundle_regfile'(Bundle, RegFile) -> true ; fail ),
	path_split(RegFile, BundleRegDir, _),
	bundlecfg_filename(Bundle, BundleRegDir, Path).

% NOTE: We reuse the directory for bundleregs!
:- export(bundlecfg_filename/3).
:- pred bundlecfg_filename(Bundle, BundleRegDir, File)
   # "@var{File} is the bundlecfg file for bundle @var{Bundle}, where
      @var{BundleRegDir} is the directory where bundleregs are
      stored".

bundlecfg_filename(Bundle, BundleRegDir, File) :-
	atom_concat(Bundle, '.bundlecfg', File0),
	path_concat(BundleRegDir, File0, File).

:- export(restore_all_bundle_flags/0).
:- pred restore_all_bundle_flags # "Restore the bundle configuration
   values from persistent store (filesystem)".

restore_all_bundle_flags :-
	( '$bundle_id'(Bundle),
	    clean_bundle_flags(Bundle),
	    restore_bundle_flags(Bundle),
	    fail
	; true
	).

restore_bundle_flags(Bundle) :-
	FlagsFile = ~bundle_flags_file(Bundle),
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
	; throw(error(corrupt_bundle_flags_entry(R), restore_bundle_flags_(FlagsFile)))
	).

:- export(reset_bundle_flags/1).
:- pred reset_bundle_flags(Bundle) # "Remove saved configuration
   values (it will be empty after @pred{restore_bundle_flags/1})".

reset_bundle_flags(Bundle) :-
	FlagsFile = ~bundle_flags_file(Bundle),
	my_del_file_nofail(FlagsFile).

:- use_module(library(system), [delete_file/1]).
% TODO: merge with system_extra:del_file_nofail?
my_del_file_nofail(FileName) :-
	( file_exists(FileName) -> delete_file(FileName) ; true ).

% ===========================================================================

:- export(save_bundle_flags/1).
:- pred save_bundle_flags(Bundle) # "Save the bundle configuration
   values to persistent store (filesystem)".

save_bundle_flags(Bundle) :-
	FlagsFile = ~bundle_flags_file(Bundle),
	open_output(FlagsFile, Output),
	write_bundle_flags(Bundle),
	close_output(Output).

write_bundle_flags(Bundle) :-
	( % (failure-driven loop)
	  current_bundle_flag(Bundle:Name, Value),
	    displayq(bundle_flag(Bundle, Name, Value)),
	    display('.'),
	    nl,
	    fail
	; true
	).


