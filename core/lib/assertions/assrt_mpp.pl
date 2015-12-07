:- module(assrt_mpp,
	[ cleanup_mpp_data/0,
	  dump_mpp_file/2,
	  module_precompiled/2,
	  mpp_file_name/2,
	  previous_true/4,
	  previous_entry/4,
	  read_mpp_file/2,
	  related_module_used/3
	],
	[ assertions
	]).

:- use_module(library(fastrw)).
:- use_module(library(read), [read/1]).
:- use_module(library(system), [chmod/2, fmode/2, modif_time0/2]).

%% ---------------------------------------------------------------------------

:- doc(title,"Assertion processing library (for mpp files)").

:- doc(author,"Francisco Bueno").

%% ---------------------------------------------------------------------------
%% data

:- data module_precompiled/2.   % MODULE has been compiled for the Nth time
:- data related_module_used/3.  % MODULE has used RELATED file module info 
                                % when such info was in the Nth iteration
:- data previous_true/4.        % In BASENAME there is a GOAL with
                                % call-pattern CALL and succ-pattern SUCC
:- data previous_entry/4.       % There is a call to BASENAME's GOAL with
                                % call-pattern INFO from basename ORIGIN

cleanup_mpp_data:-
	retractall_fact(mpp_read(_,_)),
	retractall_fact(module_precompiled(_,_)),
	retractall_fact(related_module_used(_,_,_)),
	retractall_fact(previous_true(_,_,_,_)),
	retractall_fact(previous_entry(_,_,_,_)).

%% ---------------------------------------------------------------------------
:- pred read_mpp_file(BaseName,Verbose)
	# "Reads the data in the @var{BaseName}.mpp file unless its
	   version is different from current version.".

:- data mpp_read/2. % BASENAME.mpp has been read on DATE

mpp_file_name(BaseName,MppName):-
	atom_concat(BaseName,'.mpp',MppName).


read_mpp_file(BaseName,V):-
	mpp_read(BaseName,ReadDate),
	mpp_file_name(BaseName,MppName),
	modif_time0(MppName,MppDate),
	MppDate =< ReadDate, !,
	verbose(V,Verb),
	verb_message(Verb,['{No need to read ',MppName,'}']).
read_mpp_file(BaseName,V):-
	verbose(V,Verb),
	retractall_fact(mpp_read(BaseName,_)),
	read_mpp_file_(BaseName,Verb), !,
	mpp_file_name(BaseName,MppName),
	modif_time0(MppName,MppDate),
	asserta_fact(mpp_read(BaseName,MppDate)).
read_mpp_file(_BaseName,_V).

read_mpp_file_(BaseName,Verb):-
	mpp_file_name(BaseName,MppName),
        prolog_flag(fileerrors,OldFE,off),
        open(MppName,read,Stream),
        current_input(CI),
        set_input(Stream),
        ( mpp_version(V),    % check mpp version
          read(v(V)), 
	  !
        ; verb_message(Verb,['{Old version in ',MppName,'}']),
	  set_input(CI),
          close(Stream),
          fail
        ),
        verb_message(Verb,['{Reading ',MppName]),
        read_mpp_data_loop(BaseName,Verb),
        set_input(CI),
        close(Stream),
        verb_message(Verb,'}'),
        set_prolog_flag(fileerrors, OldFE).

read_mpp_data_loop(BaseName,Verb):-
	fast_read(X), !,
	convert_data(X,BaseName,Fact),
	assertz_fact(Fact),
	% verb_message(Verb,[ 'Asserted: ',Fact]),
	read_mpp_data_loop(BaseName,Verb).
read_mpp_data_loop(_BaseName,_Verb).

convert_data(p(N),BaseName,module_precompiled(BaseName,N)).
convert_data(r(B,N),BaseName,related_module_used(BaseName,B,N)).
convert_data(t(G,C,S),BaseName,previous_true(BaseName,G,C,S)).
convert_data(e(G,C,B),BaseName,previous_entry(BaseName,G,C,B)).

%% ---------------------------------------------------------------------------
:- pred dump_mpp_file(BaseName,Verbose)
	# "Writes data to @var{BaseName}.mpp file.".

dump_mpp_file(BaseName,V):-
	verbose(V,Verb),
	dump_mpp_file_(BaseName,Verb).

dump_mpp_file_(BaseName,Verb):-
	mpp_file_name(BaseName,MppName),
        prolog_flag(fileerrors,OldFE,off),
	absolute_file_name(BaseName,PlName),
	( open(MppName,write,Stream) ->
            current_output(CO),
            set_output(Stream),
	    verb_message(Verb,['{Generating ',MppName]),
            mpp_version(V),
            display_term(v(V)),
            dump_mpp_data_loop(BaseName,Verb),
            set_output(CO),
            close(Stream),
	    fmode(PlName,Mode),
            chmod(MppName,Mode),
            verb_message(Verb,'}')
        ;   message(['{In ',PlName]),
	    message(warning, ['cannot create ',MppName]),
	    message('}')
	),
        set_prolog_flag(fileerrors, OldFE).

dump_mpp_data_loop(BaseName,_Verb):-
	convert_data(Data,BaseName,Fact),
	current_fact(Fact),
	fast_write(Data),
	% verb_message(Verb,[ 'Written: ',Fact]),
	fail.
dump_mpp_data_loop(_BaseName,_Verb).

%% ---------------------------------------------------------------------------

verbose(V,Verb):-
	( ( prolog_flag(verbose_compilation,on,on) ; V=='-v' )
	-> Verb = verbose
	;  Verb = quiet ).

verb_message(verbose,Message):- io_aux:message(Message).
verb_message(quiet,_Message).

mpp_version(0).
