% (included file)
% ===========================================================================
:- doc(section, "Parsing of builder commands from the CLI").

:- export(parse_cmd/3).
% Parse a builder command (including arguments, targets, options, etc.)
% (See cmd_fmt for the format of each command)
%
%  Opts: list of options
%  Cmd: cmd/1 or cmd_on_set/2
%
parse_cmd([], _Cmd, _Opts) :- !,
	throw(error_msg("No arguments were specified", [])).
parse_cmd([Cmd0|Args], Cmd, Opts) :-
	% Normalize command name
	norm_underscores(Cmd0, Cmd1),
	% Get expected arguments for Cmd1
	( cmd_fmt(Cmd1, CmdFmt) -> true
	; throw(unknown_cmd(Cmd1))
	),
	% Parse command and options
	parse_fmt(CmdFmt, Flags, Opts0, Targets0, Args, RestArgs),
	( RestArgs = [] -> true
	; throw(error_msg("Invalid additional arguments ('~w')", [RestArgs]))
	),
	( var(Opts0) -> Opts0 = [] ; true ), % (no opts)
	% Add flags as command argument (if needed)
	( member(config_flags, CmdFmt) ->
	    Cmd2 =.. [Cmd1, Flags] % add as command argument
	; Cmd2 = Cmd1 % no flags
	),
	% Rewrite the command
	( cmd_rw(Cmd2, Cmd3, Opts0, Opts, Targets0, Targets1, CmdFmt2) ->
	    true
	; Cmd3 = Cmd2, Opts = Opts0, Targets1 = Targets0, CmdFmt2 = CmdFmt
	),
	% Add default target (if needed)
	( member(target_args, CmdFmt2), Targets1 = [] -> % guess some
	    Targets = ['.']
	; Targets = Targets1
	),
	% Get command
	( ( member(target_args, CmdFmt2)
	  ; member(target_arg, CmdFmt2)
	  ) ->
	    Cmd = cmd_on_set(Cmd3, Targets)
	; Cmd = cmd(Cmd3)
	).

% ---------------------------------------------------------------------------

parse_fmt([config_flags|CmdFmt], Flags, Opts, Targets) --> !,
	parse_flags(Flags),
	parse_fmt(CmdFmt, Flags, Opts, Targets).
parse_fmt([opts(OptsFmt)|CmdFmt], Flags, Opts, Targets) --> !,
	parse_opts(OptsFmt, Opts),
	parse_fmt(CmdFmt, Flags, Opts, Targets).
parse_fmt([target_args|CmdFmt], Flags, Opts, Targets) --> !,
	parse_targets(Targets),
	parse_fmt(CmdFmt, Flags, Opts, Targets).
parse_fmt([target_arg|CmdFmt], Flags, Opts, Targets) --> !,
	parse_targets(Targets),
	{ Targets = [_] }, % a single target
	parse_fmt(CmdFmt, Flags, Opts, Targets).
parse_fmt([raw_args|CmdFmt], Flags, Opts, Targets) --> !,
	parse_raw(Targets),
	parse_fmt(CmdFmt, Flags, Opts, Targets).
parse_fmt([], _, _, _) --> !.

% ---------------------------------------------------------------------------
:- doc(section, "Hooks for definition of command formats").

:- discontiguous(cmd_fmt/2).
% cmd_fmt(Cmd, CmdFmt):
%
%   CmdFmt specifies how command arguments are parsed:
%
%    - raw_args: raw arguments (may look like flags or options)
%    - target_args: first non-flag argument is the target (bundle or bundle part)
%    - config_flags: parse configuration flags (pass all as argument of command)
%    - opts(...): parse list of options with given format
%        name (atom): an option (--name)
%        name=V: an option assignment (--name=value)
%        (name,V): an option assignment (with space) (--name value)
%
%      Values V can be one of:
%        v: a value (any string as an atom)
%        f: a flag (bundle:flag or flag)
%        f=v: a flag assignment
:- discontiguous(cmd_rw/7).
% cmd_rw(Cmd, Cmd2, Opts, Opts2, Args, Args2, CmtFmt2):
%   Post-process the parsed command into a meaningful query for the
%   build system.

% ---------------------------------------------------------------------------
:- doc(section, "Parse command line elements (options, flags, targets)").

:- use_module(library(lists), [list_concat/2]).
:- use_module(library(hiordlib), [map/3]).

parse_opts(OptsFmt, Opts) -->
	parse_opt(OptsFmt, Opt),
	!,
	( { Opt = end } -> [] % stop
	; { Opts = [Opt|Opts0] },
	  parse_opts(OptsFmt, Opts0)
	).
parse_opts(_OptsFmt, []) --> [].

parse_opt(OptsFmt, Opt) -->
	[Arg0],
	{ atom_concat('--', Name0, Arg0) },
	( { Name0 = '' } ->
	    { Opt = end } % No more options (--)
	; { parse_opt_assign_atm(Name0, Name, Value) } ->
	    ( { member(Name=v, OptsFmt) } -> % --Name=Value syntax
		{ Opt = opt(Name, Value) }
	    ; { fail }
	    )
	; { norm_underscores(Name0, Name) },
	  ( { member(Name, OptsFmt) } -> % --Name syntax
	      { Opt = opt(Name) }
	  ; [Arg1], % (consume another arg)
	    ( { member((Name,V), OptsFmt) } -> % --Name V syntax
		{ parse_val(V, Arg1, Value) },
		{ Opt = opt(Name, Value) }
	    ; { fail }
	    )
	  )
	).

parse_val(v, Arg, Arg) :- !. % value
parse_val(f, Arg, Value) :- !, % flag
	parse_flag_atm(Arg, Value).
parse_val(v=v, Arg, Value) :- !, % opt=value
	parse_opt_assign_atm(Arg, Opt, Val),
	Value=(Opt=Val).
parse_val(f=v, Arg, Value) :- !, % flag=value
	parse_flag_assign_atm(Arg, Flag, Val),
	Value=(Flag=Val).

% Parse an option assignment (value as atom)
parse_opt_assign_atm(Param, Name, Value) :-
	parse_assign_str(Param, NameS, ValueS),
	map(NameS, norm_underscore, NameS2),
	atom_codes(Name, NameS2),
	atom_codes(Value, ValueS).

% Parse configuration flags
parse_flags(Flags) -->
	parse_flag(Flag),
	!,
	( { Flag = end } -> [] % stop
	; { Flags = [Flag|Flags0] },
	  parse_flags(Flags0)
	).
parse_flags([]) --> [].

parse_flag(Flag) -->
	[Arg0],
	{ atom_concat('--', Assign, Arg0) },
	{ Assign = '' ->
	    Flag = end % No more flags (--)
	; parse_flag_assign_atm(Assign, Name, Value) ->
	    Flag = flag(Name, Value)
	; fail
	}.

% Parse a (maybe qualified) flag name (normalize codes)
parse_flag_atm(Param, Flag) :-
	atom_codes(Param, ParamS),
	parse_flag_codes(ParamS, Flag).

parse_flag_codes(ParamS, Bundle:Name) :-
	( list_concat([BundleS, ":", NameS], ParamS) ->
	    atom_codes(Bundle, BundleS)
	; root_bundle(Bundle), % default bundle % TODO: use default target instead?
	  NameS = ParamS
	),
	map(NameS, norm_underscore, ParamS2),
	atom_codes(Name, ParamS2).

% Parse a (maybe qualified) flag assignment (value as atom)
parse_flag_assign_atm(Param, Flag, Value) :-
	parse_assign_str(Param, ParamS, ValueS),
	parse_flag_codes(ParamS, Flag),
	atom_codes(Value, ValueS).

% Parse an assignment (...=...)
parse_assign_str(Assign, ParamS, ValueS) :-
	atom_codes(Assign, AssignS),
	list_concat([ParamS, "=", ValueS], AssignS),
	!.

% A target is an argument that does not look like an option ('--...')
parse_targets([Arg0|Targets]) -->
	[Arg0],
	{ \+ atom_concat('--', _, Arg0) },
	!,
	parse_targets(Targets).
parse_targets([]) --> [].

% Consume the rest of the arguments
parse_raw([X|Xs]) --> [X], !, parse_raw(Xs).
parse_raw([]) --> [].

% Replace 0'- by 0'_
norm_underscores(X0, X) :-
	atom_codes(X0, Cs0),
	map(Cs0, norm_underscore, Cs),
	atom_codes(X, Cs).

norm_underscore(0'-, 0'_) :- !.
norm_underscore(C,   C).

