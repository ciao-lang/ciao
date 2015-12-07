:- module(lpmake, [main/1], [make, assertions, regexp]).

:- use_module(library(terms),    [atom_concat/2]).
:- use_module(library(libpaths), [get_alias_path/0]).
:- use_module(library(make/make_rt)).

:- use_module(library(read_from_string), [read_from_atom_atmvars/2]).

%% :- use_package(trace).

%% ISO Prolog-like modules
:- use_module(library(format),     [format/3]).
:- use_module(library(aggregates), [findall/3]).

%% Ciao libraries
:- use_module(library(errhandle), [handle_error/2]).
:- use_module(library(lists),     [append/3]).
:- use_module(library(system),    [file_exists/1]).
:- use_module(library(messages),  [error_message/2]).
%% *** Will be loaded INTO library also
:- use_module(library(compiler), [use_module/1]).
%% :- use_module(library(compiler),[ensure_loaded/1,use_module/1]).

:- doc(title,    "The Ciao lpmake scripting facility").
:- doc(subtitle, "A portable make with all the power of Prolog inside").

:- doc(author, "Manuel Hermenegildo").
:- doc(author, "The CLIP Group").
:- doc(address, "@tt{clip@@dia.fi.upm.es}").
:- doc(address, "@tt{http://www.clip.dia.fi.upm.es/}").
:- doc(address, "Facultad de Inform@'{a}tica").
:- doc(address, "Universidad Polit@'{e}cnica de Madrid").

:- doc(copyright, "
Copyright @copyright{} 1997-2002 The Clip Group.

@include{DocCopyright.lpdoc}
").

:- doc(summary, "@apl{lpmake} is a small Ciao application which
   uses the Ciao @lib{make} library to implement dependency-driven
   scripts in a similar way to the Unix @apl{make}
   facility. @apl{lpmake} offers three main advantages. First,
   @em{portability}: compiled to byetcode it runs without need for
   recompilation on any platform where Ciao is supported. Second,
   @em{improved programming capabilities}. While @apl{lpmake} itself
   is simpler than @apl{make}, the full Ciao Prolog language is
   available within @apl{lpmake} scripts. This allows establishing
   more complex dependencies and programming powerful operations
   without resorting to external packages or operating system
   commands. The latter is important because it allows execution on
   multiple operating systems without needing external
   facilities. Finally, @apl{lpmake} offers @em{autodocumentation}: it
   allows adding comments to targets. These are used to report
   progress while running and also to document what @apl{lpmake} can
   be used for in a given directory: calling @apl{lpmake} (without
   arguments) in a directory explains what commands the configuration
   files in that directory implement and what these commands will
   do.").

:- doc(module, "

   @cindex{make} @cindex{lpmake} 

   @bf{Note}: @apl{lpmake} and the @lib{make} library are still under
   development, and they may change in future releases.

   @apl{lpmake} is a Ciao application which uses the Ciao @lib{make}
   library to implement a dependency-driven scripts in a similar way
   to the Unix @apl{make} facility.

   The original purpose of the Unix @apl{make} utility is to determine
   automatically which pieces of a large program needed to be
   recompiled, and issue the commands to recompile them.  In practice,
   @apl{make} is often used for many other purposes: it can be used to
   describe any task where some files must be updated automatically
   from others whenever these change.  @apl{lpmake} can be used for
   the same types of applications as @apl{make}, and also for some new
   ones, and, while being simpler, it offers a number of advantages
   over @apl{make}. The first one is @em{portability}. When compiled
   to a bytecode executable @apl{lpmake} runs on any platform where a
   Ciao engine is available. Also, the fact that typically many of the
   operations are programmed in Prolog within the makefile, not
   needing external applications, improves portability further. The
   second advantage of @apl{lpmake} is @em{improved programming
   capabilities}.  While @apl{lpmake} is simpler than @apl{make},
   @apl{lpmake} allows using the Ciao Prolog language within the
   scripts. This allows establising more complex dependencies and
   programming powerful operations within the make file, and without
   resorting to external packages (e.g., operating system commands),
   which also helps portability. A final advantage of @apl{lpmake} is
   that it supports a form of @em{autodocumentation}: @cindex{lpmake
   autodocumentation} comments associated to targets can be included
   in the configuration files. Calling @apl{lpmake} in a directory
   which has such a configuration file explains what commands the
   configuration file support and what these commands will do.

   @section{General operation}

   To prepare to use @apl{lpmake}, and in a similar way to @apl{make},
   you must write a @index{configuration file}: a module (typically
   called @file{Makefile.pl}) that describes the relationships among
   files in your program or application, and states the commands for
   updating each file.  In the case of compiling a program, typically
   the executable file is obtained from object files, which are in
   turn obtained by compiling source files.  Another example is
   running @apl{latex} and @apl{dvips} on a set of source @tt{.tex}
   files to generate a document in @tt{dvi} and @tt{postscript}
   formats. 

   Once a suitable make file exists, each time you change some source
   files, simply typing @tt{lpmake} suffices to perform all necessary
   operations (recompilations, processing text files, etc.).  The
   @apl{lpmake} program uses the dependency rules in the makefile and
   the last modification times of the files to decide which of the
   files need to be updated.  For each of those files, it issues the
   commands recorded in the makefile. For example, in the
   @apl{latex}/@apl{dvips} case one rule states that the @tt{.dvi}
   file whould be updated from the @tt{.tex} files whenever one of
   them changes and another rule states that the @tt{.ps} file needs
   to be updated from a @tt{.dvi} file every time it changes. The
   rules also describe the commands to be issued to update the files.

   So, the general process is as follows: @apl{lpmake} executes
   commands in the configuration file to update one or more target
   @em{names}, where @em{name} is often a program, but can also be a
   file to be generated or even a ``virtual'' target.  @apl{lpmake}
   updates a target if it depends on prerequisite files that have been
   modified since the target was last modified, or if the target does
   not exist.  You can provide command line arguments to @apl{lpmake}
   to control which files should be regenerated, or how. 

   @section{Format of the Configuration File}

   @apl{lpmake} uses as default configuration file the file
   @file{Makefile.pl}, if it is present in the current directory.
   This can be overridden and another file used by means of the
   @tt{-m} option. The configuration file must be a @em{module} and
   this module must make use of the @lib{make} package. This package
   provides syntax for defining the dependency rules and functionality
   for correctly interpreting these rules. 

   The configuration file can contain such rules and also arbitrary
   Ciao Prolog predicates, and can also import other Ciao modules,
   packages, or make file. This is useful to implement inherintance
   across diferent configuration files, i.e., the values declared in a
   configuration file can be easily made to override those defined in
   another, using the standard Ciao rules for module overriding,
   reexport, etc. The syntax of the rules is described in @ref{The
   Ciao Make Package}, together with some examples.

   @section{lpmake usage}

@comment{This already talks about the autodocumentation...}
@begin{verbatim}
@includefact{usage_message/1}
@end{verbatim}

").

:- doc(ack, "Some parts of the documentation are taken from the
   documentation of GNU's @apl{gmake}.").

:- push_prolog_flag(multi_arity_warnings, off).

% Note: This main/0 could not be necessary, but is present for compatibility

% main :-
% 	prolog_flag(argv, Args, _),
% 	make_toplevel(Args, lpmake).

main(Args) :-
	make_toplevel(Args, lpmake).

:- pop_prolog_flag(multi_arity_warnings).

%% ---------------------------------------------------------------------------
%% Top-level (generic, used to be in make lib, but put here for simplicity)
%% ---------------------------------------------------------------------------

make_toplevel(Args, ApplName) :-
	get_alias_path,
	catch(parse_args(Args, ApplName, options(false, C, C)), E,
	    handle_make_error(E)),
	!.
% make_toplevel(_, _) :-
% 	halt(1).

handle_make_error(make_args_error(Format, Args, ApplName)) :-
	append("~nERROR: ", Format, T1),
	append(T1,          "~n~n", T2),
	format(user_error, T2, Args),
	report_usage(ApplName),
	report_commands(''),
	fail.
handle_make_error(make_error(Format, Args)) :-
	error_message(Format, Args),
	fail.
handle_make_error(error(Error, Where)) :-
	handle_error(Error, Where).

is_help_option(H) :-
	member(H, ['-h', '-help', '--help']).

parse_args([H|Args], ApplName, options(_, C, T)) :-
	is_help_option(H),
	!,
	parse_args(Args, ApplName, options(true, C, T)).
parse_args(['--trace-deps'|Args], ApplName, Options) :-
	asserta_fact(make_option('--trace-deps')),
	!,
	parse_args(Args, ApplName, Options).
parse_args(['-d', NameValue|Args], ApplName, Options) :-
	parse_name_value(NameValue, Name, Value),
	add_name_value(Name, Value),
	!,
	parse_args(Args, ApplName, Options).
parse_args([Type, File0|Args], ApplName, options(H, C, [FFile|ConfigFile])) :-
	read_from_atom_atmvars(File0, File),
	( Type='-m', FFile = File ; Type='-l', FFile = library(File) ),
	!,
	parse_args(Args, ApplName, options(H, C, ConfigFile)).
parse_args(Args, ApplName, Options) :-
	parse_other_options(Args, ApplName, Options),
	!.
parse_args(Args, ApplName, _Options) :-
	throw(make_args_error("~n~w: Unrecognized Option -- ~w~nTry: " ||
		"`~w --help' for more information~n",
		[ApplName, Args, ApplName], ApplName)).

parse_other_options(Args, ApplName, options(H, ConfigFiles, [])) :-
	( ConfigFiles == [] ->
	    default_config_file(A),
	    load_config_files(A, "(default) module")
	;
	    load_config_files(ConfigFiles, "module")
	),
	( H == true ->
	    report_usage(ApplName),
	    report_commands(ConfigFiles)
	;
	    process_targets(Args)
	).

default_config_file(['Makefile.pl']).
default_config_file(['makefile.pl']).

load_config_files([],     _).
load_config_files([M|Ms], Message) :-
	load_config_file(Message, M),
	load_config_files(Ms, Message).

% group:name=value
% group_name_value(NameValue, Name, Value) :-
% 	atom_concat([Name, '=', Value], GroupNameValue).

% parse_other_args_and_load([Type,ConfigFile|Targets],Type,ConfigFile,Targets):- 
% 	(Type == '-m'; Type == '-l'),
% 	!,
% 	load_config_file(Type,"module",ConfigFile).

%parse_other_args_and_load([Type,ConfigFile|Targets],Type,ConfigFile,Targets):-
%% 	Type = '-u',
%% 	!,
%% 	load_config_file(Type,"user file",ConfigFile).
% parse_other_args_and_load(Targets,Type,ConfigFile,Targets) :- 
% 	\+ member('-h', Targets),
% %%	\+ member('-u', Targets),
% 	\+ member('-m', Targets),
% 	\+ member('-l', Targets),
% 	!,
% 	Type = '-m',
% 	ConfigFile = 'Makefile.pl',
% 	load_config_file(Type,"(default) module",ConfigFile).

%% Needed to access predicates generated in user Makefile.pl files
%% Unfortunately, messes up using modules, so we settle for just modules
%% :- import(user,[do_dependency/3,dependency_exists/2,do_target/1,
%%                 target_exists/1,target_deps/2,target_comment/1,
%%                 dependency_comment/3]).

% was:
% load_config_file(Type,Text,ConfigFile) :-
% 	trace_message("loading ~s ~w",[Text,ConfigFile]),
% 	(   Type == '-m' ->
% 	    ConfigModule = ConfigFile
% 	;
% 	    Type == '-l' ->
% 	    ConfigModule = library(ConfigFile)
% 	;
% 	    throw(make_error("'user' configuration files not supported",[]))
% 	),
% 	use_module(ConfigModule),
% 	(call_unknown(_:register_config_file(ConfigModule)) -> true ; true),
% 	 dyn_load_cfg_module_into_make(ConfigModule).

load_config_file(Text, ConfigFile) :-
	trace_message("loading ~s ~w", [Text, ConfigFile]),
	use_module(ConfigFile),
	% TODO: does register_config_file/1 exists anywhere?
	( call_unknown(_:register_config_file(ConfigFile)) -> true ; true ),
	dyn_load_cfg_module_into_make(ConfigFile).

%% ensure_loaded(ConfigFile),
%% dyn_load_cfg_file_into_make(ConfigFile)
% 	(   file_exists(ConfigFile) ->
% 	    ;
% 	    throw(make_error("file ~w does not exist",[ConfigFile]))
% 	).

%% If no target process default if defined
process_targets([]) :-
	get_active_config(AC),
	m_target_exists(AC, default),
	!,
	make(default).
%% else process first target
process_targets([]) :-
	get_active_config(AC),
	m_target_exists(AC, Target),
	!,
	make(Target).
%% If targets specified, process them
process_targets(Targets) :-
	make(Targets),
	!.

%% -u not used any more
%%
%% [--trace-deps] [-u <.../Configfile.pl>] <command1> ... <commandn>
%% 
%%   Process commands <command1> ... <commandn>, using user 
%%   file <.../Configfile.pl> as configuration file. If no 
%%   configuration file is specified a file 'Makefile.pl' in 
%%   the current directory will be used. 
%% 
%% -h     [ -u <.../Configfile.pl> ]
%% -help  [ -u <.../Configfile.pl> ]

%% This is in narrow format because that way it looks nicer in a man page.

usage_message("

Supported command line options:

lpmake [--trace-deps] [-d Name1=Value1] ... [-d Namen=Valuen] \\
	<command1> ... <commandn>

  Process commands <command1> ... <commandn>, using file 'Makefile.pl'
  or directory 'installer' in the current directory as configuration
  file. The configuration file must be a module.

  The optional argument '--trace-deps' produces verbose output,
  reporting on the processing of the dependency rules.  Very useful
  for debugging makefiles.

  The argument '-d' indicates that a variable definition Name=Value
  follows.  The effect of this is adding the fact 'name_value(Name, Value).'
  (i.e., 'name_value(Name) := Value.'), defined in the module 
  library(make/make_rt).

lpmake [--trace-deps] [-d Name1=Value1] ... [-d Namen=Valuen] \\
	[[-m|-l] <.../Configfile1.pl>] [[-m|-l] <.../Configfilen.pl>] \\
        <command1> ... <commandn>

  Same as above, but using files <.../Configfilex.pl> as configuration
  file. One or more configuration files can be used. When using more
  than one configuration file, the additional configuration files are
  loaded dynamically into the first one with the predicate
  register_config_file/1.  Using -l instead of -m indicates that this
  configuration file is a library module (i.e., it will be looked for
  in the libraries).

lpmake -h     [ [-m|-l] <.../Configfile.pl> ]
lpmake -help  [ [-m|-l] <.../Configfile.pl> ]
lpmake --help [ [-m|-l] <.../Configfile.pl> ]

  Print this help message. If a configuration file is available in the
  current directory or is given as an option, and the commands in it
  are commented, then information on these commands is also printed.

").

report_usage(ApplName) :-
	format(user_error, "~nUsage:~n~n       ~w <option(s)> <command(s)>~n",
	    [ApplName]),
	usage_message(Text),
	format(user_error, Text, []).

report_commands(LoadedFile) :-
	format(user_error, "~nSupported commands:~n", []),
	report_commands_aux(LoadedFile).

report_commands_aux('') :-
	!,
	format(user_error, "~n(no configuration file loaded)~n", []).
report_commands_aux(LoadedFile) :-
	format(user_error, "[From module: ~w]~n~n", [LoadedFile]),
	( findall(Target, m_target_exists(_, Target), Targets),
	  Targets = [_|_] ->
	    ( % (failure-driven loop)
	      member(Target, Targets),
	        format(user_error, "    ~w:~n    ", [Target]),
	        ( call_unknown(_:target_comment(Target)) ->
		    true
	        ; m_target_comment(_, Target, Comment, Args) ->
		    format(user_error, Comment, Args)
	        ; format(user_error, "(no information available)~n", [])
	        ),
	        format(user_error, "~n~n", []),
	      fail
	    ; true
	    )
	; format(user_error,
		"(no documented commands in the configuration file)~n",
		[])
	).
