% (included file)

:- doc(section, "MySQL bundle").
% TODO: move to its own bundle

% ---------------------------------------------------------------------------
% Configuration

:- use_module(ciaobld(detcheader), [detect_c_headers/1]).
:- use_module(library(messages), [warning_message/1]).
:- use_module(library(system), [find_executable/2]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(lists), [append/3]).

:- bundle_flag(with_mysql, [
    comment("Enable MySQL support"),
    details(
      % .....................................................................
      "Set to \"yes\" if you wish to interface with the MySQL database.\n"||
      "If you choose to have the MySQL interface, you should have the MySQL\n"||
      "client part installed in the machine where you are compiling and using\n"||
      "it.  The MySQL daemon should also be up and running when using the\n"||
      "interface."),
    valid_values(['yes', 'no']),
    %
    default_comment("MySQL detected"),
    default_value_comment(no,
        "MySQL has not been detected.  If you would like to use the\n"||
        "Ciao-MySQL interface it is highly recommended that you stop\n"||
        "the Ciao configuration now and install MySQL first."),
    rule_default(HasMySQL, has_mysql(HasMySQL)),
    %
    interactive([advanced])
]).

has_mysql(Value) :-
	( mysql_installed -> Value = yes ; Value = no ).

mysql_installed :-
	detect_c_headers(['mysql/mysql.h']).

% Note: libmysqlclient-dev in Debian

% ---------------------------------------------------------------------------
% Build

:- use_module(library(file_utils), [string_to_file/2]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(third_party_config), [foreign_config_var/4]).
:- use_module(ciaobld(builder_aux), [add_rpath/3]).

with_mysql := ~get_bundle_flag(core:with_mysql).

% Prepare source for build
% (e.g., for automatically generated code, foreign interfaces, etc.)
'$builder_hook'(persdb_mysql:prepare_build_bin) :-
	mysql_prepare_bindings.

mysql_auto_install(_) :- fail.
m_bundle_foreign_config_tool(core, mysql, 'mysql_config').

mysql_prepare_bindings :-
	( with_mysql(yes) ->
	    normal_message("configuring MySQL library", []),
 	    foreign_config_var(core, mysql, 'cflags', CompilerOpts),
 	    foreign_config_var(core, mysql, 'libs', LinkerOpts1),
	    ( mysql_auto_install(yes) ->
	        % If installed as a third party, add ./third-party/lib
	        % to the runtime library search path
	        add_rpath(local_third_party, LinkerOpts1, LinkerOpts2)
	    ; LinkerOpts2 = LinkerOpts1
	    ),
	    add_rpath(executable_path, LinkerOpts2, LinkerOpts),
	    T = ~flatten([
		    ":- extra_compiler_opts(\'"||CompilerOpts, "\').\n"||
		    ":- extra_linker_opts(\'"||LinkerOpts, "\').\n"]),
	    string_to_file(T, ~bundle_path(core, 'library/persdb_mysql/linker_opts_auto.pl'))
	; true
	).

