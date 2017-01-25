% (included file)

:- doc(section, "MySQL bundle").

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
    rule_default(WithMySQL, verify_mysql(WithMySQL)),
    %
    interactive([advanced])
]).

verify_mysql(Value) :-
	( mysql_installed -> Value = yes ; Value = no ).

mysql_installed :-
	detect_c_headers(['mysql/mysql.h']).

:- bundle_flag(mysql_client_directory, [
    comment("MySQL client library path"),
    details(
      % .....................................................................
      "Directory where the MySQL client library is installed."),
    needed_if(flag(with_mysql('yes'))),
    rule_default(MySQLDir, get_mysql_dir(MySQLDir)),
    %
    interactive
]).

% TODO: This is not a good way of detecting MySQL
get_mysql_dir(MySQL) :-
	locate_file('libmysqlclient.a', MySQL),
	!.
get_mysql_dir('/usr/lib/mysql') :-
	warning_message(
	    "Unable to determine where the MySQL client library is " ||
	    "installed.\nCurrent value (/usr/lib/mysql) is only a guess.").

% TODO: Use mysql_config
% TODO: REMOVE! 'locate' IS NOT available in many platforms (e.g., Mac OS X)
locate_file(FileName, FileDir) :-
	find_executable('locate', LocateExec), % (silently fail if no 'locate')
	process_call(LocateExec, [FileName],
	             [status(_), stdout(string(S))]),
	atom_codes(FileName, SFileName),
	append(SFileName, "\n", SFileNameN),
	append(SFileDir,  "/" || SFileNameN, S),
	atom_codes(FileDir, SFileDir),
	!.

% Note: libmysqlclient-dev in Debian

% ---------------------------------------------------------------------------
% Build

:- use_module(ciaobld(builder_aux), [wr_template/4]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(system), [file_exists/1]).

with_mysql := ~get_bundle_flag(core:with_mysql).
mysql_client_directory := ~get_bundle_flag(core:mysql_client_directory).

% Prepare source for build
% (e.g., for automatically generated code, foreign interfaces, etc.)
'$builder_hook'(persdb_mysql:prepare_build_bin) :-
	( with_mysql(yes) ->
	    % TODO: ask bundle instead
	    wr_template(origin,
	        ~bundle_path(core, 'library/persdb_mysql'),
	        'linker_opts_auto.pl',
	        ['where_mysql_client_lives' = ~mysql_client_directory]),
	    % TODO: why?
	    ( file_exists(~bundle_path(core, 'library/persdb_mysql_op')) ->
	        wr_template(origin,
		    ~bundle_path(core, 'library/persdb_mysql_op'),
		    'linker_opts_auto.pl',
		    ['where_mysql_client_lives' = ~mysql_client_directory])
	    ; true
	    )
	; true
	).
