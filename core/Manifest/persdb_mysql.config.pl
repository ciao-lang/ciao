% (included file)

:- doc(section, "Options for MySQL bindings").

:- bundle_flag(with_mysql, [
    comment("Enable MySQL support"),
    valid_values(['yes', 'no']),
    %
    default_comment("MySQL detected"),
    default_value_comment(no,
        "MySQL has not been detected.  If you would like to use the\n"||
        "Ciao-MySQL interface it is highly recommended that you stop\n"||
        "the Ciao configuration now and install MySQL first."),
    rule_default(WithMySQL, verify_mysql(WithMySQL)),
    %
    interactive([extended],
      % .....................................................................
      "Set to \"yes\" if you wish to interface with the MySQL database.\n"||
      "If you choose to have the MySQL interface, you should have the MySQL\n"||
      "client part installed in the machine where you are compiling and using\n"||
      "it.  The MySQL daemon should also be up and running when using the\n"||
      "interface.")
]).

verify_mysql(Value) :-
	( mysql_installed -> Value = yes ; Value = no ).

mysql_installed :-
	detect_c_headers(['mysql/mysql.h']).

% ---------------------------------------------------------------------------

:- use_module(library(messages), [warning_message/1]).

:- bundle_flag(mysql_client_directory, [
    comment("MySQL client library path"),
    needed_if(flag(with_mysql('yes'))),
    rule_default(MySQLDir, get_mysql_dir(MySQLDir)),
    %
    interactive([minimum, extended],
      % .....................................................................
      "Directory where the MySQL client library is installed.")
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
	process_call(path(locate), [FileName],
	             [status(_), stdout(string(S))]),
	atom_codes(FileName, SFileName),
	append(SFileName, "\n", SFileNameN),
	append(SFileDir,  "/" || SFileNameN, S),
	atom_codes(FileDir, SFileDir),
	!.

% Note: libmysqlclient-dev in Debian

