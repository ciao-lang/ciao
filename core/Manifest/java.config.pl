% (included file)

:- doc(section, "Options for Java Interface").

:- bundle_flag(with_java_interface, [
    comment("Enable Java interface"),
    valid_values(['yes', 'no']),
    %
    default_comment("javac and javadoc detected"),
    default_value_comment(no,
        "Java has not been detected. If you would like to use the\n"||
        "utilities for the Java interface it is highly recommended that\n"||
        "you stop the Ciao configuration now and install Java first."),
% 	    "If Java is already installed and you use Debian/Ubuntu perhaps\n"||
% 	    "you forgot to run: sudo update-java-alternatives --set java-6-sun."
    rule_default(VerifyJava, verify_java(VerifyJava)),
    %
    interactive([extended],
      % .....................................................................
      "Whether you have a reasonably recent version of Java.\n"||
      "If so the utilities for the Java interface under\n"||
      "<CIAOSRC>/core/library/javall will be compiled, along with\n"||
      "examples and documentation.")
]).

verify_java(Value) :-
	( javac_installed, javadoc_installed -> Value = yes ; Value = no ).

javac_installed :-
	find_executable('javac', _),
	is_sun_javac.

% TODO: Any better way to detect Sun Java? (EMM)
is_sun_javac :-
	process_call(path(javac), ['-version'],
	             [stderr(stdout), stdout(string(String)), status(_)]),
	append(_, "javac 1."||_, String),
	process_call(path(java), ['-version'],
	             [stderr(stdout), stdout(string(SJava)), status(_)]),
	% Kludge: In linux 64, you have to use the 64-bit Server VM --EMM
	( ( get_platform('LINUXi686') ; get_platform('LINUXx86_64') ) ->
	    append(_, "64-Bit"||_, SJava)
	; true
	),
	!.

javadoc_installed :- find_executable('javadoc', _).

