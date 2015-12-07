% (included file)

:- doc(section, "Options for Mathematica Support").

:- bundle_flag(with_mathematica, [
    comment("Enable Mathematica bindings"),
    valid_values(['yes', 'no']),
    %
    default_comment("Mathematica detected"),
    default_value_comment(no,
      % .....................................................................
      "Mathematica has not been detected.  If you want to use the \n" ||
      "Mathematica out of the box interface it is recommended that you\n" || 
      "stop the Ciao configuration now and install Mathematica first \n" ||
      "and/or add the directory of the MathKernel command line to your\n" ||
      "PATH variable."),
    rule_default(WithMathematica, verify_mathematica(WithMathematica)),
    %
    interactive([extended],
      % .....................................................................
      "Set to \"yes\" if you wish to interface with the Mathematica.\n" ||
      "If you choose to have the Mathematica interface, you should have\n"||
      "Mathemtical installed in the machine where you are compiling and\n" || 
      "using it, then and/or add to your PATH variable the directory of\n"|| 
      "the MathKernel command line.")
]).

:- use_module(library(system), [using_windows/0]).

% TODO: remove call to fsR/2? port mathematica-config.bash to Ciao?
m_bundle_foreign_config_tool(contrib, mathematica, ~fsR(bundle_src(contrib)/library/mathematica/'mathematica-config.bash')).

mathematica_installed :-
	\+ using_windows, % TODO: rewrite sh script so that it does not depend on sh?
	find_executable(~m_bundle_foreign_config_tool(contrib, mathematica), Exec), 
	process_call(path(Exec), ['--kernel'],
	             [stdout(null), stderr(stdout), status(0)]).

verify_mathematica(Value) :-
	( mathematica_installed -> Value = yes ; Value = no ).
