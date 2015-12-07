% (included file)
	
:- doc(section, "Options for GSL Library").

:- bundle_flag(with_gsl, [
    comment("Enable GSL bindings"),
    valid_values(['yes', 'no']),
    %
    default_comment("GSL detected"),
    default_value_comment(no,
        "GSL has not been detected.  If you want to use the math\n"||
        "library it is highly recommended that you stop the Ciao\n"||
        "configuration and install the GSL library first."),
    rule_default(WithGSL, verify_gsl(WithGSL)),
    % rule_default('no'),
    %
    interactive([extended],
      % .....................................................................
      "Set to \"yes\" if you wish to interface with the GSL (GNU Scientific\n"||
      "Library). If you choose to have the GSL interface, you should have the\n"||
      "GSL development library installed in the machine where you are\n"||
      "compiling and using it.")
]).

m_bundle_foreign_config_tool(contrib, gsl, 'gsl-config').

% TODO: it should consider auto_install option!
gsl_installed :-
	find_executable(~m_bundle_foreign_config_tool(contrib, gsl), _),
	% TODO: Next literal required because now GSL 32 bits is not available
	% TODO: in Linux 64 bits -- EMM.
	\+ get_platform('LINUXx86_64m32').

verify_gsl(Value) :-
	( gsl_installed -> Value = yes ; Value = no ).

:- bundle_flag(auto_install_gsl, [
    comment("Auto-install GSL (third party)"),
    valid_values(['yes', 'no']),
    %
    rule_default('no'),
    %
    interactive([extended],
      % .....................................................................
      "Set to \"yes\" if you want to auto-install GSL (third party)")
]).

