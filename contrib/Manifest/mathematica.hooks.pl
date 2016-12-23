% (included file)

:- doc(section, "Mathematica bundle").
% TODO: there is more code in the ciaopp/Manifest/ciaopp.hooks.pl

% ---------------------------------------------------------------------------

:- bundle_flag(with_mathematica, [
    comment("Enable Mathematica bindings"),
    details(
      % .....................................................................
      "Set to \"yes\" if you wish to interface with the Mathematica.\n" ||
      "If you choose to have the Mathematica interface, you should have\n"||
      "Mathemtical installed in the machine where you are compiling and\n" || 
      "using it, then and/or add to your PATH variable the directory of\n"|| 
      "the MathKernel command line."),
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
    interactive([advanced])
]).

:- use_module(library(system), [using_windows/0]).

% TODO: remove call to bundle_path/3? port mathematica-config.bash to Ciao?
m_bundle_foreign_config_tool(contrib, mathematica, R) :-
	R = ~bundle_path(contrib, 'library/mathematica/mathematica-config.bash').

mathematica_installed :-
	\+ using_windows, % TODO: rewrite sh script so that it does not depend on sh?
	find_executable(~m_bundle_foreign_config_tool(contrib, mathematica), Exec), 
	process_call(path(Exec), ['--kernel'],
	             [stdout(null), stderr(stdout), status(0)]).

verify_mathematica(Value) :-
	( mathematica_installed -> Value = yes ; Value = no ).

% ---------------------------------------------------------------------------

% TODO: Clean code is missing here

:- use_module(ciaobld(messages_aux), [normal_message/2]).

:- use_module(library(file_utils), [string_to_file/2]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(third_party_config), [foreign_config_var/3]).

with_mathematica := ~get_bundle_flag(contrib:with_mathematica).

'$builder_hook'(mathematica:prebuild_bin) :-
	( with_mathematica(yes) ->
	    normal_message("configuring Mathematica interface", []),
	    foreign_config_var(mathematica, 'kernel', MathLink),
 	    foreign_config_var(mathematica, 'cppflags', CompilerOpts),
 	    foreign_config_var(mathematica, 'ldflags', LinkerOpts),
	    T = ~flatten(["%Do not edit generated automatically\n\n",
		    "mathematica_kernel_path('", MathLink, "').\n",
		    ":- if(defined(mathematica__use_c_infertace)).\n",
		    ":- extra_compiler_opts('", CompilerOpts, "').\n",
		    ":- extra_linker_opts('", LinkerOpts, "').\n",
		    ":- endif.\n"])
	;
	    T = "%Do not edit generated automatically\n\n" ||
		"% Mathematica kernel path undefined.\n" ||
		"mathematica_kernel_path('').\n"
	),
	string_to_file(T, ~bundle_path(contrib, 'library/mathematica/mathematica_decl_auto.pl')).

