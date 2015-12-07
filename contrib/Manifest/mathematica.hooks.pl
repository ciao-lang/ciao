% (included file)

:- doc(section, "Mathematica bundle").
% TODO: there is more code in the ciaopp/Manifest/ciaopp.hooks.pl

% TODO: Clean code is missing here

:- use_module(library(file_utils), [string_to_file/2]).
:- use_module(library(llists), [flatten/2]).

mathematica_dir := bundle_src(contrib)/library/'mathematica'.
with_mathematica := ~get_bundle_flag(contrib:with_mathematica).

'$builder_hook'(mathematica:item_prebuild_nodocs) :-
	( with_mathematica(yes) ->
	    normal_message("Configuring Mathematica Interface", []),
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
	string_to_file(T, ~fsR(~mathematica_dir/'mathematica_decl_auto.pl')).

