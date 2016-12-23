% (included file)

:- doc(section, "GSL bundle").
% TODO: Share code for PPL, GMP, GSL.

% ===========================================================================

:- bundle_flag(with_gsl, [
    comment("Enable GSL bindings"),
    details(
      % .....................................................................
      "Set to \"yes\" if you wish to interface with the GSL (GNU Scientific\n"||
      "Library). If you choose to have the GSL interface, you should have the\n"||
      "GSL development library installed in the machine where you are\n"||
      "compiling and using it."),
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
    interactive([advanced])
]).

m_bundle_foreign_config_tool(contrib, gsl, 'gsl-config').

% TODO: it should consider auto_install option!
gsl_installed :-
	find_executable(~m_bundle_foreign_config_tool(contrib, gsl), _),
	% TODO: Next literal required because now GSL 32 bits is not available
	% TODO: in Linux 64 bits -- EMM.
	\+ get_platform('LINUXi686').

verify_gsl(Value) :-
	( gsl_installed -> Value = yes ; Value = no ).

:- bundle_flag(auto_install_gsl, [
    comment("Auto-install GSL (third party)"),
    details([advanced],
      % .....................................................................
      "Set to \"yes\" if you want to auto-install GSL (third party)"),
    valid_values(['yes', 'no']),
    %
    rule_default('no'),
    %
    interactive([advanced])
]).

% ===========================================================================

:- use_module(ciaobld(messages_aux), [normal_message/2]).

:- use_module(library(file_utils), [string_to_file/2]).
:- use_module(library(pathnames), [path_relocate/4, path_concat/3]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(third_party_config), [foreign_config_var/3]).

% Specification of GSL (third-party component)
:- def_third_party(gsl, [
    version('1.16'),
    source_url(tar('http://ftp.gnu.org/gnu/gsl/gsl-1.16.tar.gz')),
    source_md5("e49a664db13d81c968415cd53f62bc8b"),
    %
    build_system(gnu_build_system)
]).

:- use_module(library(llists), [flatten/2]).

with_gsl := ~get_bundle_flag(contrib:with_gsl).
auto_install_gsl := ~get_bundle_flag(contrib:auto_install_gsl).

'$builder_hook'(gsl:prebuild_bin) :-
	do_auto_install_gsl,
	prebuild_gsl_bindings.

:- use_module(ciaobld(third_party_install), [auto_install/2]).
:- use_module(ciaobld(eng_defs), [bld_eng_path/3]).

do_auto_install_gsl :-
	( auto_install_gsl(yes) -> 
	    normal_message("auto-installing GSL (third party)", []),
	    third_party_install:auto_install(contrib, gsl)
	; true
	).

prebuild_gsl_bindings :-
	( with_gsl(yes) ->
	    normal_message("configuring GSL library", []),
	    S = ":- use_module(library(gsl_imports)).\n",
 	    foreign_config_var(gsl, 'cflags', CompilerOpts),
 	    foreign_config_var(gsl, 'libs', LinkerOpts0),
	    fix_linker_opts(LinkerOpts0, LinkerOpts1),
	    ( auto_install_gsl(yes) ->
	        % If installed as a third party, add ./third-party/lib
	        % to the runtime library search path
	        add_rpath(local_third_party, LinkerOpts1, LinkerOpts2)
	    ; LinkerOpts2 = LinkerOpts1
	    ),
	    add_rpath(executable_path, LinkerOpts2, LinkerOpts),
	    T = ~flatten([
		    ":- extra_compiler_opts(\'"||CompilerOpts, "\').\n"||
		    ":- extra_linker_opts(\'"||LinkerOpts, "\').\n"]),
	    string_to_file(T, ~bundle_path(contrib, 'library/gsl_imports/gsl_imports_decl_auto.pl')),
	    % List of static libraries from GSL
	    % TODO: generalize for any other library
	    M = ~flatten(["GSL_STAT_LIBS=\'"||LinkerOpts, "\'\n"])
	;
	    LinkerOpts = "",
	    normal_message("ignoring GSL library", []),
	    S = ":- use_module(library(gsl_imports/gsl_imports_dummy)).\n",
	    M = ""
	),
        % TODO: At this moment this is hardwired into the core engine.
	%   Make sure that prebuild of this library happens before
	%   engine build. Options in GSL's 'config_sh' are added in
	%   the final 'config_mk' and 'config_sh' files for the engine
	%
	% TODO: Simplify, generalize for other libs
	%
	EngOpts = [],
	GSLEng = eng_def(core, 'gsl', EngOpts),
	GSLEngDir = ~bld_eng_path(cfgdir, GSLEng), % NOTE: not an engine
	mkpath(GSLEngDir),
	string_to_file(M, ~path_concat(GSLEngDir, 'config_sh')),
	string_to_file(S, ~bundle_path(contrib, 'library/gsl_imports/gsl_imports_auto.pl')).

:- use_module(library(lists), [append/3]).

% Remove the -L option, hack that allows to run in LINUXi686 or LINUXx86_64 --EMM:
fix_linker_opts(LinkerOpts0, LinkerOpts) :-
	( get_platform('LINUXx86_64') 
	; get_platform('LINUXi686') 
	),
	append("-L"||_, " "||LinkerOpts, LinkerOpts0),
	!.
fix_linker_opts(LinkerOpts, LinkerOpts).

