% (included file)

:- doc(section, "GSL bundle").
% TODO: Share code for PPL, GMP, GSL.

:- use_module(library(file_utils), [string_to_file/2]).
:- use_module(library(pathnames), [path_relocate/4, path_concat/3]).
:- use_module(library(system_extra), [mkpath/1]).

% Specification of GSL (third-party component)
:- def_third_party(gsl, [
    version('1.16'),
    source_url(tar('http://ftp.gnu.org/gnu/gsl/gsl-1.16.tar.gz')),
    source_md5("e49a664db13d81c968415cd53f62bc8b"),
    %
    build_system(gnu_build_system)
]).

:- use_module(library(llists), [flatten/2]).

gsl_dir := bundle_src(contrib)/library/'gsl_imports'.
with_gsl := ~get_bundle_flag(contrib:with_gsl).
auto_install_gsl := ~get_bundle_flag(contrib:auto_install_gsl).

% TODO: rename by 'gsl' (no mathlibs)?
'$builder_hook'(mathlibs:item_prebuild_nodocs) :-
	do_auto_install_gsl,
	prebuild_gsl_bindings.

:- use_module(ciaobld(third_party_install), [auto_install/1]).
:- use_module(ciaobld(config_common), [bld_eng_path/4, bundle_to_bldid/2]).

do_auto_install_gsl :-
	( auto_install_gsl(yes) -> 
	    normal_message("Auto-install GSL (third party)", []),
	    third_party_install:auto_install(gsl)
	; true
	).

prebuild_gsl_bindings :-
	( with_gsl(yes) ->
	    normal_message("Configuring GSL Library", []),
	    S = ":- use_module(library(gsl_imports)).\n",
 	    foreign_config_var(gsl, 'cflags', CompilerOpts),
 	    foreign_config_var(gsl, 'libs', LinkerOpts0),
	    fix_linker_opts(LinkerOpts0, LinkerOpts1),
	    ( % If gsl is installed as a third party, add ./third-party/lib
	      % to the runtime library search path
	      auto_install_gsl(yes) ->
		% TODO: better way to compute RelativeLibDir.
		fsR(bundle_src(ciao),  CiaoSrc),
		third_party_path(libdir, LibDir),
		path_relocate(CiaoSrc, '.', LibDir, RelativeLibDir),
		atom_codes(RelativeLibDir, RelativeLibDir_),
		append("-Wl,-rpath,"||RelativeLibDir_, " "||LinkerOpts1, LinkerOpts)
	    ;
	        LinkerOpts1 = LinkerOpts
	    ),
	    T = ~flatten([
		    ":- extra_compiler_opts(\'"||CompilerOpts, "\').\n"||
		    ":- extra_linker_opts(\'"||LinkerOpts, "\').\n"]),
	    string_to_file(T, ~fsR(~gsl_dir/'gsl_imports_decl_auto.pl')),
	    % List of static libraries from GSL
	    % TODO: generalize for any other library
	    M = ~flatten(["GSL_STAT_LIBS=\'"||LinkerOpts, "\'\n"])
	;
	    LinkerOpts = "",
	    normal_message("Ignoring GSL Library", []),
	    S = ":- use_module(library(gsl_imports/gsl_imports_dummy)).\n",
	    M = ""
	),
        % TODO: Make sure that prebuild of this library happens before
	%   engine build. Options in GSL's 'config_sh' are added in
	%   the final 'config_mk' and 'config_sh' files for the
	%   engine.
	%
	% TODO: Simplify, generalize for other libs
	%
	Bundle = contrib,
	bundle_to_bldid(Bundle, GSLBldId),
	GSLEngDir = ~bld_eng_path(cfgdir, GSLBldId, 'gsl'), % NOTE: not an engine
	mkpath(GSLEngDir),
	string_to_file(M, ~path_concat(GSLEngDir, 'config_sh')),
	string_to_file(S, ~fsR(~gsl_dir/'gsl_imports_auto.pl')).

:- use_module(library(lists), [append/3]).

% Remove the -L option, hack that allows to run in LINUXi686 or LINUXx86_64 --EMM:
fix_linker_opts(LinkerOpts0, LinkerOpts) :-
	( get_platform('LINUXx86_64') 
	; get_platform('LINUXi686') 
	),
	append("-L"||_, " "||LinkerOpts, LinkerOpts0),
	!.
fix_linker_opts(LinkerOpts, LinkerOpts).

