% (included file)

:- doc(section, "PPL (Parma Polyhedra Library) bundle").
% TODO: Share code for PPL, GMP, GSL.

:- use_module(library(file_utils), [string_to_file/2]).
:- use_module(library(pathnames), [path_relocate/4]).

% TODO: Look at PPL-related code in ciaopp/Manifest/ciaopp.hooks.pl
% TODO: Clean code is missing here

% Specification of M4 (third-party component)
:- def_third_party(m4, [
    version('1.4.17'),
    source_url(tar('http://ftp.gnu.org/gnu/m4/m4-1.4.17.tar.bz2')),
    source_md5("8a1787edcba75ae5cd1dc40d7d8ed03a"),
    %
    build_system(gnu_build_system)
]).

% Specification of GMP (third-party component)
:- def_third_party(gmp, [
    version('5.0.5'),
    source_url(tar('https://gmplib.org/download/gmp/gmp-5.0.5.tar.bz2')),
    source_md5("041487d25e9c230b0c42b106361055fe"),
    %
    build_system(gnu_build_system),
    option1('enable-cxx') % for PPL
]).

% Specification of PPL (third-party component)
:- def_third_party(ppl, [
    version('1.0'),
    source_url(tar('http://bugseng.com/products/ppl/download/ftp/releases/1.0/ppl-1.0.tar.bz2')),
    source_md5("8a90e0b0b3e9527609a6e5ed3616fab1"),
    %
    patch(bundle_src(contrib)/'Manifest'/patches/'ppl_1_0.patch'),
    build_system(gnu_build_system),
    option2('enable-interfaces','c cxx')
]).

:- use_module(library(lists), [append/3]).
:- use_module(library(llists), [flatten/2]).

ppl_dir := bundle_src(contrib)/library/'ppl'.
with_ppl := ~get_bundle_flag(contrib:with_ppl).
auto_install_ppl := ~get_bundle_flag(contrib:auto_install_ppl).

remove_all_substrings(Input, String, Output):-
	append(String, Postfix, Input), !,
	remove_all_substrings(Postfix, String, Output).
remove_all_substrings([H|T1], String, [H|T2]):-
	remove_all_substrings(T1, String, T2).
remove_all_substrings([], _String, []).

ppl_version(Version) :-
	foreign_config_version(ppl, Version).

'$builder_hook'(ppl:item_prebuild_nodocs) :-
	do_auto_install_ppl,
	prebuild_ppl_bindings.

:- use_module(ciaobld(third_party_install), [auto_install/1, third_party_path/2]).
:- use_module(ciaobld(builder_aux), [add_rpath/3]).

do_auto_install_ppl :-
	( auto_install_ppl(yes) -> 
	    % TODO: add dependencies between PPL and GMP
	    % normal_message("Auto-install M4 (third party)", []),
	    % third_party_install:auto_install(m4),
	    normal_message("Auto-install GMP (third party)", []),
	    third_party_install:auto_install(gmp),
	    normal_message("Auto-install PPL (third party)", []),
	    third_party_install:auto_install(ppl)
	; true
	).

prebuild_ppl_bindings :-
	( with_ppl(yes) ->
	    normal_message("Configuring PPL Interface", []),
 	    foreign_config_var(ppl, 'cppflags', CompilerOpts1),
 	    foreign_config_var(ppl, 'cxxflags', CompilerOpts2),
	    append(CompilerOpts1, " "||CompilerOpts2, CompilerOpts3),
 	    foreign_config_var(ppl, 'ldflags', LinkerOpts1),
	    patch_arch_opts(CompilerOpts3, LinkerOpts1, CompilerOpts4, LinkerOpts2),
	    remove_all_substrings(CompilerOpts4, "-g ", CompilerOpts), % TODO: parse options and remove '-g' (it is safer)
	    ( auto_install_ppl(yes) ->
	        % If installed as a third party, add ./third-party/lib
	        % to the runtime library search path
	        add_rpath(local_third_party, LinkerOpts2, LinkerOpts3)
	    ; LinkerOpts3 = LinkerOpts2
	    ),
	    add_rpath(executable_path, LinkerOpts3, LinkerOpts4),
%	    LinkerOpts3 = LinkerOpts4,
	    append(LinkerOpts4, " -lstdc++", LinkerOpts),
	    flatten(["%Do not edit generated automatically\n\n",
		    ":- extra_compiler_opts('", CompilerOpts, "').\n",
		    ":- extra_linker_opts('", LinkerOpts, "').\n"], T),
	    % TODO: generalize, share with GSL
	    string_to_file(T, ~fsR(~ppl_dir/'ppl_decl_auto.pl')),
	    ppl_version(Version),
	    ( Version @< [0, 9] ->
		fail
	    ; Version @< [0, 10] ->
		ppl_interface_version("0_9")
	    ;  Version @< [1, 0] ->
		ppl_interface_version("0_10")
	    ; ppl_interface_version("1_0")
	    )
	;
	    string_to_file(
		":- initialization(error('PPL library not installed')).\n\n",
		~fsR(~ppl_dir/'ppl_auto.pl'))
	).

% Patch architecture specific options (for universal OSX binaries)
patch_arch_opts(CompilerOpts0, LinkerOpts0, CompilerOpts, LinkerOpts) :-
	( get_platform('DARWINi686') ->
	    % Remove "-arch x86_64" option if ppl is an MacOS universal binary
	    remove_all_substrings(CompilerOpts0, "-arch x86_64", CompilerOpts),
	    remove_all_substrings(LinkerOpts0, "-arch x86_64", LinkerOpts)
	; CompilerOpts = CompilerOpts0,
	  LinkerOpts = LinkerOpts0
	),
	list(CompilerOpts). % TODO: Why?

% This selects one of the two versions
% TODO: maybe we can use the package 'condcomp' to make this simpler?
ppl_interface_version(StrVer) :-
	S = ~flatten([
           "%Do not edit generated automatically.\n\n"||
           ":- include(library(ppl/'", StrVer, "'/ppl_ciao)).\n"]),
 	string_to_file(S, ~fsR(~ppl_dir/'ppl_auto.pl')).

