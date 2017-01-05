:- module(ciaoc_car, [main/1], [assertions]).

:- doc(title, "Standalone executables with assets").
:- doc(author, "Remy Haemmerle").
:- doc(author, "Jose F. Morales (minor changes)").

:- doc(module, "This command creates standalone executables including
   assets, including foreign libraries, data files, modules that can
   be loaded dynamically, etc.

   Usage:

@begin{verbatim}
$ ciaoc_car SRC DST
@end{verbatim}

   This will create a @tt{DST} executable (a shell script) and a
   @tt{DST.car} directory containing all assets (including relocatable
   binaries).

@begin{alert}
NOTE: This is experimental and will contain more files than needed.
@end{alert}
").

:- use_module(library(pathnames), [path_basename/2, path_concat/3]).
:- use_module(library(system),
	[mktemp_in_tmp/2,
	 file_exists/1,
	 delete_file/1, delete_directory/1,
	 copy_file/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(process), [process_call/3]).

:- use_module(library(compiler/exemaker), [make_exec/2]).
:- use_module(library(libpaths),          [get_alias_path/0]).

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

% ---------------------------------------------------------------------------

% (integration into compiler)
:- doc(bug, "Integrate with ciaoc_sdyn").
:- doc(bug, "Add as dynamic plug-ins for 'ciaoc' (for options
   implemented in other modules, which are not included in bootstrap
   ciaoc)").

% (integration with OptimComp)
:- doc(bug, "Merge with OptimComp's bootstrappable executables (static
   bytecode + autocompilable C code) -- this feature is not provided
   as a dynamic plug-in in OptimComp").
:- doc(bug, "Merge $sh_boot_dir/car_exec_stub.sh with core_OC/compiler/scripts/run.sh").

% (improvements)
:- doc(bug, "Modify $sh_boot_dir/car_exec_stub.sh to auto-build if needed").
:- doc(bug, "Move part of $sh_boot_dir/car_exec_stub.sh as a 'ciao run BINARY' option?").
:- doc(bug, "Use library(source_tree) instead of library(process)").
:- doc(bug, "Do not copy all ciao, just bundles").
:- doc(bug, "Make it work with CIAOCACHEDIR (use
   library(source_tree) to copy .po/.itf)").

% ---------------------------------------------------------------------------

main([SRC, DST]):-
	make_car_exec(SRC, DST).

make_car_exec(SRC, DST):-
	bundle_path(ciao, '.', CiaoSrc),
	path_concat(CiaoSrc, 'builder/sh_boot/car_exec_stub.sh', ExecStub),
	%
	path_basename(DST, DST_BASE_NAME), 
	%
	atom_concat(DST, '.car', DST_CAR),
	path_concat(DST_CAR, 'build/bin', DST_bindir),
	path_concat(DST_bindir, DST_BASE_NAME, DST_BIN), 
	%
	mktemp_in_tmp('ciao_make_car.XXXXXXXX', TMP),
	%
	make_exec([SRC], TMP),
	%
	(file_exists(DST) -> rec_delete_file(DST) ;  true),
	(file_exists(DST_CAR) -> rec_delete_file(DST_CAR);  true),
	%
	% ignore error 
	rec_copy_file(CiaoSrc, DST_CAR),
	rec_copy_file(ExecStub, DST), 
	rec_copy_file(TMP, DST_BIN), 
	rec_delete_file(TMP).

rec_copy_file(SRC, DST):-
	process_call(path(cp), ['-fRp', SRC, DST], []).

rec_delete_file(FILE):-
	( process_call(path(rm), ['-R', FILE], [status(0)]) ->
	    true
	; throw(error(rec_delete_file/1))
	).
