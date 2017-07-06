:- module(tmpbased_common, [module_to_addressfile/2], []).

:- use_module(library(system), [get_tmp_dir/1]).
:- use_module(library(pathnames), [path_concat/3]).

common_directory(TmpDir) :-
	get_tmp_dir(TmpDir). % Directory where address files are saved

module_to_addressfile(Mod, AddrPath) :-
        atom_concat(Mod,'_address',AddrFile),
        common_directory(Dir),
        path_concat(Dir, AddrFile, AddrPath).
