:- module(use_url, [
    use_module_url/2,use_module_url/3,use_module_url/4
], [assertions]).

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(compiler), [use_module/3]).
:- use_module(library(system), 
    [delete_directory/1,delete_file/1,directory_files/2,
     make_directory/1,mktemp_in_tmp/2]). 
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(engine(stream_basic), [sourcename/1]).

:- use_module(library(http_get), [http_get/2]).

:- pred use_module_url(Module, URL) : sourcename * string
    # "Imports from module @var{Module} all the predicates exported 
      by it into the calling module. @var{Module} is loaded from
      @var{URL}. If @var{URL} is not accesible, it fails.".

:- meta_predicate use_module_url(?,addmodule).

use_module_url(Mod,URL,This):- use_module_url(Mod,URL,all,This).

:- pred use_module_url(Module, URL, Imports) 
    : sourcename * string * list(predname)
    # "Imports from module @var{Module} the predicates in @var{Imports}
      (if exported) into the calling module. @var{Module} is loaded from
      @var{URL}. If @var{URL} is not accesible, it fails.".

:- meta_predicate use_module_url(?,?,addmodule).

use_module_url(Mod, URL, Imports, This):-
    mktemp_in_tmp('ciaoXXXXXX',TmpDir),
    del_file_nofail(TmpDir),
    path_concat(TmpDir,Mod,TmpFile),
    make_directory(TmpDir),
    %
    http_get(URL, file(TmpFile)),
    %
    use_module(TmpFile,Imports,This),
    %
    ( directory_files(TmpDir,FileList),
      delete_all_files(FileList,Mod,TmpDir),
      delete_directory(TmpDir)
    ; true
    ).

delete_all_files([],_F,_D).
delete_all_files([File0|Files],TmpFile,TmpDir):-
    ( atom_concat(TmpFile,_Any,File0) ->
        path_concat(TmpDir,File0,File),
        delete_file(File)
    ; true
    ),
    delete_all_files(Files,TmpFile,TmpDir).
