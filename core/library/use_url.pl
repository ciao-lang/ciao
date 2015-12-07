:- module(use_url,
	[use_module_url/2,use_module_url/3,use_module_url/4],
	[assertions,pillow]).
:- use_module(library(compiler), [use_module/3]).
:- use_module(library(system), 
	[delete_directory/1,delete_file/1,directory_files/2,
	 make_directory/1,mktemp_in_tmp/2]). 

:- true pred use_module_url(Module, Url) : sourcename * string
        # "Imports from module @var{Module} all the predicates exported 
          by it into the calling module. @var{Module} is loaded from
          @var{Url}. If @var{Url} is not accesible, it fails.".

:- meta_predicate use_module_url(?,addmodule).

use_module_url(Mod,Url,This):- use_module_url(Mod,Url,all,This).

:- true pred use_module_url(Module, Url, Imports) 
	: sourcename * string * list(predname)
        # "Imports from module @var{Module} the predicates in @var{Imports}
          (if exported) into the calling module. @var{Module} is loaded from
          @var{Url}. If @var{Url} is not accesible, it fails.".

:- meta_predicate use_module_url(?,?,addmodule).

use_module_url(Mod,NameUrl,Imports,This):-
	url_info(NameUrl,Url),
	fetch_url(Url,[],Response),
	member(status(A,I,E),Response),
	( A = success
	; throw(use_url(status(A,I,E)))
	),
	member(content(Content),Response),
	mktemp_in_tmp('ciaoXXXXXX',TmpDir),
	atom_concat(TmpDir,Mod,TmpFile),
	make_directory(TmpDir),
	open(TmpFile,write,Stream),
	putall(Content,Stream),
	close(Stream),
	use_module(TmpFile,Imports,This),
	( directory_files(TmpDir,FileList),
	  delete_all_files(FileList,Mod,TmpDir),
	  delete_directory(TmpDir)
	; true
	).

putall([],_Stream).
putall([X|Xs],Stream):- put_code(Stream,X), putall(Xs,Stream).

delete_all_files([],_F,_D).
delete_all_files([File0|Files],TmpFile,TmpDir):-
	( atom_concat(TmpFile,_Any,File0)
	-> atom_concat(TmpDir,File0,File),
	   delete_file(File)
	 ; true
	),
	delete_all_files(Files,TmpFile,TmpDir).
