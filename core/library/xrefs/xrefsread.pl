
:- module(xrefsread,
        [ xrefs_modules/2,
          xrefs_files/1,
          set_files/1,
          set_flag/1,
	  meta_call/3
        ],
        [ assertions, regtypes
        ]).

:- use_module(library(aggregates), [findall/3, setof/3]).
:- use_module(library(compiler/c_itf)).
:- use_module(library(ctrlcclean), [ctrlc_clean/1]).
:- use_module(library(errhandle), [error_protect/1]).  
:- use_module(library(terms), [atom_concat/2]).  

:- doc(title,"Finding crossed-references between modules/files").
:- doc(subtitle_extra,"@bf{The CIAO System Documentation Series}").
:- doc(subtitle_extra,"@em{Draft printed on:} @today{}").
:- doc(author,"Francisco Bueno").
:- doc(module,"This library provides support for obtaining 
        crossed-references between modules (based on the module interface)
	or files (based on the source code, disregarding the module
	interface).").

:- doc(bug,"For the time being, @tt{meta_call/3} does nothing.").

%-----------------------------------------------------------------------------
% entry points

:- data the_files/1.

the_files([]).

:- doc(doinclude,sourcename/1).
:- doc(sourcename/1,"See @tt{engine(streams_basic)}.").

:- pred set_files(List_of_files) : list(sourcename)
        # "Sets the current files to @var{List_of_files}.".
:- pred set_files(List_of_files) : var
        # "Binds @var{List_of_files} to the current files.".

set_files(Files):-
        var(Files), !,
        the_files(Files).
set_files(Files):-
        retractall_fact(the_files(_)),
        asserta_fact(the_files(Files)).

:- doc(set_flag(Flag),"@var{Flag} encodes in binary the references
	wanted:
          1st bit- from current files, 3rd bit- from others,
          2nd bit- to current files,   4th bit- to others.
          Default flag is 3: from current files to current files.").
:- pred set_flag(Flag) : number
        # "Sets the current flag to @var{Flag}.".
:- pred set_flag(Flag) : var
        # "Binds @var{Flag} to the current flag.".

set_flag(Flag):-
        var(Flag), !,
        flag(FlagM,FlagIM),
        Flag is FlagM+FlagIM*2.
set_flag(Flag):-
        number(Flag),
        allowed(Flag,FlagM,FlagIM),
        retractall_fact(flag(_,_)),
        asserta_fact(flag(FlagM,FlagIM)).

:- data flag/2.

flag(1,1).

allowed(Flag,FlagM,FlagIM):-
        FlagM is Flag/\5,
        FlagIM is (Flag/\10)//2,
        allowed_flag(FlagM),
        allowed_flag(FlagIM).

allowed_flag(1).  % current file
allowed_flag(4).  % other file
allowed_flag(5).  % any

:- pred xrefs_modules(Lbl,Graph) : lblflag * var
	# "Obtains a graph of xrefs with (@tt{labels}) or
	   without (@tt{nolabels}) labels, based on the module interface
	   of the current files read.".

xrefs_modules(labels,Graph):-
	modules_graph(Graph).
xrefs_modules(nolabels,Graph):-
	modules_links(Graph).

:- regtype lblflag(F)
	# "@var{F} is either @tt{labels} or @tt{nolabels}.".

lblflag(labels).
lblflag(nolabels).

:- pred xrefs_files(CList) : var
	# "Returns a list of clauses of the files read.".

xrefs_files(CList):-
	xrefs_read(files),
	findall((Base,Clauses),
	        setof((H:-B),
	                clause_read(Base,H,B),
			Clauses),
		CList).

:- doc(meta_call(Atom,File,Call),"@var{Atom} is a meta-predicate in file
	@var{File} and meta-calls @var{Call}. If the meta-call cannot be
	identified as a well-formed body @var{Call} is bound to @tt{0}.").

meta_call(_,_,0):- fail.

%-----------------------------------------------------------------------------

xrefs_read(Flag):-
        retractall_fact(module_read(_)),
        retractall_fact(imported(_,_,_,_)),
        retractall_fact(clause_read(_,_,_)),
        cleanup_c_itf_data,
        the_files(Files),
        pass_one(Files,Flag).

pass_one([File|Files],Flag):-
        error_protect(ctrlc_clean(
                process_file(File, xrefs, any, 
                             recorda(Flag),
                             false, false, always)
                                 )),
        pass_one(Files,Flag).
pass_one([],_Flag).

always(_BaseName).

recorda(BaseName,modules):- !,
        defines_module(BaseName,M),
	recorda_imported(M),
	asserta_fact(module_read(M)).
recorda(BaseName,files):-
	recorda_clauses(BaseName).

recorda_clauses(BaseName):-
        clause_of(BaseName,H,B,_VNs,_Source,_LBegin,_LEnd),
	\+ number(H),
	asserta_fact(clause_read(BaseName,H,B)),
	fail.
recorda_clauses(_BaseName).

:- data clause_read/3.
:- data module_read/1.
:- data imported/4.

% unfortunately allowed_m cannot be used here
%   (not all module_read are available)
% also, passing the Files list does not work, either
%   (they may not conform the BaseName or Module format)
recorda_imported(Module):-
	imports(Module,Imported,F,A),
	asserta_fact(imported(Module,Imported,F,A)),
	fail.
recorda_imported(_Module).

%-----------------------------------------------------------------------------

modules_links(Graph):-
	xrefs_read(modules),
	flag(FlagM,FlagIM),
	findall((Module,Imported),
	        ( imported(Module,Imported,_F,_A),
		  accepted_m(FlagM,Module),
		  accepted_m(FlagIM,Imported)
		),
		Graph).

modules_graph(HGraph):-
	xrefs_read(modules),
        findall((Module,Labels), imported_from(Module, Labels), HGraph).

imported_from(Module, Labels) :-
        setof((L,Imported), imported_list(Module, L, Imported), Labels).

imported_list(Module, L, Imports) :-
        flag(FlagM,FlagIM),
        setof(Imported,
              ( imported(Module,Imported,F,A),
                accepted_m(FlagM,Module),
                accepted_m(FlagIM,Imported)
              ),
              Imports),
        name(A,N),
        atom_codes(At,N),
        atom_concat([F,/,At],L).

imports(Module,M,F,A):-
        imports_pred(ModuleBase,ImpFile,F,A,_DefType,_Meta,_EndFile),
        defines_module(ModuleBase,Module),
        base_name(ImpFile,ImpModuleBase),
        defines_module(ImpModuleBase,M).

%-----------------------------------------------------------------------------

accepted_m(1,M):-
        module_read(M).
accepted_m(4,M):-
        \+ module_read(M).
accepted_m(5,_M).
