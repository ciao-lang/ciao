:- module(provrml,[vrml_web_to_terms/2,
	                 vrml_file_to_terms/2,
			 vrml_web_to_terms_file/2,
			 vrml_file_to_terms_file/2,
			 terms_file_to_vrml/2,
			 terms_file_to_vrml_file/2,
			 terms_to_vrml_file/2,
			 terms_to_vrml/2,
			 vrml_to_terms/2,
			 vrml_in_out/2,
			 vrml_http_access/2 ],
			 [assertions,
			  isomodes,
			  regtypes]).


:- doc(title,"ProVRML - a Prolog interface for VRML").

:- doc(author, "G@..{o}ran Smedb@..{a}ck").
:- doc(author, "Manuel Carro (some changes)").
:- doc(author, "The CLIP Group").

:- doc(copyright,"
Copyright @copyright{} 1998-2002 The Clip Group.

@include{DocCopyright.lpdoc}
").

:- doc(module,"@apl{ProVRML} is Prolog library to handle VRML code.
   The library consists of modules to  handle the tokenising, that is 
   breaking the VRML code into smaller parts that can be analysed further.
   The further analysis will be the parsing. This is a complex part of the 
   library and consists of several modules to handle errors and value check.
   When the parsing is done we have the Prolog terms of the VRML code. The 
   terms are quite similar to the origin VRML code and can easily be read
   if you recognise that syntax.

   This Prolog terms of the VRML code is then possible to use for
analysis, reconstruction, reverse enginering, building blocks for
automatic generation of VRML code. There are several possibilities and
these are only some of them.

   When you are done with the Prolog terms for the code, you would probably
   want to reverse the action and return to VRML code. This is done with 
   the code generation modules. These are built up in more or less the same 
   manner as the parser modules. 
").


:- use_package(pillow).

%:- use_module(library(basicprops)).
:- use_module(library(provrml/provrml_io), 
        [read_terms_file/2,
         read_vrml_file/2,
         write_terms_file/2,
         write_vrml_file/2]).
:- use_module(library(provrml/provrml_parser), 
        [parser/2]).
:- use_module(library(provrml/generator), 
        [generator/2]).

:- set_prolog_flag(write_strings,on).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% :- doc(module, "This file implements various predicates involving VRML.
 %%      
 %%     
 %%     Implemented by G@..{o}ran Smedb@..{a}ck
 %% 
 %% ").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred vrml_web_to_terms(+WEBAddress,-Terms)
   :: atm * string
   #
   "Given a address to a VRML-document on the Internet, the predicate
    will return the prolog-terms.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vrml_web_to_terms(Address, Terms) :-
	read_page(Address, VRML),
	parser(VRML, Terms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred vrml_web_to_terms_file(+WEBAddress,+FileName)
   :: atm * atm
   #
   "Given a address to a VRML-document on the Internet and a filename, 
    the predicate will write the prolog_terms to the file.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
vrml_web_to_terms_file(Address, FileTerms) :-
	vrml_web_to_terms(Address, Terms),
	write_terms_file(FileTerms, Terms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- discontiguous vrml_file_to_terms/2.

:- pred vrml_file_to_terms(+FileName,-Term)
   :: atm * atm
   #
   "Given a filename containing a VRML-file the predicate returns 
    the prolog terms corresponding.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vrml_file_to_terms(File, Terms) :-
	read_vrml_file(File, VRML),
	parser(VRML, Terms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred vrml_file_to_terms(+FileName,+Terms)
   :: atm * atm
   #
   "Given a filename containing a VRML-file and a filename, the 
    predicate write the prolog terms corresponding to the filename.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vrml_file_to_terms_file(FileVRML, FileTerms) :-
	vrml_file_to_terms(FileVRML, Terms),
	write_terms_file(FileTerms, Terms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred terms_file_to_vrml(+FileName, -List)
   :: atm * string
   #
   "From a given filename with prologterms on the special format,
    the predicate returns the corresponding VRML-code.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terms_file_to_vrml(File, VRML) :-
	read_terms_file(File, T),
	generator(T, VRML).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred terms_file_to_vrml_file(+Atom, +Atom)
   :: atm * atm
   #
   "From a given filename with prologterms on the special format,
    the predicate writes the corresponding VRML-code to second filename.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terms_file_to_vrml_file(TermFile, VRMLFile) :-
	read_terms_file(TermFile, T),
	generator(T, VRML),
	write_vrml_file(VRMLFile, VRML).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred terms_to_vrml_file(+Term, +FileName)
   :: atm * atm
   #
   "Given prolog-terms the predicate writes the corresponding 
    VRML-code to the given file.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terms_to_vrml_file(Terms, FileOut) :-
	generator(Terms, VRML),
	write_vrml_file(FileOut, VRML).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred terms_to_vrml(+Term, -VRMLCode)
   :: atm * string
   #
   "Given prolog-terms the predicate returns a list with the
    corresponding VRML-code.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terms_to_vrml(Terms, VRML) :-
	generator(Terms, VRML).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred vrml_to_terms(+VRMLCode, -Terms)
   :: string * atm
   #
   "Given a list with VRML-code the predicate will return the 
    corresponding prolog-terms.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vrml_to_terms(VRML, Terms) :-
	parser(VRML, Terms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred vrml_in_out(+FileName, +FileName)
   :: atm * atm
   #
   "This is a controll-predicate that given a filename to a VRML-file
    and a filename, the predicate will read the VRML-code. Transform
    it to prolog-terms and then transform it back to VRRML-code and 
    write it to the latter file.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vrml_in_out(FileIn, FileOut) :-
    vrml_file_to_terms(FileIn, Terms),
    !,
    atom_concat(FileOut,'.term',TermName),
    write_terms_file(TermName, Terms),
    terms_to_vrml(Terms, VRML),
    write_vrml_file(FileOut, VRML).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred vrml_http_access(+ReadFilename, +BaseFilename)
   :: atm * atm
   #
   "Given a web-address to a VRML-file the predicate will load the 
    code, write it first to the second argument with extension '_first.wrl'.
    Then it transform the code to prolog terms and write it with the 
    extension '.term'. Transform it back to VRML-code and write it
    to the filename with '.wrl. A good test-predicate.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vrml_http_access(Address, FileOutBase) :-
	read_page(Address, VRML),
	atom_concat(FileOutBase,'_first.wrl',VRMLName),
	write_vrml_file(VRMLName, VRML),
	parser(VRML, Terms),
	!,
	atom_concat(FileOutBase,'.term',TermName),
	write_terms_file(TermName, Terms),
	terms_to_vrml(Terms, VRML1),
	atom_concat(FileOutBase,'.wrl',FileOut),
	write_vrml_file(FileOut, VRML1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(doinclude, read_page/2).
:- pred read_page(+WEBAddress, -Data)
   :: atm * string
   #
   "This routine reads a page on the web using pillow routines.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_page(File, Content) :-
        url_info(File, UI),
        fetch_url(UI,[],Response),
	member(content(Content),Response).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     

%% member(X, [X|_]).
%% member(X, [_|More]) :-
%% 	member(X,More).
