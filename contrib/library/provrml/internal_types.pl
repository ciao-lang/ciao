:- module(internal_types,
	[bound/1,
	 bound_double/1,
	 dictionary/1,
	 environment/1,
	 parse/1,
	 tree/1,
	 whitespace/1],[assertions,isomodes,regtypes]).

:- doc(author, "G@..{o}ran Smedb@..{a}ck").

%:- use_module(library(basicprops)).


:- doc(module,"These are the internal data types used in the predicates.
They are only used to simplify this documentation and make it more
understandable.


   Implemented by G@..{o}ran Smedb@..{a}ck
").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-  true regtype bound(Bound)
	# "@var{Bound} is a variable interval.".

:- doc(bound/1,"Min is a number or an atom that indicates the 
           minimal value, Max indicates the maximal.
           @includedef{bound/1}"). 

bound(bound(Min,Max)) :-
	atm(Min),
	atm(Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-  true regtype bound_double(Bound)
	# "@var{Bound} is a variable interval.".

:- doc(bound_double/1,"Min is a number or an atom that indicates the 
           minimal value, Max indicates the maximal. The first two for some
           value and the second pair for some other. Typically used for types
           that are compound, e.g., rotationvalue.
           @includedef{bound_double/1}"). 

bound_double(bound(Min0,Max0,Min1,Max1)) :-
	atm(Min0),
	atm(Max0),
	atm(Min1),
	atm(Max1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-  true regtype dictionary(Dictionary) 
	# "@var{Dictionary} is a dictionary.".

:- doc(dictionary/1,"Dic is a tree structure and is used as the 
           internal representation of the dictionary.
           @includedef{dictionary/1}").

dictionary(dic(Dic)) :-
	tree(Dic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- true regtype tree(Tree)
	# "@var{Tree} is a tree structure.".

:- doc(tree/1,"Key is the search-key, Leaf is the information, 
                    Left and Right are more dictionary posts, 
                    where Left have less Key-value.
                    @includedef{tree/1}").

tree(tree(Key,Leaf,Left,Right)) :-
	atm(Key),
	leaf(Leaf),
	tree(Left),
	tree(Right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-  true regtype leaf(Leaf)
	# "@var{Leaf} is a leaf structure.".

:- doc(leaf/1,"Key is the search-key, Type an identifier for different 
                   types of elements in our case 'DEF','PROTO' and 
                   'EXTERNPROTO. Info is the information for the element
                   and Leaf is a dynamic place for equal key inserted, they
                   might be of different types.
                   @includedef{leaf/1}").

leaf(leaf(Key,Type,Info,MoreLeafs)) :-
	atm(Key),
	atm(Type),
	term(Info),
	leaf(MoreLeafs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-  true regtype parse(Parse)
	# "@var{Parse} is a parse structure.".

:- doc(parse/1,"In is the list of tokens to parse and Out is the 
                    resulting list after the parsing. Env is of type
                    env and is the environment-structure.The dictinonary 
                    Dic contains created information and structures.
                    @includedef{parse/1}").

parse(parse(In,Out,Env,Dic)) :-
	list(In),
	list(Out),
	environment(Env),
	dictionary(Dic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- true  regtype environment(Environment)
	# "@var{Environment} is an environment structure.".

:- doc(environment/1,"EnvironmentType one of 'DEF','PROTO','EXTERNPROTO' 
                          with the name Name. Whitespace is a structure with
                          whitespace information.
                          @includedef{environment/1}").

environment(env(Env,Name,WhiteSpace)) :-
	atm(Env),
	atm(Name),
	whitespace(WhiteSpace).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- true regtype whitespace(Whitespace)
	# "@var{Whitespace} is a whitespace structure.".

:- doc(whitespace/1,"The Row and Indentation information. The row 
                         information used when parsing the VRML code to
                         give accurate error position and the indentation
          		 is used when generating VRML code from terms.
                         @includedef{whitespace/1}").

whitespace(w(Row,Indentation)) :-
	number(Row),
	number(Indentation).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- true regtype stream(X) # "@var{X} is a stream for input/output.".

%stream(_).
