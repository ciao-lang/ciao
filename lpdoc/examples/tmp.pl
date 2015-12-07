%% %% The module headers produce documentation on the module interface 
%% %% Exported predicates (+ properties and types) are documented by default
%% :- module(example_module,
%%           [bar/1,baz/1,aorb/1,listof_or_aorb/2,list/1,q/1,q/2,r/1,
%%            og/1,t/5,u/3,w/1,p/5,long/1], 

:- use_package([assertions,basicmodes,fsyntax,regtypes]).  
 
%% We declare the type of file (needed for 'user'-type files)
:- doc(filetype,user).

:- doc(doinclude, [op/3, new_declaration/2]).

%% We import a number of properties to use in assertions
:- use_module(library(basicprops)).

%% We import two types: list/1 and list/2 (now in basic_props, which is 
%% exported by default from assertions). 
%% :- use_module(library(lists),[list/1,list/2]).

:- use_module(bar).
:- ensure_loaded(foo).

%% "commment" declarations provide additional information
:- doc(title,"tmp-lpdoc Output for the Example").  

:- doc(author,"Alan Robinson").  
:- doc(author,"David H.D. Warren").  

:- doc(summary,"This is a brief summary description of the module
   or file. In this case the file is a library.").
 
:- doc(module,"This is where general comments on the file go. In
   this case the file is a library which contains some assertion examples
   for testing the @em{automatic documentation system}. ").

%% An example of a comment documenting a bug
:- doc(bug,"Library is hard to execute: no actual code!").

%% Standard declarations are documented with the corresponding predicate
:- data r/1.
:- dynamic q/2.
:- multifile p/3.
:- dynamic p/3.
:- meta_predicate p(?,:,?).

%% This is a type definition in Prolog syntax: declaration and code
:- true regtype bar(X) # "@var{X} is an acceptable kind of bar.".

bar(night).
bar(day).

%% This is another type definition in Prolog syntax, with no comment.
:- true regtype baz/1.

baz(a).
baz(b).

%% Two type definitions in 'typedef' syntax (will be expanded to code as above)
%% :- typedef aorb ::= ^a;^b.
%% :- typedef listof_or_aorb(X) ::= list(X);aorb.

%% Using functional notation:
:- regtype aorb/1. 

aorb := a. 
aorb := b. 

:- regtype listof_or_aorb/2.

listof_or_aorb(X) := ~list(X).
listof_or_aorb(_) := ~aorb.

%% This is a property definition
%% This comment appears only in the place where the property is itself 
%% is documented. 
:- doc(long/1,"This is a property, describing a list that is longish. 
   The definition is: 

   @includedef{long/1}

   ").

%% The comment here will be used to document any predicate which has an 
%% assertion which uses the property 
:- prop long(L) # "@var{L} is rather long.".

long(L) :- 
	length(L,N),
	N>100.

%% Now, a series of assertions:
%% 
%% This declares the entry mode of this exported predicate (i.e., 
%% how it is called from outside).
:- entry p/3 : gnd * var * var.

%% This describes all the calls
:- calls p/3 : foo * bar * baz.

%% This describes the successes (for a given type of calls)
:- success p/3 : int * int * var => int * int * gnd.

%% This describes a global property (for a given type of calls)
:- comp p/3 : int * int * var + no_fail.

:- doc(p/3,"A @bf{general comment} on the predicate." ).
%% Documenting some typical usages of the predicate
:- pred p/3 
        : int * int * var 
       => int * int * list 
        + (iso,no_fail) 
        # "This mode is nice.".
:- pred p(Preds,Value,Assoc) 
        : var * var * list
       => int * int * list 
        + no_fail # "This mode is also nice.".
:- pred p/3 
       => list * int * list 
        + (no_fail,no_fail) 
        # "Just playing around.".

:- pred q(A) 
        : list(A) 
       => (list(A),gnd(A)) 
        + no_fail
        # "Foo".
:- pred q(A) 
        # "Not a bad use at all.".

:- pred q/2 
        : var * {gnd,int} 
       => {gnd,int} * int.
:- pred q/2 
        :: int * list
        # "Non-moded types are best used this way.".

:- pred p/1 : var => list.
 
:- pred r(A) 
        : list(A) 
       => (list(A,int),gnd(A)) 
        + no_fail 
        # "This uses parametric types".

:- doc(doinclude,s/1). %% Forces documentation even if not exported
:- pred s(A) 
        : list(A) 
       => (list(A),gnd(A)) 
        + no_fail.

:- doc(doinclude,list/2). %% Forces (local) documentation even if 
                              %% not exported 
:- doc(doinclude,list/1). %% Forces local documentation 

:- modedef og(A) 
       => gnd(A) 
        # "This is a @em{mode} definition: the output is ground.".

:- doc(doinclude,og/2).

:- modedef og(A,T) 
        :: regtype(A,T) 
        => gnd(A) 
        #  "This is a @em{parametric mode definition}.".

:- pred t(+A,-B,?C,@D,og(E)) 
        :: list * list * int * int * list 
        :  long(B)
        => (gnd(C),gnd(A)) 
        +  no_fail 
        #  "This predicate uses @em{modes} extensively.".

%% Some other miscellaneous assertions:

%% Check is default assertion status anyway...
:- check pred u(+,-,og).
:- check pred u(int,list(mytype),int).

%% ``true'' status is normally compiler output
:- true pred w(+list(mytype)).

:- doc(doinclude,is/2).

:- trust pred is(Num,Expr) : arithexpression(Expr) => num(Num)
   # "Typical way to describe/document an external predicate (e.g.,
      written in C).".

:- doc(doinclude,p/5).
:- pred p(og(int),in,@list(int),-,+A) + cost(1+length(A)).

%% Version information. The ciao.el emacs mode allows automatic maintenance
