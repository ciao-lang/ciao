:- module(c_itf_props,[ moddesc/1, filename/1, switch/1 ],
	[ assertions, regtypes ]).

:- doc(title,"Some types and properties related to c_itf").

:- doc(author,"Francisco Bueno").

% ---------------------------------------------------------------------------
:- prop moddesc(X) + regtype # "@var{X} is a module descriptor.".
% ---------------------------------------------------------------------------

moddesc(X)       :- atm(X).
moddesc(user(X)) :- atm(X).

% ---------------------------------------------------------------------------
:- regtype filename(X) 
   # "@var{X} is an atom describing the name of a file.".
% ---------------------------------------------------------------------------

filename(X) :- 
	atm(X).

% ---------------------------------------------------------------------------
:- regtype switch(X) 
   # "@var{X} is an atom indicating yes/no.".
% ---------------------------------------------------------------------------

switch(yes).
switch(no).
