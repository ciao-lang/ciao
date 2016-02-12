:- module(usagedoc_modes, [
	], [assertions, doccomments]).

%! @title Assertions for Usage vs. Documentation (modes)
%
%  @module Examples that show the difference between adding assertions
%    just for documentation and for. Execute @key{C-c T}.

%! @doinclude [prime1/1, prime2/1]

%! prime1(X): X is a prime number (no usage here)
% %  prime1(+): accepts nonvar input

% We want this:
% :- pred prime1(+X).
% But we must write it like:
:- pred prime1(X) : nonvar(X).
prime1(1).
prime1(3).

% %! prime2(?X): X is a prime number (and, *any* mode usage)
% %  prime2(+): accepts nonvar input

:- pred prime2(X).
% We want this:
% :- pred prime2(+).
% But we must write it like:
:- pred prime2(X) : nonvar(X).
prime2(1).
prime2(3).

:- export(test1/0).
%! test1/0: A test for `prime1/1`. Raises errors when analyzed.
test1 :- prime1(_).

:- export(test2/0).
%! test2/0: A test for `prime2/1`. Just shows warnings when analyzed.
test2 :- prime2(_).

%% Output:
%%
%% {Checking assertions of [...]/doccomments/examples/usagedoc_modes.pl
%% {ERROR (ctchecks_pred_messages): (lns 4-17) False assertion:
%% :- check calls prime1(A)
%%          : nonvar(A).
%% 
%% because 
%% on calls usagedoc_modes:prime1(X) :
%% 
%% [shfr] : native_props:mshare([[X]]),term_typing:var(X) 
%% }
%% {WARNING (ctchecks_pred_messages): (lns 21-28) Cannot verify assertion:
%% :- check calls prime2(A)
%%          : ( true; nonvar(A) ).
%% 
%% because 
%% on calls usagedoc_modes:prime2(X) :
%% 
%% [eterms] : basic_props:term(X) 
%% 
%% [shfr]   : native_props:mshare([[X]]),term_typing:var(X) 
%%  
%% Left to prove: []
%% }
