:- module(usagedoc_types, [
	], [assertions, doccomments]).

%! @title Assertions for Usage vs. Documentation (types)
%
%  @module Examples that show the difference between adding assertions
%    just for documentation and for. Execute @key{C-c T}.
%
%  @bug   Does not really work with wiki sintax (see assertion of prime1)

%! @doinclude [prime1/1, prime2/1]

% %! prime1(X): X is a prime number (no usage here)
% %  prime1(+num): accepts bound numbers on call

:- doc(prime1(X), "@var{X} is a prime number (no usage here)").
% We want this:
% :- pred prime1(X) :: num(X) => nonvar(X).
% But we must write it like:
:- pred prime1(X) : num(X) => nonvar(X).
prime1(1).
prime1(3).
prime1(a).

% %! prime2(?X): X is a prime number (and, *any* mode usage)
% %  prime2(+num): accepts bound numbers on call

:- pred prime2(X).
% We want this:
% :- pred prime1(X) :: num(X) => nonvar(X).
% But we must write it like:
:- pred prime2(X) : num(X) => nonvar(X).
prime2(1).
prime2(3).
prime2(a).

:- export(test1/0).
%! test1/0: A test for `prime1/1`. Raises errors when analyzed.
test1 :- prime1(a).

:- export(test2/0).
%! test2/0: A test for `prime2/1`. Just shows warnings when analyzed.
test2 :- prime2(a). % Just a warning

%% {Checking assertions of [...]/doccomments/examples/usagedoc_types.pl
%% {ERROR (ctchecks_pred_messages): (lns 18-20) False assertion:
%% :- check calls prime1(A)
%%          : num(A).
%% 
%% because 
%% on calls usagedoc_types:prime1(X) :
%% 
%% [eterms] : rt0(X) 
%% with: rt0 ::= ^a
%% }
%% {WARNING (ctchecks_pred_messages): (lns 25-32) Cannot verify assertion:
%% :- check calls prime2(A)
%%          : ( true; num(A) ).
%% 
%% because 
%% on calls usagedoc_types:prime2(X) :
%% 
%% [eterms] : rt4(X) 
%% with: rt4 ::= ^a
%% 
%% [shfr]   : term_typing:ground([X]) 
%%  
%% Left to prove: []
%% }
