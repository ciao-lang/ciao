:- module(assrt_norm, [], [assertions, basicmodes, regtypes, datafacts]).

:- doc(title, "Assertion normalization").

:- doc(module, "This module defines an instance of the assertion
   normalization (@lib{library(assertions/assrt_norm_common)})
   connected with the program databases from @tt{p_unit}").

% TODO: review bug list, see assrt_lib.pl
:- doc(bug,"1. Check that conjunctions and disjunctions in bodies of
   assertions are properly normalized.").
:- doc(bug,"2. Check that modes are properly normalized").
%:- doc(bug,"3. Have to manipulate : explicitly for qualification.
%   Otherwise, we get :(reference(_),native_props,regtype) instead
%   of native_props:regtype(reference(_)).").
%:- doc(bug,"4. p(+list(character_code)) goes to list(character_code,X).").
:- doc(bug,"5. Allow nonground terms with abridged syntax. E.g.:
            :- entry p(f(X,Y),a,Z): ground * var * var .").
:- doc(bug,"6. Have a look to modes. See: user:call/2 used in an 
   assertion for user:+/2 is not a property.").

% ===========================================================================
% Assertion normalization

:- use_module(library(formulae), [asbody_to_conj/2]).
:- use_module(library(assertions/assrt_lib), [prop_apply/3]).
:- use_module(library(assertions/assrt_lib), [assertion_body/7]).

:- use_module(library(compiler/c_itf), [clause_of/7]).
:- use_module(library(compiler/p_unit/assrt_db), [assertion_of/9]).

:- export(normalize_assertions/3).
:- export(normalize_assertion/9).
:- export(norm_goal_prop/3). 
:- export(denorm_goal_prop/3).
:- include(library(assertions/assrt_norm_common)).
% (see assrt_norm_common.pl for details on hooks)

% (hook)
ignore_norm_props(modedef). % modedefs already transformed -- leave as is 
ignore_norm_props(test). % TODO: why?

% (hook)
pass_one_cleanup(_). % TODO: ignored in ciaopp, why?

% (hook)
pass_two_not_required(modedef). %% modedefs already transformed in pass one -- leave as is
%pass_two_not_required(test).    %% tests do not require transformation

% (hook)
pass_two_check_body(_,_,_,_,_,_,_,_). % TODO: never here? why? (JF)
% Orig comment:
%   check_body_properties moved somewhere else
%   since it can not be done here: there is no guarantee that properties
%   have been already read in

% (hook)
get_source_assrt(Base,Assrt,Dict,S,LB,LE) :-
    %% Lets try to do a more clever search % TODO: why? (JF) this is not done in assrt_lib.pl
    clause_of(Base, 1, new_declaration(AssrtF/AssrtA), _, _, _, _),
    functor(Assrt, AssrtF, AssrtA),
    %% Normalize all assertions in this module
    clause_of(Base,1,Assrt,Dict,S,LB,LE).

% (hook)
add_assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE) :-
    assertz_fact(assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE)).

% (hook)
get_assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE) :-
    current_fact(assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE)).

% (hook)
del_assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE) :-
    retract_fact(assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE)).

% TODO: old commented code, remove?

% DTM: There is an error => no so clear!

% :- use_module(library(aggregates)).
% normalize_status_and_type(Ass,AssrtStatus,AssrtType,_UBody) :- 
%       Ass  =.. [AssrtType,AssrtStatus|_],
%       (
%           assrt_type(AssrtType)
%       ->
%           (
%               assrt_status( AssrtStatus )
%           ->
%               true
%           ;
%               findall( X , assrt_status( X ), L ),
%               error_message( "In assertion ~p: ~w is not valid status. "||
%                              "Expected one of ~w." ,
%                          [Ass, AssrtStatus, L] )
%           )
%       ;
%           findall( X , assrt_type( X ), L ),
%           error_message( "In assertion ~q: ~w is not a valid type. "||
%                          "Expected one of ~w." ,
%                          [Ass, AssrtType, L] )
%       ),
%       fail.

% (hook)
% TODO: different in assrt_lib.pl
norm_normal_props(A, B) :- asbody_to_conj(B, A).

% TODO: old code, remove?
% % norm_normal_props( A , B ) :-   
% %       conj_to_list_of_list( A , B , [] ).
% 
% % conj_to_list_of_list( (A,B) ,  Ac  , TAc ) :-
% %       !,
% %       conj_to_list_of_list( A , Ac , T   ),
% %       conj_to_list_of_list( B , T  , TAc ).
% 
% % conj_to_list_of_list( (A;B) , [ [ AC , BC ] | T ] , T ) :-
% %       !,
% %       conj_to_list( A , AC ),
% %       conj_to_list( B , BC ).
% 
% % conj_to_list_of_list( A , [ A | T ] , T ).
% 
% % conj_to_list( (A,B) , [A|Bs] ) :-
% %       !,
% %       conj_to_list( B , Bs ).
% % conj_to_list( A , [A] ).

