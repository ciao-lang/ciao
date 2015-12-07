% *** entry ***
% call after bs, before nonlin
%

simplex_reconsider( 0, _).
simplex_reconsider( 1, Eqs) :-
  get_attribute( Eqs, p(_,De,_,_,_)),
  inf_rows( De, Vars),
  reconsider( Vars, De).

inf_rows( De,     []) :- var( De), !.
inf_rows( [D|De], Res) :-
  ( get_attribute( D, eqn_var(_,l(_),Lin,_,_)),
    get_attribute( Lin, I+_),
    arith_eval( I >= 0) ->  				% maybe infeasible
      Res = [D|R],
      inf_rows( De, R)
  ;
      inf_rows( De, Res)
  ).

reconsider( [],     _).
reconsider( [V|Vs], _) :-
  ( get_attribute( V, eqn_var(_,_,_,Ref,_)) ->
      get_attribute( Ref, p(_,De,_,_,_)),
      ph1( V, De, State),
      ( State = stop					% eq, will callback
      ; State = go,  reconsider( Vs, De)
      )
  ;
      throw(clp_error(illegal_simplex_var))
  ).

% *** entry ***
%
solve_ineq_le( [], I) :- !, arith_eval( I =< 0).
solve_ineq_le( H,  I) :-
  var_with_def( _Var, l(e), 1, I, H).			% callback ph1

%
solve_ineq_lt( [], I) :- !, arith_eval( I < 0).
solve_ineq_lt( H,  I) :-
  var_with_def( _Var, l(t), 1, I, H). 			% callback ph1

% ------------------------------ optimization ---------------------------

% *** entry ***
%
maximize( Lin) :-  sup( Lin, Lin).
%
minimize( Lin) :-  inf( Lin, Lin).

inf( Lin, Inf) :-
  normalize( Lin, Kl, Il, Hl),
  ( Hl = [] ->
      arith_eval( Kl*Il, Inf)
  ; get_attribute( Lin, term_wrap(_,_)) ->
      detach_attribute( Lin),
      var_with_def( Lin, v, Kl, Il, Hl),
      get_attribute( Lin, eqn_var(_,_,_,Ref,_)),
      get_attribute( Ref, p(_,De,_,_,_)),
      ph2_inf( Lin, De, Inf)
  ;
      get_attribute( Lin, eqn_var(_,_,_,Ref,_)),
      get_attribute( Ref, p(_,De,_,_,_)),
      ph2_inf( Lin, De, Inf)
  ).

sup( Lin, Sup) :-
  normalize( Lin, Kl, Il, Hl),
  ( Hl = [] ->
      arith_eval( Kl*Il, Sup)
  ; get_attribute( Lin, term_wrap(_,_)) ->
      detach_attribute( Lin),
      var_with_def( Lin, v, Kl, Il, Hl),
      get_attribute( Lin, eqn_var(_,_,_,Ref,_)),
      get_attribute( Ref, p(_,De,_,_,_)),
      ph2_sup( Lin, De, Sup)
  ;
      get_attribute( Lin, eqn_var(_,_,_,Ref,_)),
      get_attribute( Ref, p(_,De,_,_,_)),
      ph2_sup( Lin, De, Sup)
  ).

ph2_inf( Var, De0, Inf) :-
  get_attribute( Var, eqn_var(_,_,Lin,_,_)),
  get_attribute( Lin, I+H),
  ph2_dec( H, De0, De1, Stat),
  ( Stat = go,   ph2_inf( Var, De1, Inf)
  ; Stat = stop, Inf = I
  ).

ph2_sup( Var, De0, Sup) :-
  get_attribute( Var, eqn_var(_,_,Lin,_,_)),
  get_attribute( Lin, I+H),
  ph2_inc( H, De0, De1, Stat),
  ( Stat = go,   ph2_sup( Var, De1, Sup)
  ; Stat = stop, Sup = I
  ).

/*
bounds( X, Inf, Sup) :-
  ( inf(X,Inf) -> true ; Inf = -infinity ),
  ( sup(X,Sup) -> true ; Sup =  infinity ).
*/
% ---------------------------------------------------------------------------

% decrement until I (=)< 0, i.e., feasible
%
ph1( Var, De0, Res) :-
  get_attribute( Var, eqn_var(_,T,Lin,_,_)),
  get_attribute( Lin, I+H),
  ( arith_eval( I < 0) ->
      Res = go
  ; arith_eval( I > 0) ->
      ph1_dec( H, Var, De0, De1),
      ph1( Var, De1, Res)
  ; ph2_dec( H, De0, De1, Stat) ->
      ( Stat = stop, T = l(e),   			% shortcut, solve_lin would fail
		     Res = stop,
                     solve_lin( H, 0)
      ; Stat = go,   ph1( Var, De1, Res)
      )
  ;
      Res = go
  ).

/*
all_neg_l( []          ).
all_neg_l( [Col*K|Cols]) :-
  arith_eval( K < 0),
  get_attribute( Col, eqn_var(_,l(_),_,_,_)),
  all_neg_l( Cols).
*/

% the trick: use the infeasible row as default candidate
% for the exchange
% This is guarded by I > 0 -> Inf is never indep in this loop
%
ph1_dec( [Col*K|Cols], Inf, De0, De1) :-
  ( arith_eval( K < 0) ->   				% need pos
     ( get_attribute( Col, eqn_var(_,l(_),_,_,_)) ->
         ph1_dec( Cols, Inf, De0, De1)
     ;
         get_attribute( Inf, eqn_var(_,_,Lin,_,_)),
         get_attribute( Lin, I+_),
         arith_eval( -I/K, Intro), 			% if we take the infeasible row ..
         min_pos( De0, Col, Inf, Row, Intro),		% never fails
         swap( Row, Col, De1)
     )
  ;         						% need neg
     get_attribute( Inf, eqn_var(_,_,Lin,_,_)),
     get_attribute( Lin, I+_),
     arith_eval( -I/K, Intro),
     max_neg( De0, Col, Inf, Row, Intro),		% never fails
     swap( Row, Col, De1)
  ).

ph2_dec( [],           De,  De,  stop).
ph2_dec( [Col*K|Cols], De0, De1, Res) :-
  ( arith_eval( K < 0) ->
     ( get_attribute( Col, eqn_var(_,l(_),_,_,_)) ->  	% cannot get > 0, try next
         ph2_dec( Cols, De0, De1, Res)
     ;
         Res = go,
         min_pos( De0, Col, Row),
         swap( Row, Col, De1)
     )
  ;
     Res = go,
     max_neg( De0, Col, Row),
     swap( Row, Col, De1)
  ).

ph2_inc( [],           De,  De,  stop).
ph2_inc( [Col*K|Cols], De0, De1, Res) :-
  ( arith_eval( K > 0) ->
     ( get_attribute( Col, eqn_var(_,l(_),_,_,_)) ->   	% cannot get > 0, try next
         ph2_inc( Cols, De0, De1, Res)
     ;
         Res = go,
         min_pos( De0, Col, Row),
         swap( Row, Col, De1)
     )
  ;
     Res = go,
     max_neg( De0, Col, Row),
     swap( Row, Col, De1)
  ).

min_pos( De,     _,   _) :- var( De), !, fail.
min_pos( [D|De], Col, M) :-
  ( get_attribute( D, eqn_var(_,l(_),Lin,_,_)),
    get_attribute( Lin, I+H),
    arith_eval( I =< 0),				% feasible ?
    nf_coeff_of( H, Col, K),
    arith_eval( K > 0),		       			% feasible for exchange
    arith_eval( -I/K, Mv0) ->
      min_pos( De, Col, D, M, Mv0)
  ;
      min_pos( De, Col, M)
  ).

min_pos( De,     _,   M,  M, _) :- var( De), !.
min_pos( [D|De], Col, M0, M, Mv0) :-
  ( get_attribute( D, eqn_var(_,l(_),Lin,_,_)),
    get_attribute( Lin, I+H),
    arith_eval( I =< 0),				% feasible ?
    nf_coeff_of( H, Col, K),
    arith_eval( K > 0),     				% feasible for exchange
    arith_eval( -I/K, Mv1),
    arith_eval( Mv1 < Mv0) ->				% tighter
      ( arith_zero( Mv1) -> 				% tightest
          M = D
      ;
          min_pos( De, Col, D,  M, Mv1)
      )
  ;
      min_pos( De, Col, M0, M, Mv0)
  ).

max_neg( De,     _,   _) :- var( De), !, fail.
max_neg( [D|De], Col, M) :-
  ( get_attribute( D, eqn_var(_,l(_),Lin,_,_)),
    get_attribute( Lin, I+H),
    arith_eval( I =< 0),				% feasible ?
    nf_coeff_of( H, Col, K),
    arith_eval( K < 0),       				% feasible for exchange
    arith_eval( -I/K, Mv0) ->
      max_neg( De, Col, D, M, Mv0)
  ;
      max_neg( De, Col, M)
  ).

max_neg( De,     _,   M,  M, _) :- var( De), !.
max_neg( [D|De], Col, M0, M, Mv0) :-
  ( get_attribute( D, eqn_var(_,l(_),Lin,_,_)),
    get_attribute( Lin, I+H),
    arith_eval( I =< 0),				% feasible ?
    nf_coeff_of( H, Col, K),
    arith_eval( K < 0),     				% feasible for exchange
    arith_eval( -I/K, Mv1),
    arith_eval( Mv1 > Mv0) ->				% tighter
      ( arith_zero( Mv1) -> 				% tightest
          M = D
      ;
          max_neg( De, Col, D,  M, Mv1)
      )
  ;
      max_neg( De, Col, M0, M, Mv0)
  ).


%
% This assumes that the system is in simplex nf
%
remove_redundancy( Eqs) :-
  get_attribute( Eqs, p(_,De,_,_,_)),
  narrow_dep( De),
  get_attribute( Eqs, p(_,_,_,In,_)),
  narrow_indep( In, Eqs).

%
% Changing the id of an indep. var is a mess ...
%
% This iterator assumes that Indep of Eqs does not change
% in the course of the execution
%
narrow_indep( Indep,     _) :- var( Indep), !.
narrow_indep( [I|Indep], Eqs) :-
  ( get_attribute( I, eqn_var(_,l(T),_,_,Nl)),
    attach_attribute( New, eqn_var(New,v,Lin,Eqs,Nl)),
    attach_attribute( Lin, 0+[New*1]),
    get_attribute( Eqs, p(_,De,_,_,_)),
    swap_bs( De, I, 0, [New*1]),				% need id of I in this pass
    detach_attribute( I),
    I = New,
    other_type( T, Ot),
    \+ var_with_def( _, Ot, 1, 0, [New* -1]) -> 		% calls ph1
      								% l(_) on I was redundant
       narrow_indep( Indep, Eqs)				% iterate
  ;                                                     	% l(_) on I is not redundant
       narrow_indep( Indep, Eqs)				% iterate
  ).

narrow_dep( De) :- var( De), !.
narrow_dep( [D|De]) :-
  ( get_attribute( D, eqn_var(_,l(T),Lin,Ref,Nl)) ->
      ( \+ simplex_non_redundant( T, D, Lin, Ref, Nl) ->	% \+ is for encapsulation
          detach_attribute( D)					% redundant, remove
      ;
          true							% nonredundant
      ),
      narrow_dep( De)
  ;
      narrow_dep( De)
  ).

simplex_non_redundant( Type, Var, Lin, Ref, Nl) :-
  other_type( Type, OType),
  get_attribute( Lin, I+H),
  arith_eval( -I, I1),
  mult_linear_factor( H, -1, H1),
  attach_attribute( Lin1, I1+H1),
  update_attribute( Var, eqn_var(Var,OType,Lin1,Ref,Nl)),
  get_attribute( Ref, p(_,De,_,_,_)),
  ph1( Var, De, _).

other_type( e, l(t)).
other_type( t, l(e)).
