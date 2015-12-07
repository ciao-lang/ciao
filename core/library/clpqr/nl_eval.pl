%
% local propagation evaluator
% for nonlinear expressions in clp(r,q)
%

:- use_module(engine(internals), [term_to_meta/2]).

/*
arith_ground(X) :- 			% substitute for number(X)
  type(X,Xt), normalize(Xt, X, _, _, H),
  H = [].
*/
arith_ground(X, Val) :-
  type(X,Xt), normalize(Xt, X, K, I, H),
  H = [],
  arith_eval(K*I, Val).

:- meta_predicate delay_all(_,_,goal).

delay_all( yes, _, _).
delay_all( no, Vs, G) :- delay_all( Vs, G).

:- meta_predicate delay_all(_,goal).

delay_all( [], _).
delay_all( [V|Vs], G) :-
  system_freeze( V, G),
  delay_all( Vs, G).

% a = abs(b)
%
solve_abs( A, B) :-
  solve_abs( _Mutex, A, B, no).
%
solve_abs( Mux, A, B, Retry) :- var(Mux), !,
  ( arith_ground( A, Av) ->
      % arith_eval( A >= 0),
      ( arith_zero( Av) ->
          Mux = solved,
          solve_one( B, 0)
      ; arith_ground( B, Bv) ->   			% +a ?b
          Mux = solved,
          arith_eval( abs(Bv) =:= Av)
      ;                                       		% +a -b
          delay_all( Retry, [B], solve_abs(Mux,A,B,yes))
      )
  ; arith_ground( B, Bv) ->                   		% -a +b
      Mux = solved,
      arith_eval( abs(Bv), Val), solve_one( A, Val)
  ;                                                 	% -a -b
      delay_all( Retry, [A,B], solve_abs(Mux,A,B,yes))
  ).
solve_abs( _, _, _, _).

normalize_abs( B, 1, Inhom, Hom) :-
  type(B,Bt), normalize( Bt, B, Kb, Ib, Hb),
  ( Hb = [] ->                   			% -a +b
      arith_eval( abs(Kb*Ib), Inhom), Hom = []
  ;                                                 	% -a -b
      Inhom = 0, Hom = [VA*1],
      eqn_var_new( v, VA),
      var_with_def( VB, v, Kb, Ib, Hb),
      delay_all( [VA,VB], solve_abs(_Mux,VA,VB,yes))
  ).

% a = sign(b)
%
solve_sign( A, B) :-
  solve_sign( _Mutex, A, B, no).
%
solve_sign( Mux, A, B, Retry) :- var(Mux), !,
  ( arith_ground( A, Av) ->
      % arith_eval( A >= 0),
      ( arith_zero( Av) ->
          Mux = solved,
          solve_one( B, 0)
      ; arith_ground( B, Bv) ->   			% +a ?b
          Mux = solved,
          arith_eval( sign(Bv) =:= Av)
      ;                                       		% +a -b
          delay_all( Retry, [B], solve_sign(Mux,A,B,yes))
      )
  ; arith_ground( B, Bv) ->                   		% -a +b
      Mux = solved,
      arith_eval( sign(Bv), Val), solve_one( A, Val)
  ;                                                 	% -a -b
      delay_all( Retry, [A,B], solve_sign(Mux,A,B,yes))
  ).
solve_sign( _, _, _, _).

normalize_sign( B, 1, Inhom, Hom) :-
  type(B,Bt), normalize( Bt, B, Kb, Ib, Hb),
  ( Hb = [] ->                   			% -a +b
      arith_eval( sign(Kb*Ib), Inhom), Hom = []
  ;                                                 	% -a -b
      Inhom = 0, Hom = [VA*1],
      eqn_var_new( v, VA),
      var_with_def( VB, v, Kb, Ib, Hb),
      delay_all( [VA,VB], solve_sign(_Mux,VA,VB,yes))
  ).


% a = {min,max}(b,c)
%
solve_mix( MIX, A, B, C) :-
  solve_mix( _Mutex, MIX, A, B, C, no).
%
solve_mix( Mux, MIX, A, B, C, Retry) :- var( Mux), !,
  ( arith_ground( A, Av) ->
     ( arith_ground( B, Bv) ->
        ( arith_ground( C, Cv) ->   			% +a +b +c
	    Mux = solved,
 	    ( MIX = min, arith_eval( Av=:=min(Bv,Cv))
            ; MIX = max, arith_eval( Av=:=max(Bv,Cv))
            )
        ;                               		% +a +b -c
            delay_all( Retry, [C], solve_mix(Mux,MIX,A,B,C,yes))
        )
     ; arith_ground( C, Cv) ->                  	% +a -b +c
        delay_all( Retry, [B], solve_mix(Mux,MIX,A,B,C,yes))
     ;                                      		% +a -b -c
       delay_all( Retry, [B,C], solve_mix(Mux,MIX,A,B,C,yes))
     )
  ; arith_ground( B, Bv) ->
     ( arith_ground( C, Cv) ->      			% -a +b +c
         Mux = solved,
 	 ( MIX = min, arith_eval( min(Bv,Cv), Av)
         ; MIX = max, arith_eval( max(Bv,Cv), Av)
         ),
         solve_one( A, Av)
     ;                               			% -a +b -c
         delay_all( Retry, [A,C], solve_mix(Mux,MIX,A,B,C,yes))
     )
  ; arith_ground( C, Cv) ->    				% -a -b +c
      delay_all( Retry, [A,B], solve_mix(Mux,MIX,A,B,C,yes))
  ;                                        		% -a -b -c
      delay_all( Retry, [A,B,C], solve_mix(Mux,MIX,A,B,C,yes))
  ).
solve_mix( _, _, _, _, _, _).

normalize_mix( MIX, B, C, 1, Inhom, Hom) :-
  type(B,Bt), normalize( Bt, B, Kb, Ib, Hb),
  type(C,Ct), normalize( Ct, C, Kc, Ic, Hc),
  ( Hb = [] ->
     ( Hc = [] ->      					% -a +b +c
         ( MIX = min, arith_eval( min(Kb*Ib,Kc*Ic), Inhom)
         ; MIX = max, arith_eval( max(Kb*Ib,Kc*Ic), Inhom)
         ),
         Hom = []
     ;                               			% -a +b -c
         Inhom = 0, Hom = [VA*1],
         eqn_var_new( v, VA),
         arith_eval( Kb*Ib, Vb),
         var_with_def( VC, v, Kc, Ic, Hc),
         delay_all( [VA,VC], solve_mix(Mux,MIX,VA,Vb,VC,yes))
     )
  ; Hc = [] ->    					% -a -b +c
      Inhom = 0, Hom = [VA*1],
      eqn_var_new( v, VA),
      var_with_def( VB, v, Kb, Ib, Hb),
      arith_eval( Kc*Ic, Vc),
      delay_all( [VA,VB], solve_mix(Mux,MIX,VA,VB,Vc,yes))
  ;                                        		% -a -b -c
      Inhom = 0, Hom = [VA*1],
      eqn_var_new( v, VA),
      var_with_def( VB, v, Kb, Ib, Hb),
      var_with_def( VC, v, Kc, Ic, Hc),
      delay_all( [VA,VB,VC], solve_mix(_Mux,MIX,VA,VB,VC,yes))
  ).

% a = b * c
%
solve_mult( A, B, C) :-
  solve_mult( _Mutex, A, B, C, no).
%
solve_mult( Mux, A, B, C, Retry) :-
  var( Mux),
  !,
  ( arith_ground( A, Av) ->
     ( arith_ground( B, Bv) ->       			% +a +b ?c
	Mux = solved,
	( arith_zero( Bv) ->      arith_zero( Av)	% otherwise: zero division
	; arith_eval( Av/Bv, Vc), solve_one( C, Vc)
        )
     ; arith_ground( C, Cv) ->                  	% +a -b +c
	Mux = solved,
        ( arith_zero( Cv)   ->    arith_zero( Av)	% otherwise: zero division
	; arith_eval( Av/Cv, Vb), solve_one( B, Vb)
        )
     ;                                      		% +a -b -c
       delay_all( Retry, [B,C], solve_mult(Mux,A,B,C,yes))
     )
  ; arith_ground( B, Bv) ->
     Mux = solved,
     ( arith_ground( C, Cv) ->      			% -a +b +c
        arith_eval( Bv*Cv, Va), solve_one( A, Va)
     ; arith_zero( Bv) ->   	            		% -a +b -c
 	solve_one( A, 0)
     ;
        solve_two( A, C, Bv)
     )
  ; arith_ground( C, Cv) ->    				% -a -b +c
     Mux = solved,
     ( arith_zero( Cv) ->
        solve_one( A, 0)
     ;
        solve_two( A, B, Cv)
     )
  ;                                        		% -a -b -c
      delay_all( Retry, [A,B,C], solve_mult(Mux,A,B,C,yes))
  ).
solve_mult( _, _, _, _, _).

normalize_mult( B,C, K, Inhom, Hom) :-
  type(B,Bt), normalize( Bt, B, Kb, Ib, Hb),
  type(C,Ct), normalize( Ct, C, Kc, Ic, Hc),
  ( Hb = [] ->
     ( Hc = [] ->      					% -a +b +c
        arith_eval( Kb*Kc, K), arith_eval( Ib*Ic, Inhom), Hom = []
     ; arith_zero( Ib) ->
 	K = 1, Inhom = 0, Hom = []
     ;                               			% -a +b -c
        arith_eval( Kb*Ib*Kc, K), Inhom = Ic, Hom = Hc
     )
  ; Hc = [] ->    					% -a -b +c
     ( arith_zero( Ic) ->
        K = 1, Inhom = 0, Hom = []
     ;
        arith_eval( Kc*Ic*Kb, K), Inhom = Ib, Hom = Hb
     )
  ;                                        		% -a -b -c
      arith_eval( Kb*Kc, K), Inhom = 0, Hom = [VA*1],
      eqn_var_new( v, VA),
      var_with_def( VB, v, 1, Ib, Hb),
      var_with_def( VC, v, 1, Ic, Hc),
      delay_all( [VA,VB,VC], solve_mult(_Mux,VA,VB,VC,yes))
  ).

normalize_div( B,C, K, Inhom, Hom) :-			% a = b/c
  type(B,Bt), normalize( Bt, B, Kb, Ib, Hb),
  type(C,Ct), normalize( Ct, C, Kc, Ic, Hc),
  ( Hb = [] ->
     ( Hc = [] ->      					% -a +b +c
        ( arith_zero( Ic) ->
            fail  					% zero divison
        ;
            arith_eval( Kb/Kc, K), arith_eval( Ib/Ic, Inhom), Hom = []
        )
     ; arith_zero( Ib) ->
        var_with_def( VC, nz, 1, Ic, Hc),  		% nonzero( VC)
 	K = 1, Inhom = 0, Hom = []
     ;                               			% -a +b -c
        K = Kb, Inhom = 0, Hom = [VA*1],
        eqn_var_new( v, VA),
        var_with_def( VC, v, Kc, Ic, Hc),
        delay_all( [VA,VC], solve_mult(_Mux,Ib,VA,VC,yes))
     )
  ; Hc = [] ->    					% -a -b +c
     ( arith_zero( Ic) ->
        fail						% zero division
     ;
        arith_eval( Kb/(Kc*Ic), K), Inhom = Ib, Hom = Hb
     )
  ;                                        		% -a -b -c
      arith_eval( Kb/Kc, K), Inhom = 0, Hom = [VA*1],
      eqn_var_new( v, VA),
      var_with_def( VB, v, 1, Ib, Hb),
      var_with_def( VC, nz, 1, Ic, Hc),  		% nonzero( VC)
      delay_all( [VA,VB,VC], solve_mult(_Mux,VB,VA,VC,yes))
  ).


% a = b^c
%
solve_pow( A, B, C) :-
  solve_pow( _Mutex, A, B, C, no).
%
solve_pow( Mux, A, B, C, Retry) :- var(Mux), !,
  ( arith_ground( A, Av) ->      				% +a ?b ?c
     ( arith_zero( Av) ->
        Mux = solved, solve_one( B, 0)
     ; arith_ground( B, Bv) ->                     		% +a +b ?c
	Mux = solved,
	( arith_zero( Bv) ->                fail		% A=0, \+ zero(Va)
        ; arith_eval( 1=:=Bv) ->            arith_eval( 1=:=Av)
	; arith_eval( log(Av)/log(Bv), Vc), solve_one( C, Vc)
        )
     ; arith_ground( C, Cv) ->                    		% +a -b +c
	Mux = solved,
        ( arith_zero( Cv)   ->          arith_eval( 1=:=Av)
        ; arith_eval( 1=:=Cv) ->        solve_one( B, Av)
	; arith_eval( '**'(Av,1/Cv), Vb), solve_one( B, Vb)
        )
     ;                                             		% +a -b -c
       delay_all( Retry, [B,C], solve_pow(Mux,A,B,C,yes))
     )
  ; arith_ground( B, Bv) ->
     ( arith_ground( C, Cv) ->        				% -a +b +c
        Mux = solved, arith_eval( '**'(Bv,Cv), Va), solve_one( A, Va)
     ;                                         			% -a +b -c
        delay_all( Retry, [A,C], solve_pow(Mux,A,B,C,yes))
     )
  ; arith_ground( C, Cv) ->        				% -a -b +c
     ( arith_zero( Cv) ->
        Mux = solved,
	% nonzero( B),
        solve_one( A, 1)
     ; arith_eval( 1=:=Cv) ->
        Mux = solved, solve_two( A, B, 1)
     ;
        delay_all( Retry, [A,B], solve_pow(Mux,A,B,C,yes))
     )
  ;                                                  		% -a -b -c
      delay_all( Retry, [A,B,C], solve_pow(Mux,A,B,C,yes))
  ).
solve_pow( _, _, _, _, _).

normalize_pow( B, C, K, Inhom, Hom) :-
  type(B,Bt), normalize( Bt, B, Kb, Ib, Hb),
  type(C,Ct), normalize( Ct, C, Kc, Ic, Hc),
  ( Hb = [] ->
     ( Hc = [] ->        					% -a +b +c
        Mux = solved, K = 1, arith_eval( '**'(Kb*Ib,Kc*Ic), Inhom), Hom = []
     ;                                         			% -a +b -c
        K = 1, Inhom = 0, Hom = [VA*1],
        eqn_var_new( v, VA),
        arith_eval( Kb*Ib, Vb),
        var_with_def( VC, v, Kc, Ic, Hc),
        delay_all( [VA,VC], solve_pow(_Mux,VA,Vb,VC,yes))
     )
  ; Hc = [] ->        						% -a -b +c
     ( arith_zero( Ic) ->
        Mux = solved,
	var_with_def( VB, nz, 1, Ib, Hb),  			% nonzero( VB)
        K = 1, Inhom = 1, Hom = []
     ; arith_eval( 1=:=Kc*Ic) ->
        Mux = solved, K = Kb, Inhom = Ib, Hom = Hb
     ;
        K = 1, Inhom = 0, Hom = [VA*1],
        eqn_var_new( v, VA),
        var_with_def( VB, v, Kb, Ib, Hb),
        arith_eval( Kc*Ic, Vc),
        delay_all( [VA,VB], solve_pow(_Mux,VA,VB,Vc,yes))
     )
  ;                                                  		% -a -b -c
      K = 1, Inhom = 0, Hom = [VA*1],
      eqn_var_new( v, VA),
      var_with_def( VB, v, Kb, Ib, Hb),
      var_with_def( VC, v, Kc, Ic, Hc),
      delay_all( [VA,VB,VC], solve_pow(_Mux,VA,VB,VC,yes))
  ).

% a = TRIG(b)
%
solve_trig( Trig, A, B) :-
  solve_trig( _Mutex, Trig, A, B, no).
%
solve_trig( Mux, Trig, A, B, Retry) :- var( Mux), !,
  ( arith_ground( A, Av) ->               		% +a ?b
      Mux = solved,
      trig( Trig, y, Av, Val), solve_one( B, Val)
  ; arith_ground( B, Bv) ->                     	% -a +b
      Mux = solved,
      trig( Trig, x, Bv, Val), solve_one( A, Val)
  ;                                                   	% -a -b
      delay_all( Retry, [A,B], solve_trig( Mux, Trig, A, B, yes))
  ).
solve_trig( _, _, _, _, _).

trig( sin,  x, X, Y) :- arith_eval( sin(X),  Y).
trig( sin,  y, X, Y) :- arith_eval( asin(X), Y).
trig( asin, x, X, Y) :- arith_eval( asin(X), Y).
trig( asin, y, X, Y) :- arith_eval( sin(X),  Y).
trig( cos,  x, X, Y) :- arith_eval( cos(X),  Y).
trig( cos,  y, X, Y) :- arith_eval( acos(X), Y).
trig( acos, x, X, Y) :- arith_eval( acos(X), Y).
trig( acos, y, X, Y) :- arith_eval( cos(X),  Y).
trig( tan,  x, X, Y) :- arith_eval( tan(X),  Y).
trig( tan,  y, X, Y) :- arith_eval( atan(X), Y).
trig( atan, x, X, Y) :- arith_eval( atan(X), Y).
trig( atan, y, X, Y) :- arith_eval( tan(X),  Y).

normalize_trig( Trig, B, 1, Inhom, Hom) :-
  type(B,Bt), normalize( Bt, B, K, Ib, Hb),
  ( Hb = [] ->                     			% -a +b
      arith_eval( K*Ib, Ibk),
      trig( Trig, x, Ibk, Inhom), Hom = []
  ;                                                   	% -a -b
      Inhom = 0, Hom = [VA*1],
      eqn_var_new( v, VA),
      var_with_def( VB, v, K, Ib, Hb),
      delay_all( [VA,VB], solve_trig(_Mux,Trig,VA,VB,yes))
  ).


% See clpr_freeze/2 in clprt.pl

system_freeze(Var, Goal) :-
  % nonvar(Goal),					% assert
  term_to_meta(G,Goal),
  attach_attribute(V, clpr_frozen(V,G,true)),
  Var = V.
