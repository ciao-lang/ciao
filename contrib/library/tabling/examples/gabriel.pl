:- module(gabriel, 
	[
	    test/1, 
	    result/1,
	    spend_time/2
	]).

:- include(tabling_type).

:- set_prolog_flag(multi_arity_warnings, off).

:- dynamic results/1.

 %% :- bridge /(mylength,3).
 %% :- bridge /(concat,3).
 %% :- bridge /(match,2).
 %% :- bridge /(p_match,2).
 %% :- bridge /(p_investigate,2).
 %% :- bridge /(property,3).
 %% :- bridge /(get_pats,4).
 %% :- bridge /(investigate,2).
 %% :- bridge /(split,4).
 %% :- bridge /(randomize,3).
 %% :- bridge /(fill,3).
 %% :- bridge /(init,6).

:- table set_unify/2.
:- table length__1/3.
:- table length__1/2.
:- table concat__1/3.
:- table my_atom__1/1.
:- table match__1/2.
:- table p_match__1/2.
:- table p_investigate__1/2.
:- table my_arg__1/3.
:- table my_functor__1/3.
:- table property__1/3.
:- table get_pats__1/4.
:- table get_pats__1/3.
:- table investigate__1/2.
:- table split__1/4.
:- table randomize__1/3.
:- table fill__1/3.
:- table test__1/3.
:- table init__1/6.
:- table init__1/5.
:- table main__1/2.

spend_time(NT,T) :-
	abolish_all_tables,
        statistics(runtime,[_,_]),
	tp, abolish_all_tables,
        statistics(runtime,[_,Tt]),
	N is NT / Tt,
        statistics(runtime,[_,_]),
        (
            between(1,N,_),
            tp, abolish_all_tables,
            fail
        ;
            true
        ),
        statistics(runtime,[_,Tfin]),
        (
            between(1,N,_),
            fail
        ;
            true
        ),
        statistics(runtime,[_,Tfin2]),
        T is (Tfin - Tfin2) / N.

test(_) :- abolish_all_tables, tp, fail.
test(L) :- show_facts, get_list(L).

get_list([R|L]) :-
	retract(results(R)), !,
	get_list(L).

get_list([]).

unify_sets([],[]).
unify_sets([A|As],[B|Bs]) :- 
       set_unify(A,B), unify_sets(As,Bs).


%---------------- Transformed program ----------------------

main__1(_428,_430) :- 
        [_365] = _432, [_399] = _434, 
        init([num],[num],[num],[_365],[_379]),
        randomize([_379],[_395],[num]),
        investigate([_395],[_399]),
        normalize_result([_432,_434],
                         [_428,_430]).
init__1(_392,_394,_396,_398,_400) :- 
        [_342] = _402, [_350] = _404, [_354] = _406, [_358] = _408, [_362] = _410, 
        init([_342],[_350],[_350],[_354],[_358],[_362]),
        normalize_result([_402,_404,_406,_408,_410],
                         [_392,_394,_396,_398,_400]).
init__1(_238,_240,_242,_244,_246,_248) :- 
        [num] = _250, [_197] = _252, [_201] = _254, [_205] = _256, [_209] = _258, [_213] = _260, 
        normalize_result([_250,_252,_254,_256,_258,_260],
                          [_238,_240,_242,_244,_246,_248]).
init__1(_999,_1001,_1003,_1005,_1007,_1009) :- 
        [_898] = _1011, [_913] = _1013, [_937] = _1015, [_941] = _1017, [_945] = _1019, [list,_949] = _1021, 
        fill([_913],[list],[_823]),
        get_pats([_941],[_945],[_839]),
        'my is'([_871],[[_937],[_913]]),
        fill([_871],[list,_823],[_881]),
        'my is'([_929],[[_898],[num]]),
        test([_913],[_933],[_937]),
        init([_929],[_933],[_937],[_941],[_945],[_949]),
        normalize_result([_1011,_1013,_1015,_1017,_1019,_1021],
                         [_999,_1001,_1003,_1005,_1007,_1009]).
test__1(_286,_288,_290) :- 
        [_232] = _292, [_250] = _294, [_259] = _296, 
        'my ='([_232],[num]),
        'my is'([_250],[_259]),
        normalize_result([_292,_294,_296],
                         [_286,_288,_290]).
test__1(_259,_261,_263) :- 
        [_229] = _265, [_220] = _267, [_205] = _269, 
        'my is'([_220],[[_229],[num]]),
        normalize_result([_265,_267,_269],
                         [_259,_261,_263]).
fill__1(_149,_151,_153) :- 
        [num] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
fill__1(_362,_364,_366) :- 
        [_313] = _368, [_332] = _370, [list,_336] = _372, 
        'my is'([_328],[[_313],[num]]),
        fill([_328],[_332],[_336]),
        normalize_result([_368,_370,_372],
                         [_362,_364,_366]).
randomize__1(_145,_147,_149) :- 
        [list] = _151, [list] = _153, [_129] = _155, 
        normalize_result([_151,_153,_155],
                          [_145,_147,_149]).
randomize__1(_613,_615,_617) :- 
        [_553] = _619, [list,_574] = _621, [_516] = _623, 
        mylength([_553],[_500]),
        'my is'([_578],[[_516],[num]]),
        'my is'([_549],[_578]),
        split([_549],[_553],[_557],[_570]),
        randomize([_570],[_574],[_578]),
        normalize_result([_619,_621,_623],
                         [_613,_615,_617]).
split__1(_188,_190,_192,_194) :- 
        [num] = _196, [list,_169] = _198, [_165] = _200, [_169] = _202, 
        normalize_result([_196,_198,_200,_202],
                          [_188,_190,_192,_194]).
split__1(_426,_428,_430,_432) :- 
        [_370] = _434, [list,_389] = _436, [_393] = _438, [list,_397] = _440, 
        'my is'([_385],[[_370],[num]]),
        split([_385],[_389],[_393],[_397]),
        normalize_result([_434,_436,_438,_440],
                         [_426,_428,_430,_432]).
investigate__1(_122,_124) :- 
        [list] = _126, [_109] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
investigate__1(_372,_374) :- 
        [list,_342] = _376, [_346] = _378, 
        property([_315],[atom],[_331]),
        p_investigate([_331],[_346]),
        investigate([_342],[_346]),
        normalize_result([_376,_378],
                         [_372,_374]).
get_pats__1(_284,_286,_288) :- 
        [_248] = _290, [_260] = _292, [_256] = _294, 
        get_pats([_248],[_260],[_256],[_260]),
        normalize_result([_290,_292,_294],
                         [_284,_286,_288]).
get_pats__1(_174,_176,_178,_180) :- 
        [num] = _182, [_147] = _184, [list] = _186, [_155] = _188, 
        normalize_result([_182,_184,_186,_188],
                          [_174,_176,_178,_180]).
get_pats__1(_426,_428,_430,_432) :- 
        [_370] = _434, [list,_389] = _436, [list,_393] = _438, [_397] = _440, 
        'my is'([_385],[[_370],[num]]),
        get_pats([_385],[_389],[_393],[_397]),
        normalize_result([_434,_436,_438,_440],
                         [_426,_428,_430,_432]).
get_pats__1(_307,_309,_311,_313) :- 
        [_269] = _315, [list] = _317, [_277] = _319, [_281] = _321, 
        get_pats([_269],[_281],[_277],[_281]),
        normalize_result([_315,_317,_319,_321],
                         [_307,_309,_311,_313]).
property__1(_360,_362,_364) :- 
        [list,_84] = _366, [_314] = _368, [_334] = _370, 
        my_functor([_330],[_314],[_318]),
        my_arg([num],[_330],[_334]),
        normalize_result([_366,_368,_370],
                         [_360,_362,_364]).
property__1(_269,_271,_273) :- 
        [list,_238] = _275, [_242] = _277, [_246] = _279, 
        property([_238],[_242],[_246]),
        normalize_result([_275,_277,_279],
                         [_269,_271,_273]).
my_functor__1(_159,_161,_163) :- 
        [other] = _165, [atom] = _167, [list,list,list] = _169, 
        normalize_result([_165,_167,_169],
                          [_159,_161,_163]).
my_functor__1(_175,_177,_179) :- 
        [list,_84] = _181, [atom] = _183, [list,list,list] = _185, 
        normalize_result([_181,_183,_185],
                          [_175,_177,_179]).
my_functor__1(_149,_151,_153) :- 
        [other] = _155, [atom] = _157, [list,list] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
my_functor__1(_149,_151,_153) :- 
        [other] = _155, [atom] = _157, [list,list] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
my_arg__1(_145,_147,_149) :- 
        [num] = _151, [other] = _153, [_129] = _155, 
        normalize_result([_151,_153,_155],
                          [_145,_147,_149]).
my_arg__1(_161,_163,_165) :- 
        [num] = _167, [list,_100] = _169, [_145] = _171, 
        normalize_result([_167,_169,_171],
                          [_161,_163,_165]).
my_arg__1(_145,_147,_149) :- 
        [num] = _151, [other] = _153, [_129] = _155, 
        normalize_result([_151,_153,_155],
                          [_145,_147,_149]).
my_arg__1(_145,_147,_149) :- 
        [num] = _151, [other] = _153, [_129] = _155, 
        normalize_result([_151,_153,_155],
                          [_145,_147,_149]).
p_investigate__1(_122,_124) :- 
        [list] = _126, [_109] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
p_investigate__1(_283,_285) :- 
        [list,_256] = _287, [_260] = _289, 
        p_match([_260],[_249]),
        p_investigate([_256],[_260]),
        normalize_result([_287,_289],
                         [_283,_285]).
p_match__1(_122,_124) :- 
        [list] = _126, [_109] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
p_match__1(_270,_272) :- 
        [list,_84] = _274, [_225] = _276, 
        match([_225],[_229]),
        'my ='([atom],[atom]),
        normalize_result([_274,_276],
                         [_270,_272]).
p_match__1(_215,_217) :- 
        [list,_191] = _219, [_195] = _221, 
        p_match([_191],[_195]),
        normalize_result([_219,_221],
                         [_215,_217]).
match__1(_116,_118) :- 
        [list] = _120, [list] = _122, 
        normalize_result([_120,_122],
                          [_116,_118]).
match__1(_290,_292) :- 
        [list,_263] = _294, [list,_267] = _296, 
        'my ='([_245],[_254]),
        match([_263],[_267]),
        normalize_result([_294,_296],
                         [_290,_292]).
match__1(_369,_371) :- 
        [_331] = _373, [list,_343] = _375, 
        'my ='([_305],[other]),
        concat([_323],[_339],[_331]),
        match([_339],[_343]),
        normalize_result([_373,_375],
                         [_369,_371]).
match__1(_331,_333) :- 
        [list,_301] = _335, [list,_305] = _337, 
        my_atom([_283]),
        'my ='([_283],[_292]),
        match([_301],[_305]),
        normalize_result([_335,_337],
                         [_331,_333]).
match__1(_295,_297) :- 
        [list,_268] = _299, [list,_272] = _301, 
        match([_257],[_261]),
        match([_268],[_272]),
        normalize_result([_299,_301],
                         [_295,_297]).
my_atom__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
my_atom__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
concat__1(_149,_151,_153) :- 
        [list] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
concat__1(_279,_281,_283) :- 
        [list,_248] = _285, [_252] = _287, [list,_256] = _289, 
        concat([_248],[_252],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
length__1(_226,_228) :- 
        [_197] = _230, [_205] = _232, 
        mylength([_197],[num],[_205]),
        normalize_result([_230,_232],
                         [_226,_228]).
length__1(_149,_151,_153) :- 
        [list] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
length__1(_362,_364,_366) :- 
        [list,_328] = _368, [_313] = _370, [_336] = _372, 
        'my is'([_332],[[_313],[num]]),
        mylength([_328],[_332],[_336]),
        normalize_result([_368,_370,_372],
                         [_362,_364,_366]).

%---------------- Definitions of tabled preds --------------
mylength(_63,_65,_67) :- 
        length__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
mylength(_63,_65) :- 
        length__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
concat(_63,_65,_67) :- 
        concat__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
my_atom(_63) :- 
        my_atom__1(_65),
        unify_sets([_63], [_65]).
match(_63,_65) :- 
        match__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
p_match(_63,_65) :- 
        p_match__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
p_investigate(_63,_65) :- 
        p_investigate__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
my_arg(_63,_65,_67) :- 
        my_arg__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
my_functor(_63,_65,_67) :- 
        my_functor__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
property(_63,_65,_67) :- 
        property__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
get_pats(_63,_65,_67,_69) :- 
        get_pats__1(_71,_73,_75,_77),
        unify_sets([_63,_65,_67,_69], [_71,_73,_75,_77]).
get_pats(_63,_65,_67) :- 
        get_pats__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
investigate(_63,_65) :- 
        investigate__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
split(_63,_65,_67,_69) :- 
        split__1(_71,_73,_75,_77),
        unify_sets([_63,_65,_67,_69], [_71,_73,_75,_77]).
randomize(_63,_65,_67) :- 
        randomize__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
fill(_63,_65,_67) :- 
        fill__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
test(_63,_65,_67) :- 
        test__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
init(_63,_65,_67,_69,_71,_73) :- 
        init__1(_75,_77,_79,_81,_83,_85),
        unify_sets([_63,_65,_67,_69,_71,_73], [_75,_77,_79,_81,_83,_85]).
init(_63,_65,_67,_69,_71) :- 
        init__1(_73,_75,_77,_79,_81),
        unify_sets([_63,_65,_67,_69,_71], [_73,_75,_77,_79,_81]).
main(_63,_65) :- 
        main__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).

%---------------- Tp ---------------------------------------

tp :- length__1(_64,_66,_68), fail.
tp :- length__1(_64,_66), fail.
tp :- concat__1(_64,_66,_68), fail.
tp :- my_atom__1(_64), fail.
tp :- match__1(_64,_66), fail.
tp :- p_match__1(_64,_66), fail.
tp :- p_investigate__1(_64,_66), fail.
tp :- my_arg__1(_64,_66,_68), fail.
tp :- my_functor__1(_64,_66,_68), fail.
tp :- property__1(_64,_66,_68), fail.
tp :- get_pats__1(_64,_66,_68,_70), fail.
tp :- get_pats__1(_64,_66,_68), fail.
tp :- investigate__1(_64,_66), fail.
tp :- split__1(_64,_66,_68,_70), fail.
tp :- randomize__1(_64,_66,_68), fail.
tp :- fill__1(_64,_66,_68), fail.
tp :- test__1(_64,_66,_68), fail.
tp :- init__1(_64,_66,_68,_70,_72,_74), fail.
tp :- init__1(_64,_66,_68,_70,_72), fail.
tp :- main__1(_64,_66), fail.
tp.


%---------------- Builtin Preds ----------------------------

'my ='(X1,X2) :- 'my =_1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my ==='(X1,X2) :- 'my ===__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my =='([num],[num]).
'my is'(X1,X2) :- 'my is__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my <'([num],[num]).
'my >'([num],[num]).
'my >='([num],[num]).
'my =<'([num],[num]).
'my =:='([num],[num]).

'my =_1'(X,X).
'my ===__1'(_,_).
'my is__1'(num,num).


%---------------- Show Result ------------------------------

show_facts :- length__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(length(_63,_65,_67))), fail.
show_facts :- length__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(length(_63,_65))), fail.
show_facts :- concat__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(concat(_63,_65,_67))), fail.
show_facts :- my_atom__1(_63),
              numbervars([_63]),
              assert(results(my_atom(_63))), fail.
show_facts :- match__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(match(_63,_65))), fail.
show_facts :- p_match__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(p_match(_63,_65))), fail.
show_facts :- p_investigate__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(p_investigate(_63,_65))), fail.
show_facts :- my_arg__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(my_arg(_63,_65,_67))), fail.
show_facts :- my_functor__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(my_functor(_63,_65,_67))), fail.
show_facts :- property__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(property(_63,_65,_67))), fail.
show_facts :- get_pats__1(_63,_65,_67,_69),
              numbervars([_63,_65,_67,_69]),
              assert(results(get_pats(_63,_65,_67,_69))), fail.
show_facts :- get_pats__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(get_pats(_63,_65,_67))), fail.
show_facts :- investigate__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(investigate(_63,_65))), fail.
show_facts :- split__1(_63,_65,_67,_69),
              numbervars([_63,_65,_67,_69]),
              assert(results(split(_63,_65,_67,_69))), fail.
show_facts :- randomize__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(randomize(_63,_65,_67))), fail.
show_facts :- fill__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(fill(_63,_65,_67))), fail.
show_facts :- test__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(test(_63,_65,_67))), fail.
show_facts :- init__1(_63,_65,_67,_69,_71,_73),
              numbervars([_63,_65,_67,_69,_71,_73]),
              assert(results(init(_63,_65,_67,_69,_71,_73))), fail.
show_facts :- init__1(_63,_65,_67,_69,_71),
              numbervars([_63,_65,_67,_69,_71]),
              assert(results(init(_63,_65,_67,_69,_71))), fail.
show_facts :- main__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(main(_63,_65))), fail.
show_facts.
%----------------------------------------------------------
%:- import set_unify/2 from set_unify.

set_unify(A,B) :-
	flatten(A,AF), flatten(B,BF),
        (my_ground(AF), my_ground(BF) -> AF==BF
          ;
         ord_union(AF,BF,ToCover),
         ord_setproduct(AF,BF,AxB),
         drop_nonuni(AxB,UTable),
         covering_u_table(UTable,ToCover,CovUTable),
         keysort(CovUTable,CovUTableS),
         table2graph(CovUTableS,CovUGraph),
         u_graph_proceed(CovUGraph)
        ).

drop_nonuni([],[]).
drop_nonuni([X-Y|AxB],Out) :-
        (\+ \+ X=Y -> Out = [X-Y|NewAxB] ; Out = NewAxB),
        drop_nonuni(AxB,NewAxB).

covering_u_table([],[],[]).
covering_u_table([X-Y|XsYs],ToCover,[X-L,Y-L|UTRest]) :-
        (X @< Y ->
         ord_subtract(ToCover,[X,Y],ToCoverRest)
          ;
         ord_subtract(ToCover,[Y,X],ToCoverRest)),
        covering_u_table(XsYs,ToCoverRest,UTRest).
covering_u_table([_|XsYs],ToCover,UTable) :-
        covering_u_table(XsYs,ToCover,UTable).

u_graph_proceed([]).
u_graph_proceed([V-E|G]) :- match_single(V,E), u_graph_proceed(G).

match_single(S,Y) :-
	var(S) -> S=Y ; match_list(Y,S).	% simple atom

match_list([],_).
match_list([S|Xs],S) :- match_list(Xs,S).

normalize_result(Args,SortedArgsC) :-!,
        flatten_sets(Args,ArgsF),
        occur_graph(ArgsF,OG),
        swap_keys_values(OG,GO),
        keysort(GO,GOS),
        drop_equiv(GOS,NewGO),
        graph2table(NewGO,OGTable),
	keysort(OGTable,OGTableS),	% was merge_keysort
        table2graph(OGTableS,KeysArgs),
        keys_and_values(KeysArgs,_,ArgsC),
	sortall(ArgsC,SortedArgsC).

sortall([], []).
sortall([H|T], [NH|NT]) :- sort(H,NH), sortall(T,NT).

drop_equiv([], []).
drop_equiv([One|Rest], Out) :- drop_equiv(One, Rest, Out).

%:- index drop_equiv/3-2.

drop_equiv(X, [], [X]).
drop_equiv(E1-V1, [E2-V2|Vs],Out) :-
        (E1==E2, my_variant(V1,V2) ->
         V1=V2, Out=OutRest
          ;
         Out=[E1-V1|OutRest]),
        drop_equiv(E2-V2, Vs, OutRest).

og_table([],_,[]).
og_table([One|Args],I,OG_Table) :- og_table(One,Args,I,OG_Table).

og_table([],Args,I,OG_Table) :- !, J is I+1, og_table(Args,J,OG_Table).
og_table([V|Vs],Args,I,[V-I|OG_Table]) :- og_table(Vs,Args,I,OG_Table).

table2graph([],[]).
table2graph([One|Rest],Out) :- table2graph(One,Rest,Out).

%:- index table2graph/3-2.

table2graph(V-I,[],[V-[I]]).
table2graph(V-I,[W-J|Vs],Out) :-
        ( V==W ->
          Out = [V-[I|Is]|VsClps], Cont = [V-Is|VsClps]
          ;
          Out = [V-[I]|VsClps], Cont = VsClps),
        table2graph(W-J,Vs,Cont).

graph2table([],[]).
graph2table([Elem-V|Vs],Edges) :- graph2table(Elem,V,Vs,Edges).

graph2table([],_,Vs,Edges) :- graph2table(Vs,Edges).
graph2table([E|Es],V,Vs,[E-V|Edges]) :- graph2table(Es,V,Vs,Edges).

swap_keys_values([],[]).
swap_keys_values([Key-Val|KVs],[Val-Key|VKs]) :- swap_keys_values(KVs,VKs).

/*------------------ Bypasses a bug of SWI -----------------------------

merge_keysort([],[]) :- !.
merge_keysort([X],[X]) :- !.
merge_keysort(VL,VS) :-
        split(VL,VL1,VL2),
        merge_keysort(VL1,VL1S), merge_keysort(VL2,VL2S),
        merge(VL1S,VL2S,VS).

split([],[],[]) :- !.
split([X],[X],[]) :- !.
split([X1,X2|Xs],[X1|X1s],[X2|X2s]) :- split(Xs,X1s,X2s).

merge([],[],[]) :- !.
merge(L,[],L) :- !.
merge([],L,L) :- !.
merge([K1-V1|X1s],[K2-V2|X2s],[K1-V1|Xs]) :- K1@<K2, !,
        merge(X1s,[K2-V2|X2s],Xs).
merge([X1|X1s],[X2|X2s],[X2|Xs]) :- merge([X1|X1s],X2s,Xs).

  ----------------------------------------------------------------------*/

%%-----------------------------------------------------------------------------

keys_and_values([], [], []).
keys_and_values([Key-Value|Pairs], [Key|Keys], [Value|Values]) :-
   keys_and_values(Pairs, Keys, Values).

%%-----------------------------------------------------------------------------
flatten(Set,SetF) :- colterms(Set,[],SetMemb), sort(SetMemb,SetF).

colterms(V,I,O) :- var(V), !, O = [V|I].
colterms([],I,O) :- !, O = I.
colterms(A,I,O) :- atom(A), !, O=[A|I].
colterms([T|Ts],I,O) :- colterms(T,I,IT), colterms(Ts,IT,O).

flatten_sets([],[]).
flatten_sets([T|Ts],[FT|FTs]) :- flatten(T,FT), flatten_sets(Ts,FTs).

%%-----------------------------------------------------------------------------
%% Operations on graphs

insert_vertice([],N,[N]).
insert_vertice([V-E|G],V1-E1,[V-EE1|G]) :- V==V1, ord_union(E,E1,EE1), !.
insert_vertice([N|G],N1,[N|NewG]) :- insert_vertice(G,N1,NewG).

%%-----------------------------------------------------------------------------
occur_graph(ArgsVs,OG) :-
        og_table(ArgsVs,0,OG_Table),
        sort(OG_Table,OG_TableSorted),
        table2graph(OG_TableSorted,OG).

occur_graph(ArgsV,OG) :- occur_graph(ArgsV,0,OG).
occur_graph([],_,[]).
occur_graph([L|A],I,OG) :- J is I+1, occur_graph(A,J,O1), ogl(L,I,O1,OG) .

ogl([],_,O,O).
ogl([V|R],I,In,Out) :- ogl(R,I,In,O1), insert_vertice(O1,V-[I],Out).

%%-----------------------------------------------------------------------------

numbervars(X):-my_number_vars(X,0,_).

my_number_vars(T, N0, N) :- 
    var(T),!, 
    T='$VAR'(N0),
    N is N0+1.
my_number_vars(T, N0, N) :- 
    atomic(T),!,
     N0=N.
my_number_vars([X|Xs], N0, N) :- !,
    my_number_vars(X, N0, N1),
    my_number_vars(Xs, N1, N).
my_number_vars(X, N0, N) :-
    functor(X, _, A),
    my_number_vars(0, A, X, N0, N).

my_number_vars(A, A, _, N0, N) :- !,
    N0=N.
my_number_vars(A0, A, X, N0, N) :-
    A1 is A0+1,
    arg(A1, X, X1),
    my_number_vars(X1, N0, N1),
    my_number_vars(A1, A, X, N1, N).


%   list_to_ord_set(+List, ?Set)
%   is true when Set is the ordered representation of the set represented
%   by the unordered representation List.  The only reason for giving it
%   a name at all is that you may not have realised that sort/2 could be
%   used this way.

list_to_ord_set(List, Set) :-
	sort(List, Set).


%   merge(+List1, +List2, -Merged)
%   is true when Merged is the stable merge of the two given lists.
%   If the two lists are not ordered, the merge doesn't mean a great
%   deal.  Merging is perfectly well defined when the inputs contain
%   duplicates, and all copies of an element are preserved in the
%   output, e.g. merge("122357", "34568", "12233455678").  Study this
%   routine carefully, as it is the basis for all the rest.

merge([Head1|Tail1], [Head2|Tail2], [Head2|Merged]) :-
	Head1 @> Head2, !,
	merge([Head1|Tail1], Tail2, Merged).
merge([Head1|Tail1], List2, [Head1|Merged]) :-
	List2 \== [], !,
	merge(Tail1, List2, Merged).
merge([], List2, List2) :- !.
merge(List1, [], List1).



%   ord_disjoint(+Set1, +Set2)
%   is true when the two ordered sets have no element in common.  If the
%   arguments are not ordered, I have no idea what happens.

ord_disjoint([], _) :- !.
ord_disjoint(_, []) :- !.
ord_disjoint([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_disjoint(Order, Head1, Tail1, Head2, Tail2).

ord_disjoint(<, _, Tail1, Head2, Tail2) :-
	ord_disjoint(Tail1, [Head2|Tail2]).
ord_disjoint(>, Head1, Tail1, _, Tail2) :-
	ord_disjoint([Head1|Tail1], Tail2).



%   ord_insert(+Set1, +Element, ?Set2)
%   is the equivalent of add_element for ordered sets.  It should give
%   exactly the same result as merge(Set1, [Element], Set2), but a bit
%   faster, and certainly more clearly.

ord_insert([], Element, [Element]).
ord_insert([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_insert(Order, Head, Tail, Element, Set).


ord_insert(<, Head, Tail, Element, [Head|Set]) :-
	ord_insert(Tail, Element, Set).
ord_insert(=, Head, Tail, _, [Head|Tail]).
ord_insert(>, Head, Tail, Element, [Element,Head|Tail]).



%   ord_intersect(+Set1, +Set2)
%   is true when the two ordered sets have at least one element in common.
%   Note that the test is == rather than = .

ord_intersect([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).

ord_intersect(=, _, _, _, _).
ord_intersect(<, _, Tail1, Head2, Tail2) :-
	ord_intersect(Tail1, [Head2|Tail2]).
ord_intersect(>, Head1, Tail1, _, Tail2) :-
	ord_intersect([Head1|Tail1], Tail2).



%   ord_intersect(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the ordered representation of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.

ord_intersect(_, [], []) :- !.
ord_intersect([], _, []) :- !.
ord_intersect([Head1|Tail1], [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2, Intersection).

ord_intersect(=, Head,  Tail1, _,     Tail2, [Head|Intersection]) :-
	ord_intersect(Tail1, Tail2, Intersection).
ord_intersect(<, _,     Tail1, Head2, Tail2, Intersection) :-
	ord_intersect(Tail1, [Head2|Tail2], Intersection).
ord_intersect(>, Head1, Tail1, _,     Tail2, Intersection) :-
	ord_intersect([Head1|Tail1], Tail2, Intersection).



%   ord_seteq(+Set1, +Set2)
%   is true when the two arguments represent the same set.  Since they
%   are assumed to be ordered representations, they must be identical.


ord_seteq(Set1, Set2) :-
	Set1 == Set2.


%   ord_setproduct(+Set1, +Set2, ?Product)
%   is in fact identical to setproduct(Set1, Set2, Product).
%   If Set1 and Set2 are ordered sets, Product will be an ordered
%   set of x1-x2 pairs.  Note that we cannot solve for Set1 and
%   Set2, because there are infinitely many solutions when
%   Product is empty, and may be a large number in other cases.

ord_setproduct([], _, []).
ord_setproduct([H|T], L, Product) :-
	ord_setproduct(L, H, Product, Rest),
	ord_setproduct(T, L, Rest).

ord_setproduct([], _, L, L).
ord_setproduct([H|T], X, [X-H|TX], TL) :-
	ord_setproduct(T, X, TX, TL).


%   ord_subset(+Set1, +Set2)
%   is true when every element of the ordered set Set1 appears in the
%   ordered set Set2.

ord_subset([], _) :- !.
ord_subset([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset(Order, Head1, Tail1, Head2, Tail2).

ord_subset(=, _, Tail1, _, Tail2) :-
	ord_subset(Tail1, Tail2).
ord_subset(>, Head1, Tail1, _, Tail2) :-
	ord_subset([Head1|Tail1], Tail2).



%   ord_subtract(+Set1, +Set2, ?Difference)
%   is true when Difference contains all and only the elements of Set1
%   which are not also in Set2.


ord_subtract([], _, []).
ord_subtract([Head1|Tail1], Set2, Difference) :-
	ord_subtract(Set2, Head1, Tail1, Difference).

ord_subtract([], Head1, Tail1, [Head1|Tail1]).
ord_subtract([Head2|Tail2], Head1, Tail1, Difference) :-
	compare(Order, Head1, Head2),
	ord_subtract(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_subtract(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
        'ord subtract'(Tail1, Head2, Tail2, Difference).
ord_subtract(>, Head1, Tail1, _,     Tail2, Difference) :-
        ord_subtract(Tail2, Head1, Tail1, Difference).
ord_subtract(=, _,     Tail1, _,     Tail2, Difference) :-
        ord_subtract(Tail1, Tail2, Difference).

'ord subtract'([], _, _, []).
'ord subtract'([Head1|Tail1], Head2, Tail2, Difference) :-
        compare(Order, Head1, Head2),
        ord_subtract(Order, Head1, Tail1, Head2, Tail2, Difference).


%   ord_symdiff(+Set1, +Set2, ?Difference)
%   is true when Difference is the symmetric difference of Set1 and Set2.

ord_symdiff(Set1, [], Set1) :- !.
ord_symdiff([], Set2, Set2) :- !.
ord_symdiff([Head1|Tail1], [Head2|Tail2], Difference) :-
	compare(Order, Head1, Head2),
	ord_symdiff(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_symdiff(=, _,     Tail1, _,     Tail2, Difference) :-
	ord_symdiff(Tail1, Tail2, Difference).
ord_symdiff(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
	ord_symdiff(Tail1, [Head2|Tail2], Difference).
ord_symdiff(>, Head1, Tail1, Head2, Tail2, [Head2|Difference]) :-
	ord_symdiff([Head1|Tail1], Tail2, Difference).



%   ord_union(+Set1, +Set2, ?Union)
%   is true when Union is the union of Set1 and Set2.  Note that when
%   something occurs in both sets, we want to retain only one copy.

ord_union([], Set2, Set2).
ord_union([Head1|Tail1], Set2, Union) :-
	ord_union(Set2, Head1, Tail1, Union).

ord_union([], Head1, Tail1, [Head1|Tail1]).
ord_union([Head2|Tail2], Head1, Tail1, Union) :-
	compare(Order, Head1, Head2),
	ord_union(Order, Head1, Tail1, Head2, Tail2, Union).

ord_union(=, Head,  Tail1, _,     Tail2, [Head|Union]) :-
	ord_union(Tail1, Tail2, Union).
ord_union(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	ord_union(Tail1, [Head2|Tail2], Union).
ord_union(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	ord_union([Head1|Tail1], Tail2, Union).


%   ord_union(+ListOfSets, ?Union)
%   is true when ListOfSets is given as a proper list of ordered sets
%   and Union is their union.  Letting K be the length of ListOfSets,
%   and N the sum of the sizes of its elements, the cost is of order
%   N.lg(K).  The auxiliary routine
%   ord_union_3(N, L, U, R)
%   is true when the union of the first N sets in L is U and
%   R is the remaining elements of L.

ord_union(ListOfSets, Union) :-
	mylength(ListOfSets, NumberOfSets),
	ord_union_3(NumberOfSets, ListOfSets, Union, []).

ord_union_3(0, R, [], R) :- !.
ord_union_3(1, [U|R], U, R) :- !.
ord_union_3(2, [A,B|R], U, R) :- !,
	ord_union(A, B, U).
ord_union_3(N, R0, U, R) :-
	P is N>>1,      % |first  half of list|
	Q is N- P,      % |second half of list|
	ord_union_3(P, R0, A, R1),
	ord_union_3(Q, R1, B, R),
	ord_union(A, B, U).


%   ord_del_element(+Set1, +Element, ?Set2)
%   is the equivalent of del_element for ordered sets.  Because it uses
%   ordering, it typically builds less structure, but is slower than
%   del_element.  I am beginning to wonder whether a predicate
%       set_plus(SmallSet, Element, LargeSet)
%   would be a better way of doing this, the idea being that
%   LargeSet = SmallSet U {Element} and Element is not in SmallSet.
%   There is already a predicate with this effect called select/3.

ord_del_element([], _, []).
ord_del_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_del_element(Order, Element, Head, Tail, Set).

ord_del_element(<, Element, Head, Tail, [Head|Set]) :-
	ord_del_element(Tail, Element, Set).
ord_del_element(=, _, _, Set, Set).
ord_del_element(>, _, Head, Tail, [Head|Tail]).


my_ground(X):-var(X),!,fail.
my_ground(X):-atomic(X),!,true.
my_ground([X|Xs]):-!,my_ground(X),my_ground(Xs).
my_ground(X):-functor(X,_,N),my_ground(X,N).

my_ground(_,N):-N=:=0,!,true.
my_ground(X,N):-N>0,arg(N,X,A),my_ground(A),N1 is N-1,my_ground(X,N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   subsumes_chk(General, Specific)
%   is true when Specific is an instance of General. However, this
%   predicate performs the test without binding any variables neither
%   in General nor in Specific.

subsumes_chk(General, Specific) :-
    \+((numbervars(Specific),
       \+ (General = Specific)
       )).

%   subsumes(General, Specific)
%   is true when Specific is an instance of General. Unlike subsumes_chk/2
%   this predicate will bind variables in General (but not those in Specific)
%   so that General becomes identical to Specific.

subsumes(General, Specific) :-
    subsumes_chk(General, Specific),
    General = Specific.

/*** SB-Prolog one
subsumes(X,Y) :- \+ (X=Y),!,fail.
subsumes(X,Y) :- numbervars(Y,0,_),\+ (X=Y),!,fail.
subsumes(_,_).
***/

%   my_variant(Term1, Term2)
%   is true when Term1 and Term2 are alphabetic variants. The definition 
%   here is not quite right;  if Term1 and Term2 share any variables it 
%   may give the wrong answer. It is only meant to be used when the two 
%   terms have no variables in common. 

/*** A naive first version */
my_variant(Term1, Term2) :-
    subsumes_chk(Term1, Term2),
    subsumes_chk(Term2, Term1).

result([length([list],[A],[A]), length([list],[num],[num]),
	length([list],[num]), concat([list],[A],[A]),
	concat([list],[A],[A,list]), my_atom([atom]),
	match([list],[list]), p_match([list],[A]),
	p_match([A,list],[list]), p_investigate([list],[A]),
	p_investigate([list],[list]), p_investigate([list],[A,list]),
	my_arg([num],[other],[A]), my_arg([num],[A,list],[B]),
	my_functor([other],[atom],[list]),
	my_functor([A,list],[atom],[list]),
	property([A,list],[atom],[B]), get_pats([num],[A],[list],[B]),
	get_pats([num],[A,list],[list],[B]),
	get_pats([num],[list],[list],[A]),
	get_pats([num],[list],[list],[A,list]),
	get_pats([num],[list],[list],[list]),
	get_pats([num],[A],[list]), get_pats([num],[A,list],[list]),
	get_pats([num],[list],[list]), investigate([list],[A]),
	investigate([list],[list]), investigate([list],[A,list]),
	split([num],[A,list],[B],[A]),
	split([num],[A,list],[B],[A,list]),
	randomize([list],[list],[A]), randomize([list],[list],[num]),
	fill([num],[A],[A]), fill([num],[A],[A,list]),
	test([num],[num],[num]), test([num],[num],[A]),
	init([num],[A],[B],[C],[D],[_]),
	init([num],[num],[num],[num],[A],[B,list]),
	init([num],[num],[num],[num],[A,list],[B,list]),
	init([num],[num],[num],[num],[list],[A,list]),
	init([num],[A],[B],[C],[D]),
	init([num],[num],[num],[A],[B,list]),
	init([num],[num],[num],[A,list],[B,list]),
	init([num],[num],[num],[list],[A,list]), main([A],[B]),
	main([A],[list]), main([A],[B,list]), main([A,list],[B]),
	main([A,list],[list]), main([A,list],[B,list]),
	main([list],[A]), main([list],[list]),
	main([list],[A,list])]).
