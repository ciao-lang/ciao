:- module(kalah, 
	[
	    tp/0,
	    test/1, 
	    result/1,
	    spend_time/2
	]).

:- include(tabling_type).

:- dynamic results/1.
:- set_prolog_flag(multi_arity_warnings, off).


:- table set_unify/2.
:- table pieces__1/1.
:- table initialize__1/3.
:- table lookahead__1/1.
:- table sumlist__1/3.
:- table sumlist__1/2.
:- table rev__1/3.
:- table reverse__1/2.
:- table nonzero__1/1.
:- table zero__1/1.
:- table writekalahs__1/2.
:- table writepile__1/1.
:- table displayholes__1/1.
:- table writestones__1/1.
:- table show__1/1.
:- table displaygame__1/2.
:- table swap__1/2.
:- table genlegal__1/1.
:- table legal__1/1.
:- table nextplayer__1/2.
:- table nsubstitute__1/4.
:- table nthmember__1/3.
:- table announce__1/1.
:- table gameover__1/3.
:- table value__1/2.
:- table distribute__1/3.
:- table pickupanddistribute__1/4.
:- table distributeyourholes__1/3.
:- table updatekalah__1/5.
:- table checkiffinished__1/2.
:- table checkcapture__1/7.
:- table distributemyholes__1/5.
:- table distributestones__1/4.
:- table move__1/3.
:- table extendmove__1/4.
:- table stonesinhole__1/3.
:- table member__1/2.
:- table move__1/2.
:- table cutoff__1/9.
:- table evaluateandchoose__1/7.
:- table allmoves__1/2.
:- table alphabeta__1/6.
:- table choosemove__1/3.
:- table play__1/3.
:- table play__1/2.

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

play__1(_394,_396) :- 
        [_329] = _398, [_364] = _400, 
        initialize([_329],[_356],[_360]),
        displaygame([_356],[_360]),
        play([_356],[_360],[_364]),
        normalize_result([_398,_400],
                         [_394,_396]).
play__1(_300,_302,_304) :- 
        [_260] = _306, [_264] = _308, [_276] = _310, 
        gameover([_260],[_264],[_276]),
        announce([_276]),
        normalize_result([_306,_308,_310],
                         [_300,_302,_304]).
play__1(_579,_581,_583) :- 
        [_502] = _585, [_525] = _587, [_544] = _589, 
        choosemove([_502],[_525],[_498]),
        move([_498],[_502],[_536]),
        displaygame([_536],[_525]),
        nextplayer([_525],[_540]),
        play([_536],[_540],[_544]),
        normalize_result([_585,_587,_589],
                         [_579,_581,_583]).
choosemove__1(_365,_367,_369) :- 
        [_320] = _371, [atom] = _373, [_332] = _375, 
        lookahead([_316]),
        alphabeta([_316],[_320],[num],[num],[_332],[_336]),
        normalize_result([_371,_373,_375],
                         [_365,_367,_369]).
choosemove__1(_203,_205,_207) :- 
        [_159] = _209, [atom] = _211, [_182] = _213, 
        genlegal([_182]),
        normalize_result([_209,_211,_213],
                         [_203,_205,_207]).
alphabeta__1(_315,_317,_319,_321,_323,_325) :- 
        [num] = _327, [_283] = _329, [_253] = _331, [_257] = _333, [_261] = _335, [_287] = _337, 
        value([_283],[_287]),
        normalize_result([_327,_329,_331,_333,_335,_337],
                         [_315,_317,_319,_321,_323,_325]).
alphabeta__1(_842,_844,_846,_848,_850,_852) :- 
        [_755] = _854, [_774] = _856, [_735] = _858, [_711] = _860, [_647] = _862, [_651] = _864, 
        'my >'([_755],[num]),
        allmoves([_774],[_770]),
        'my is'([_782],[[num],[_711]]),
        'my is'([_786],[[num],[_735]]),
        'my is'([_778],[[_755],[num]]),
        evaluateandchoose([_770],[_774],[_778],[_782],[_786],[atom],[other]),
        normalize_result([_854,_856,_858,_860,_862,_864],
                         [_842,_844,_846,_848,_850,_852]).
allmoves__1(_211,_213) :- 
        [_187] = _215, [list,list] = _217, 
        move([_187],[_191]),
        normalize_result([_215,_217],
                         [_211,_213]).
allmoves__1(_283,_285) :- 
        [_256] = _287, [list,_260] = _289, 
        move([_256],[_249]),
        allmoves([_256],[_260]),
        normalize_result([_287,_289],
                         [_283,_285]).
evaluateandchoose__1(_893,_895,_897,_899,_901,_903,_905) :- 
        [list,_835] = _907, [_839] = _909, [_823] = _911, [_827] = _913, [_831] = _915, [_843] = _917, [_847] = _919, 
        move([_815],[_839],[_764]),
        alphabeta([_823],[_764],[_827],[_831],[_776],[_804]),
        'my is'([_819],[[num],[_804]]),
        cutoff([_815],[_819],[_823],[_827],[_831],[_835],[_839],[_843],[_847]),
        normalize_result([_907,_909,_911,_913,_915,_917,_919],
                         [_893,_895,_897,_899,_901,_903,_905]).
evaluateandchoose__1(_261,_263,_265,_267,_269,_271,_273) :- 
        [list] = _275, [_213] = _277, [_217] = _279, [_221] = _281, 
	[_225] = _283, [_229] = _285, [other] = _287, 
        normalize_result([_275,_277,_279,_281,_283,_285,_287],
                          [_261,_263,_265,_267,_269,_271,_273]).

cutoff__1(_397,_399,_401,_403,_405,_407,_409,_411,_413) :- 
        [_299] = _415, [_352] = _417, [_307] = _419, [_311] = _421, [_361] = _423, [_319] = _425, [_323] = _427, [_327] = _429, [other] = _431, 
        'my >='([_352],[_361]),
        normalize_result([_415,_417,_419,_421,_423,_425,_427,_429,_431],
                         [_397,_399,_401,_403,_405,_407,_409,_411,_413]).
cutoff__1(_655,_657,_659,_661,_663,_665,_667,_669,_671) :- 
        [_606] = _673, [_598] = _675, [_594] = _677, [_550] = _679, [_602] = _681, [_586] = _683, [_590] = _685, [_525] = _687, [_610] = _689, 
        'my <'([_550],[_598]),
        'my <'([_598],[_602]),
        evaluateandchoose([_586],[_590],[_594],[_598],[_602],[_606],[_610]),
        normalize_result([_673,_675,_677,_679,_681,_683,_685,_687,_689],
                         [_655,_657,_659,_661,_663,_665,_667,_669,_671]).
cutoff__1(_594,_596,_598,_600,_602,_604,_606,_608,_610) :- 
        [_457] = _612, [_510] = _614, [_536] = _616, [_540] = _618, [_544] = _620, [_528] = _622, [_532] = _624, [_548] = _626, [_552] = _628, 
        'my =<'([_510],[_540]),
        evaluateandchoose([_528],[_532],[_536],[_540],[_544],[_548],[_552]),
        normalize_result([_612,_614,_616,_618,_620,_622,_624,_626,_628],
                         [_594,_596,_598,_600,_602,_604,_606,_608,_610]).
move__1(_482,_484) :- 
        [_450] = _486, [list,_454] = _488, 
        my_member([_446],[list,list,list,list,list,list,list]),
        stonesinhole([_446],[_450],[_442]),
        extendmove([_442],[_446],[_450],[_454]),
        normalize_result([_486,_488],
                         [_482,_484]).
move__1(_116,_118) :- 
        [other] = _120, [list] = _122, 
        normalize_result([_120,_122],
                          [_116,_118]).
member__1(_138,_140) :- 
        [_119] = _142, [list,_106] = _144, 
        normalize_result([_142,_144],
                          [_138,_140]).
member__1(_215,_217) :- 
        [_191] = _219, [list,_195] = _221, 
        my_member([_191],[_195]),
        normalize_result([_219,_221],
                         [_215,_217]).
stonesinhole__1(_312,_314,_316) :- 
        [_260] = _318, [other] = _320, [_276] = _322, 
        nthmember([_260],[_264],[_276]),
        'my >'([_276],[num]),
        normalize_result([_318,_320,_322],
                         [_312,_314,_316]).
extendmove__1(_282,_284,_286,_288) :- 
        [_241] = _290, [_254] = _292, [_221] = _294, [list] = _296, 
        'my =='([_241],[[num],[_254]]),
        normalize_result([_290,_292,_294,_296],
                         [_282,_284,_286,_288]).
extendmove__1(_472,_474,_476,_478) :- 
        [_417] = _480, [_421] = _482, [_425] = _484, [_442] = _486, 
        'my =:='([_417],[[num],[_421]]),
        distributestones([_417],[_421],[_425],[_438]),
        move([_438],[_442]),
        normalize_result([_480,_482,_484,_486],
                         [_472,_474,_476,_478]).
move__1(_482,_484,_486) :- 
        [list,_445] = _488, [_432] = _490, [_453] = _492, 
        stonesinhole([_428],[_432],[_424]),
        distributestones([_424],[_428],[_432],[_449]),
        move([_445],[_449],[_453]),
        normalize_result([_488,_490,_492],
                         [_482,_484,_486]).
move__1(_228,_230,_232) :- 
        [list] = _234, [_202] = _236, [_206] = _238, 
        swap([_202],[_206]),
        normalize_result([_234,_236,_238],
                         [_228,_230,_232]).
distributestones__1(_433,_435,_437,_439) :- 
        [_371] = _441, [_375] = _443, [_379] = _445, [_405] = _447, 
        distributemyholes([_371],[_375],[_379],[_401],[_397]),
        distributeyourholes([_397],[_401],[_405]),
        normalize_result([_441,_443,_445,_447],
                         [_433,_435,_437,_439]).
distributemyholes__1(_645,_647,_649,_651,_653) :- 
        [_596] = _655, [_600] = _657, [other] = _659, [other] = _661, [_587] = _663, 
        'my >'([_596],[[num],[_600]]),
        pickupanddistribute([_600],[_596],[_550],[_554]),
        'my is'([_563],[[_572],[num]]),
        'my is'([_587],[[[_596],[_600]],[num]]),
        normalize_result([_655,_657,_659,_661,_663],
                         [_645,_647,_649,_651,_653]).
distributemyholes__1(_740,_742,_744,_746,_748) :- 
        [_683] = _750, [_679] = _752, [other] = _754, [_705] = _756, [num] = _758, 
        pickupanddistribute([_679],[_683],[_626],[_647]),
        checkcapture([_679],[_683],[_647],[_651],[_655],[_659],[_675]),
        updatekalah([_675],[_679],[_683],[_687],[_691]),
        checkiffinished([other],[_705]),
        normalize_result([_750,_752,_754,_756,_758],
                         [_740,_742,_744,_746,_748]).
checkcapture__1(_939,_941,_943,_945,_947,_949,_951) :- 
        [_759] = _953, [_763] = _955, [_836] = _957, [_844] = _959, [_857] = _961, [_865] = _963, [_874] = _965, 
        'my is'([_853],[[_759],[_763]]),
        'my is'([_832],[[num],[_853]]),
        nthmember([_832],[_857],[_883]),
        'my >'([_883],[num]),
        nsubstitute([_832],[_836],[num],[_844]),
        nsubstitute([_853],[_857],[num],[_865]),
        'my is'([_874],[[_883],[num]]),
        normalize_result([_953,_955,_957,_959,_961,_963,_965],
                         [_939,_941,_943,_945,_947,_949,_951]).
checkcapture__1(_263,_265,_267,_269,_271,_273,_275) :- 
        [_211] = _277, [_215] = _279, [_223] = _281, [_223] = _283, [_231] = _285, [_231] = _287, [num] = _289, 
        normalize_result([_277,_279,_281,_283,_285,_287,_289],
                          [_263,_265,_267,_269,_271,_273,_275]).
checkiffinished__1(_339,_341) :- 
        [other] = _343, [other] = _345, 
        zero([_279]),
        sumlist([_285],[_309]),
        'my is'([_296],[[_305],[_309]]),
        normalize_result([_343,_345],
                         [_339,_341]).
checkiffinished__1(_339,_341) :- 
        [other] = _343, [other] = _345, 
        zero([_279]),
        sumlist([_285],[_309]),
        'my is'([_296],[[_305],[_309]]),
        normalize_result([_343,_345],
                         [_339,_341]).
checkiffinished__1(_126,_128) :- 
        [_113] = _130, [_113] = _132, 
        normalize_result([_130,_132],
                          [_126,_128]).
updatekalah__1(_309,_311,_313,_315,_317) :- 
        [num] = _319, [_266] = _321, [_279] = _323, [_249] = _325, [_249] = _327, 
        'my <'([_266],[[num],[_279]]),
        normalize_result([_319,_321,_323,_325,_327],
                         [_309,_311,_313,_315,_317]).
updatekalah__1(_402,_404,_406,_408,_410) :- 
        [num] = _412, [_332] = _414, [_345] = _416, [_365] = _418, [_356] = _420, 
        'my =:='([_332],[[num],[_345]]),
        'my is'([_356],[[_365],[num]]),
        normalize_result([_412,_414,_416,_418,_420],
                         [_402,_404,_406,_408,_410]).
updatekalah__1(_378,_380,_382,_384,_386) :- 
        [_345] = _388, [_285] = _390, [_289] = _392, [_341] = _394, [_332] = _396, 
        'my >'([_345],[num]),
        'my is'([_332],[[_341],[_345]]),
        normalize_result([_388,_390,_392,_394,_396],
                         [_378,_380,_382,_384,_386]).
distributeyourholes__1(_149,_151,_153) :- 
        [num] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
distributeyourholes__1(_408,_410,_412) :- 
        [_368] = _414, [other] = _416, [other] = _418, 
        'my =<'([num],[_368]),
        'my =<'([_368],[num]),
        nonzero([_362]),
        distribute([_368],[_372],[_376]),
        normalize_result([_414,_416,_418],
                         [_408,_410,_412]).
distributeyourholes__1(_507,_509,_511) :- 
        [_447] = _513, [other] = _515, [other] = _517, 
        'my >'([_447],[num]),
        distribute([num],[_426],[_430]),
        'my is'([_462],[[_447],[num]]),
        distributestones([_462],[num],[other],[_474]),
        normalize_result([_513,_515,_517],
                         [_507,_509,_511]).
distributeyourholes__1(_402,_404,_406) :- 
        [_360] = _408, [other] = _410, [other] = _412, 
        zero([_334]),
        sumlist([_340],[_364]),
        'my is'([_351],[[[_360],[_364]],[_370]]),
        normalize_result([_408,_410,_412],
                         [_402,_404,_406]).
pickupanddistribute__1(_302,_304,_306,_308) :- 
        [num] = _310, [_269] = _312, [list,_273] = _314, [list,_277] = _316, 
        distribute([_269],[_273],[_277]),
        normalize_result([_310,_312,_314,_316],
                         [_302,_304,_306,_308]).
pickupanddistribute__1(_483,_485,_487,_489) :- 
        [_424] = _491, [_443] = _493, [list,_447] = _495, [list,_451] = _497, 
        'my >'([_424],[num]),
        'my is'([_439],[[_424],[num]]),
        pickupanddistribute([_439],[_443],[_447],[_451]),
        normalize_result([_491,_493,_495,_497],
                         [_483,_485,_487,_489]).
distribute__1(_149,_151,_153) :- 
        [num] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
distribute__1(_524,_526,_528) :- 
        [_445] = _530, [list,_488] = _532, [list,_492] = _534, 
        'my >'([_445],[num]),
        'my is'([_484],[[_445],[num]]),
        'my is'([_460],[[_469],[num]]),
        distribute([_484],[_488],[_492]),
        normalize_result([_530,_532,_534],
                         [_524,_526,_528]).
distribute__1(_145,_147,_149) :- 
        [_121] = _151, [list] = _153, [list] = _155, 
        normalize_result([_151,_153,_155],
                          [_145,_147,_149]).
value__1(_232,_234) :- 
        [other] = _236, [_195] = _238, 
        'my is'([_195],[[_204],[_208]]),
        normalize_result([_236,_238],
                         [_232,_234]).
gameover__1(_292,_294,_296) :- 
        [other] = _298, [_225] = _300, [atom] = _302, 
        pieces([_263]),
        'my =:='([_250],[[num],[_263]]),
        normalize_result([_298,_300,_302],
                         [_292,_294,_296]).
gameover__1(_296,_298,_300) :- 
        [other] = _302, [_233] = _304, [_233] = _306, 
        pieces([_267]),
        'my >'([_254],[[num],[_267]]),
        normalize_result([_302,_304,_306],
                         [_296,_298,_300]).
gameover__1(_364,_366,_368) :- 
        [other] = _370, [_332] = _372, [_336] = _374, 
        pieces([_321]),
        'my >'([_308],[[num],[_321]]),
        nextplayer([_332],[_336]),
        normalize_result([_370,_372,_374],
                         [_364,_366,_368]).
announce__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
announce__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
announce__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
nthmember__1(_419,_421,_423) :- 
        [_367] = _425, [list,_386] = _427, [_390] = _429, 
        'my >'([_367],[num]),
        'my is'([_382],[[_367],[num]]),
        nthmember([_382],[_386],[_390]),
        normalize_result([_425,_427,_429],
                         [_419,_421,_423]).
nthmember__1(_161,_163,_165) :- 
        [num] = _167, [list,_100] = _169, [_145] = _171, 
        normalize_result([_167,_169,_171],
                          [_161,_163,_165]).
nsubstitute__1(_198,_200,_202,_204) :- 
        [num] = _206, [list,_100] = _208, [_173] = _210, [list,_100] = _212, 
        normalize_result([_206,_208,_210,_212],
                          [_198,_200,_202,_204]).
nsubstitute__1(_483,_485,_487,_489) :- 
        [_424] = _491, [list,_443] = _493, [_447] = _495, [list,_451] = _497, 
        'my >'([_424],[num]),
        'my is'([_439],[[_424],[num]]),
        nsubstitute([_439],[_443],[_447],[_451]),
        normalize_result([_491,_493,_495,_497],
                         [_483,_485,_487,_489]).
nextplayer__1(_116,_118) :- 
        [atom] = _120, [atom] = _122, 
        normalize_result([_120,_122],
                          [_116,_118]).
nextplayer__1(_116,_118) :- 
        [atom] = _120, [atom] = _122, 
        normalize_result([_120,_122],
                          [_116,_118]).
legal__1(_277) :- 
        [list,_254] = _279, 
        'my <'([num],[_236]),
        'my <'([_236],[num]),
        legal([_254]),
        normalize_result([_279],
                         [_277]).
legal__1(_93) :- 
        [list] = _95, 
        normalize_result([_95],
                          [_93]).
genlegal__1(_285) :- 
        [list,_265] = _287, 
        my_member([_242],[list,list,list,list,list,list,list]),
        genlegal([_265]),
        normalize_result([_287],
                         [_285]).
genlegal__1(_93) :- 
        [list] = _95, 
        normalize_result([_95],
                          [_93]).
swap__1(_116,_118) :- 
        [other] = _120, [other] = _122, 
        normalize_result([_120,_122],
                          [_116,_118]).
displaygame__1(_174,_176) :- 
        [_155] = _178, [atom] = _180, 
        show([_155]),
        normalize_result([_178,_180],
                         [_174,_176]).
displaygame__1(_242,_244) :- 
        [_209] = _246, [atom] = _248, 
        swap([_209],[_220]),
        show([_220]),
        normalize_result([_246,_248],
                         [_242,_244]).
show__1(_328) :- 
        [other] = _330, 
        my_reverse([_274],[_285]),
        writestones([_285]),
        writekalahs([_291],[_295]),
        writestones([_302]),
        normalize_result([_330],
                         [_328]).
writestones__1(_151) :- 
        [_134] = _153, 
        displayholes([_134]),
        normalize_result([_153],
                         [_151]).
displayholes__1(_204) :- 
        [list,_184] = _206, 
        writepile([_178]),
        displayholes([_184]),
        normalize_result([_206],
                         [_204]).
displayholes__1(_93) :- 
        [list] = _95, 
        normalize_result([_95],
                          [_93]).
writepile__1(_167) :- 
        [_138] = _169, 
        'my <'([_138],[num]),
        normalize_result([_169],
                         [_167]).
writepile__1(_167) :- 
        [_138] = _169, 
        'my >='([_138],[num]),
        normalize_result([_169],
                         [_167]).
writekalahs__1(_128,_130) :- 
        [_111] = _132, [_115] = _134, 
        normalize_result([_132,_134],
                          [_128,_130]).
zero__1(_153) :- 
        [list,list,list,list,list,list,list] = _155, 
        normalize_result([_155],
                          [_153]).
nonzero__1(_227) :- 
        [_186] = _229, 
        'my ==='([_186],[list,list,list,list,list,list,list]),
        normalize_result([_229],
                         [_227]).
reverse__1(_226,_228) :- 
        [_197] = _230, [_205] = _232, 
        rev([_197],[list],[_205]),
        normalize_result([_230,_232],
                         [_226,_228]).
rev__1(_149,_151,_153) :- 
        [list] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
rev__1(_279,_281,_283) :- 
        [list,_246] = _285, [_227] = _287, [_256] = _289, 
        rev([_246],[list,_227],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
sumlist__1(_226,_228) :- 
        [_197] = _230, [_205] = _232, 
        sumlist([_197],[num],[_205]),
        normalize_result([_230,_232],
                         [_226,_228]).
sumlist__1(_149,_151,_153) :- 
        [list] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
sumlist__1(_362,_364,_366) :- 
        [list,_328] = _368, [_313] = _370, [_336] = _372, 
        'my is'([_332],[[_313],[num]]),
        sumlist([_328],[_332],[_336]),
        normalize_result([_368,_370,_372],
                         [_362,_364,_366]).
lookahead__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).
lookahead__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).
initialize__1(_139,_141,_143) :- 
        [atom] = _145, [other] = _147, [atom] = _149, 
        normalize_result([_145,_147,_149],
                          [_139,_141,_143]).
initialize__1(_139,_141,_143) :- 
        [atom] = _145, [other] = _147, [atom] = _149, 
        normalize_result([_145,_147,_149],
                          [_139,_141,_143]).
pieces__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).
pieces__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).

%---------------- Definitions of tabled preds --------------
pieces(_63) :- 
        pieces__1(_65),
        unify_sets([_63], [_65]).
initialize(_63,_65,_67) :- 
        initialize__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
lookahead(_63) :- 
        lookahead__1(_65),
        unify_sets([_63], [_65]).
sumlist(_63,_65,_67) :- 
        sumlist__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
sumlist(_63,_65) :- 
        sumlist__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
rev(_63,_65,_67) :- 
        rev__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
my_reverse(_63,_65) :- 
        reverse__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
nonzero(_63) :- 
        nonzero__1(_65),
        unify_sets([_63], [_65]).
zero(_63) :- 
        zero__1(_65),
        unify_sets([_63], [_65]).
writekalahs(_63,_65) :- 
        writekalahs__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
writepile(_63) :- 
        writepile__1(_65),
        unify_sets([_63], [_65]).
displayholes(_63) :- 
        displayholes__1(_65),
        unify_sets([_63], [_65]).
writestones(_63) :- 
        writestones__1(_65),
        unify_sets([_63], [_65]).
show(_63) :- 
        show__1(_65),
        unify_sets([_63], [_65]).
displaygame(_63,_65) :- 
        displaygame__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
swap(_63,_65) :- 
        swap__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
genlegal(_63) :- 
        genlegal__1(_65),
        unify_sets([_63], [_65]).
legal(_63) :- 
        legal__1(_65),
        unify_sets([_63], [_65]).
nextplayer(_63,_65) :- 
        nextplayer__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
nsubstitute(_63,_65,_67,_69) :- 
        nsubstitute__1(_71,_73,_75,_77),
        unify_sets([_63,_65,_67,_69], [_71,_73,_75,_77]).
nthmember(_63,_65,_67) :- 
        nthmember__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
announce(_63) :- 
        announce__1(_65),
        unify_sets([_63], [_65]).
gameover(_63,_65,_67) :- 
        gameover__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
value(_63,_65) :- 
        value__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
distribute(_63,_65,_67) :- 
        distribute__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
pickupanddistribute(_63,_65,_67,_69) :- 
        pickupanddistribute__1(_71,_73,_75,_77),
        unify_sets([_63,_65,_67,_69], [_71,_73,_75,_77]).
distributeyourholes(_63,_65,_67) :- 
        distributeyourholes__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
updatekalah(_63,_65,_67,_69,_71) :- 
        updatekalah__1(_73,_75,_77,_79,_81),
        unify_sets([_63,_65,_67,_69,_71], [_73,_75,_77,_79,_81]).
checkiffinished(_63,_65) :- 
        checkiffinished__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
checkcapture(_63,_65,_67,_69,_71,_73,_75) :- 
        checkcapture__1(_77,_79,_81,_83,_85,_87,_89),
        unify_sets([_63,_65,_67,_69,_71,_73,_75], [_77,_79,_81,_83,_85,_87,_89]).
distributemyholes(_63,_65,_67,_69,_71) :- 
        distributemyholes__1(_73,_75,_77,_79,_81),
        unify_sets([_63,_65,_67,_69,_71], [_73,_75,_77,_79,_81]).
distributestones(_63,_65,_67,_69) :- 
        distributestones__1(_71,_73,_75,_77),
        unify_sets([_63,_65,_67,_69], [_71,_73,_75,_77]).
move(_63,_65,_67) :- 
        move__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
extendmove(_63,_65,_67,_69) :- 
        extendmove__1(_71,_73,_75,_77),
        unify_sets([_63,_65,_67,_69], [_71,_73,_75,_77]).
stonesinhole(_63,_65,_67) :- 
        stonesinhole__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
my_member(_63,_65) :- 
        member__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
move(_63,_65) :- 
        move__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).

cutoff(_63,_65,_67,_69,_71,_73,_75,_77,_79) :- 
        cutoff__1(_81,_83,_85,_87,_89,_91,_93,_95,_97),
        unify_sets([_63,_65,_67,_69,_71,_73,_75,_77,_79], [_81,_83,_85,_87,_89,_91,_93,_95,_97]).
evaluateandchoose(_63,_65,_67,_69,_71,_73,_75) :- 
        evaluateandchoose__1(_77,_79,_81,_83,_85,_87,_89),
        unify_sets([_63,_65,_67,_69,_71,_73,_75], [_77,_79,_81,_83,_85,_87,_89]).
allmoves(_63,_65) :- 
        allmoves__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
alphabeta(_63,_65,_67,_69,_71,_73) :- 
        alphabeta__1(_75,_77,_79,_81,_83,_85),
        unify_sets([_63,_65,_67,_69,_71,_73], [_75,_77,_79,_81,_83,_85]).
choosemove(_63,_65,_67) :- 
        choosemove__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
play(_63,_65,_67) :- 
        play__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
play(_63,_65) :- 
        play__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).

%---------------- Tp ---------------------------------------

tp :- pieces__1(_64), fail.
tp :- initialize__1(_64,_66,_68), fail.
tp :- lookahead__1(_64), fail.
tp :- sumlist__1(_64,_66,_68), fail.
tp :- sumlist__1(_64,_66), fail.
tp :- rev__1(_64,_66,_68), fail.
tp :- reverse__1(_64,_66), fail.
tp :- nonzero__1(_64), fail.
tp :- zero__1(_64), fail.
tp :- writekalahs__1(_64,_66), fail.
tp :- writepile__1(_64), fail.
tp :- displayholes__1(_64), fail.
tp :- writestones__1(_64), fail.
tp :- show__1(_64), fail.
tp :- displaygame__1(_64,_66), fail.
tp :- swap__1(_64,_66), fail.
tp :- genlegal__1(_64), fail.
tp :- legal__1(_64), fail.
tp :- nextplayer__1(_64,_66), fail.
tp :- nsubstitute__1(_64,_66,_68,_70), fail.
tp :- nthmember__1(_64,_66,_68), fail.
tp :- announce__1(_64), fail.
tp :- gameover__1(_64,_66,_68), fail.
tp :- value__1(_64,_66), fail.
tp :- distribute__1(_64,_66,_68), fail.
tp :- pickupanddistribute__1(_64,_66,_68,_70), fail.
tp :- distributeyourholes__1(_64,_66,_68), fail.
tp :- updatekalah__1(_64,_66,_68,_70,_72), fail.
tp :- checkiffinished__1(_64,_66), fail.
tp :- checkcapture__1(_64,_66,_68,_70,_72,_74,_76), fail.
tp :- distributemyholes__1(_64,_66,_68,_70,_72), fail.
tp :- distributestones__1(_64,_66,_68,_70), fail.
tp :- move__1(_64,_66,_68), fail.
tp :- extendmove__1(_64,_66,_68,_70), fail.
tp :- stonesinhole__1(_64,_66,_68), fail.
tp :- member__1(_64,_66), fail.
tp :- move__1(_64,_66), fail.
tp :- cutoff__1(_64,_66,_68,_70,_72,_74,_76,_78,_80), fail.
tp :- evaluateandchoose__1(_64,_66,_68,_70,_72,_74,_76), fail.
tp :- allmoves__1(_64,_66), fail.
tp :- alphabeta__1(_64,_66,_68,_70,_72,_74), fail.
tp :- choosemove__1(_64,_66,_68), fail.
tp :- play__1(_64,_66,_68), fail.
tp :- play__1(_64,_66), fail.
tp.


%---------------- Builtin Preds ----------------------------

'my ='(X1,X2) :- 'my =__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my ==='(X1,X2) :- 'my ===__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my =='(X1,X2) :- 'my ==__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my is'(X1,X2) :- 'my is__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my >='([num],[num]).
'my =<'([num],[num]).
'my =:='(X1,X2) :- 'my =:=__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my <'(X1,X2) :- 'my <__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my >'(X1,X2) :- 'my >__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).

'my =__1'(X,X).
'my ===__1'(_,_).
'my ==__1'(num,num).
'my is__1'(num,num).
'my <__1'(num,num).
'my >__1'(num,num).
'my =:=__1'(num,num).


%---------------- Show Result ------------------------------

show_facts :- pieces__1(_63),
              numbervars([_63]),
              assert(results(pieces(_63))), fail.
show_facts :- initialize__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(initialize(_63,_65,_67))), fail.
show_facts :- lookahead__1(_63),
              numbervars([_63]),
              assert(results(lookahead(_63))), fail.
show_facts :- sumlist__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(sumlist(_63,_65,_67))), fail.
show_facts :- sumlist__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(sumlist(_63,_65))), fail.
show_facts :- rev__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(rev(_63,_65,_67))), fail.
show_facts :- reverse__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(reverse(_63,_65))), fail.
show_facts :- nonzero__1(_63),
              numbervars([_63]),
              assert(results(nonzero(_63))), fail.
show_facts :- zero__1(_63),
              numbervars([_63]),
              assert(results(zero(_63))), fail.
show_facts :- writekalahs__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(writekalahs(_63,_65))), fail.
show_facts :- writepile__1(_63),
              numbervars([_63]),
              assert(results(writepile(_63))), fail.
show_facts :- displayholes__1(_63),
              numbervars([_63]),
              assert(results(displayholes(_63))), fail.
show_facts :- writestones__1(_63),
              numbervars([_63]),
              assert(results(writestones(_63))), fail.
show_facts :- show__1(_63),
              numbervars([_63]),
              assert(results(show(_63))), fail.
show_facts :- displaygame__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(displaygame(_63,_65))), fail.
show_facts :- swap__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(swap(_63,_65))), fail.
show_facts :- genlegal__1(_63),
              numbervars([_63]),
              assert(results(genlegal(_63))), fail.
show_facts :- legal__1(_63),
              numbervars([_63]),
              assert(results(legal(_63))), fail.
show_facts :- nextplayer__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(nextplayer(_63,_65))), fail.
show_facts :- nsubstitute__1(_63,_65,_67,_69),
              numbervars([_63,_65,_67,_69]),
              assert(results(nsubstitute(_63,_65,_67,_69))), fail.
show_facts :- nthmember__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(nthmember(_63,_65,_67))), fail.
show_facts :- announce__1(_63),
              numbervars([_63]),
              assert(results(announce(_63))), fail.
show_facts :- gameover__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(gameover(_63,_65,_67))), fail.
show_facts :- value__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(value(_63,_65))), fail.
show_facts :- distribute__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(distribute(_63,_65,_67))), fail.
show_facts :- pickupanddistribute__1(_63,_65,_67,_69),
              numbervars([_63,_65,_67,_69]),
              assert(results(pickupanddistribute(_63,_65,_67,_69))), fail.
show_facts :- distributeyourholes__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(distributeyourholes(_63,_65,_67))), fail.
show_facts :- updatekalah__1(_63,_65,_67,_69,_71),
              numbervars([_63,_65,_67,_69,_71]),
              assert(results(updatekalah(_63,_65,_67,_69,_71))), fail.
show_facts :- checkiffinished__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(checkiffinished(_63,_65))), fail.
show_facts :- checkcapture__1(_63,_65,_67,_69,_71,_73,_75),
              numbervars([_63,_65,_67,_69,_71,_73,_75]),
              assert(results(checkcapture(_63,_65,_67,_69,_71,_73,_75))), fail.
show_facts :- distributemyholes__1(_63,_65,_67,_69,_71),
              numbervars([_63,_65,_67,_69,_71]),
              assert(results(distributemyholes(_63,_65,_67,_69,_71))), fail.
show_facts :- distributestones__1(_63,_65,_67,_69),
              numbervars([_63,_65,_67,_69]),
              assert(results(distributestones(_63,_65,_67,_69))), fail.
show_facts :- move__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(move(_63,_65,_67))), fail.
show_facts :- extendmove__1(_63,_65,_67,_69),
              numbervars([_63,_65,_67,_69]),
              assert(results(extendmove(_63,_65,_67,_69))), fail.
show_facts :- stonesinhole__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(stonesinhole(_63,_65,_67))), fail.
show_facts :- member__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(member(_63,_65))), fail.
show_facts :- move__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(move(_63,_65))), fail.
show_facts :- cutoff__1(_63,_65,_67,_69,_71,_73,_75,_77,_79),
              numbervars([_63,_65,_67,_69,_71,_73,_75,_77,_79]),
              assert(results(cutoff(_63,_65,_67,_69,_71,_73,_75,_77,_79))), fail.
show_facts :- evaluateandchoose__1(_63,_65,_67,_69,_71,_73,_75),
          numbervars([_63,_65,_67,_69,_71,_73,_75]),
          assert(results(evaluateandchoose(_63,_65,_67,_69,_71,_73,_75))), fail.
show_facts :- allmoves__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(allmoves(_63,_65))), fail.
show_facts :- alphabeta__1(_63,_65,_67,_69,_71,_73),
              numbervars([_63,_65,_67,_69,_71,_73]),
              assert(results(alphabeta(_63,_65,_67,_69,_71,_73))), fail.
show_facts :- choosemove__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(choosemove(_63,_65,_67))), fail.
show_facts :- play__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(play(_63,_65,_67))), fail.
show_facts :- play__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(play(_63,_65))), fail.
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
	length(ListOfSets, NumberOfSets),
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

result([pieces([num]),
	initialize([atom],[other],[atom]),
	lookahead([num]),
	sumlist([list],[A],[A]),
	sumlist([list],[num],[num]),
	sumlist([list],[num]),
	rev([list],[A],[A]),
	rev([list],[A],[A,list]),
	reverse([list],[list]),
	nonzero([A]),
	zero([list]),
	writekalahs([A],[B]),
	writepile([num]),
	displayholes([list]),
	writestones([list]),
	show([other]),
	displaygame([other],[atom]),
	swap([other],[other]),
	genlegal([list]),
	legal([list]),
	nextplayer([atom],[atom]),
	nsubstitute([num],[A,list],[B],[A,list]),
	nthmember([num],[A,list],[B]),
	announce([atom]),
	gameover([other],[A],[atom]),
	gameover([other],[A],[A]),
	gameover([other],[atom],[atom]),
	value([other],[num]),
	distribute([num],[A],[A]),
	distribute([num],[A,list],[A,list]),
	distribute([A],[list],[list]),
	distribute([num],[list],[list]),
	pickupanddistribute([num],[num],[A,list],[A,list]),
	pickupanddistribute([num],[A],[list],[list]),
	pickupanddistribute([num],[num],[list],[list]),
	distributeyourholes([num],[A],[A]),
	distributeyourholes([num],[other],[other]),
	updatekalah([num],[num],[num],[A],[A]),
	updatekalah([num],[num],[num],[num],[num]),
	updatekalah([num],[A],[B],[num],[num]),
	checkiffinished([other],[other]),checkiffinished([A],[A]),
	checkcapture([num],[num],[A,list],[A,list],[B,list],[B,list],[num]),
	checkcapture([num],[num],[A,list],[A,list],[list],[list],[num]),
	checkcapture([A],[B],[C],[C],[D],[D],[num]),
	distributemyholes([num],[num],[other],[other],[num]),
	distributemyholes([A],[num],[other],[other],[num]),
	distributestones([num],[num],[other],[other]),
	distributestones([A],[num],[other],[other]),
	move([list],[other],[other]),
	extendmove([num],[num],[A],[list]),
	extendmove([num],[num],[other],[list]),
	stonesinhole([num],[other],[num]),
	member([A],[B,list]),
	move([other],[list]),
	cutoff([A],[num],[B],[C],[num],[D],[E],[_],[other]),
	cutoff([A],[num],[num],[num],[num],[B,list],[other],[C],[other]),
	cutoff([A],[num],[B],[num],[num],[list],[C],[D],[other]),
	cutoff([A],[num],[B],[num],[C],[list],[D],[E],[other]),
	cutoff([A],[num],[num],[num],[num],[list],[other],[B],[other]),
	cutoff([A],[num],[num],[num],[B],[list],[other],[C],[other]),
	evaluateandchoose([A,list],[other],[num],[B],[num],[C],[other]),
	evaluateandchoose([list],[A],[B],[C],[D],[E],[other]),
	evaluateandchoose([A,list],[other],[num],[num],[num],[B],[other]),
	evaluateandchoose([list],[other],[num],[num],[num],[A],[other]),
	evaluateandchoose([list],[other],[num],[num],[A],[B],[other]),
	allmoves([other],[list]),
	alphabeta([num],[other],[A],[B],[C],[num]),
	alphabeta([num],[other],[num],[num],[A],[B]),
	choosemove([other],[atom],[A]),
	choosemove([A],[atom],[list]),
	play([other],[A],[atom]),
	play([other],[atom],[atom]),
	play([atom],[atom])]).
