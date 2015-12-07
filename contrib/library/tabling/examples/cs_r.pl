:- module(cs_r, 
	[
	    test/1, 
	    result/1,
	    spend_time/2
	]).

:- include(tabling_type).

:- dynamic results/1.
:- set_prolog_flag(multi_arity_warnings, off).

 %% :- bridge /(ground_list,1).
 %% :- bridge /(my_member,2).
 %% :- bridge /(my_append,3).
 %% :- bridge /(find,3).
 %% :- bridge /(addliste,3).
 %% :- bridge /(addelement,3).
 %% :- bridge /(construct,3).
 %% :- bridge /(allsplit,2).
 %% :- bridge /(split,3).
 %% :- bridge /(genempty,2).
 %% :- bridge /(combcutaux,6).
 %% :- bridge /(nobeter,2).
 %% :- bridge /(generatecut,9).
 %% :- bridge /(generatecutaux,9).
 %% :- bridge /(gencut,4).

:- table /(set_unify,2).
:- table /(ground__1,1).
:- table /(quantity__1,1).
:- table /(empty__1,1).
:- table /(cuth__1,1).
:- table /(cutv__1,1).
:- table /(wasteh__1,1).
:- table /(wastev__1,1).
:- table /(vertical__1,1).
:- table /(ground_list__1,1).
:- table /(horizontal__1,1).
:- table /(member__1,2).
:- table /(append__1,3).
:- table /(find__1,3).
:- table /(addliste__1,3).
:- table /(addelement__1,3).
:- table /(construct__1,3).
:- table /(intersection__1,2).
:- table /(split__1,2).
:- table /(allsplit__1,2).
:- table /(split__1,3).
:- table /(genempty__1,2).
:- table /(emptyconfig__1,1).
:- table /(combcutaux__1,6).
:- table /(combcut__1,6).
:- table /(combine__1,4).
:- table /(combination__1,3).
:- table /(nobeter__1,2).
:- table /(nobeter__1,3).
:- table /(generatecut__1,6).
:- table /(generatecutaux__1,9).
:- table /(generatecut__1,9).
:- table /(cut__1,5).
:- table /(gencut__1,4).
:- table /(horizontalcut__1,1).
:- table /(verticalcut__1,1).
:- table /(configuration__1,1).
:- table /(pgenconfig__1,1).

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

pgenconfig__1(_151) :- 
        [_134] = _153, 
        configuration([_134]),
        normalize_result([_153],
                         [_151]).
pgenconfig__1(_151) :- 
        [_134] = _153, 
        configuration([_134]),
        normalize_result([_153],
                         [_151]).
configuration__1(_287) :- 
        [_262] = _289, 
        verticalcut([_254]),
        horizontalcut([_258]),
        combination([_254],[_258],[_262]),
        normalize_result([_289],
                         [_287]).
configuration__1(_287) :- 
        [_262] = _289, 
        verticalcut([_254]),
        horizontalcut([_258]),
        combination([_254],[_258],[_262]),
        normalize_result([_289],
                         [_287]).
verticalcut__1(_423) :- 
        [_390] = _425, 
        cutv([_382]),
        wastev([_386]),
        horizontal([_374]),
        vertical([_378]),
        cut([_374],[_378],[_382],[_386],[_390]),
        normalize_result([_425],
                         [_423]).
verticalcut__1(_423) :- 
        [_390] = _425, 
        cutv([_382]),
        wastev([_386]),
        horizontal([_374]),
        vertical([_378]),
        cut([_374],[_378],[_382],[_386],[_390]),
        normalize_result([_425],
                         [_423]).
horizontalcut__1(_355) :- 
        [_326] = _357, 
        cuth([_314]),
        horizontal([_318]),
        wasteh([_322]),
        gencut([_314],[_318],[_322],[_326]),
        normalize_result([_357],
                         [_355]).
horizontalcut__1(_355) :- 
        [_326] = _357, 
        cuth([_314]),
        horizontal([_318]),
        wasteh([_322]),
        gencut([_314],[_318],[_322],[_326]),
        normalize_result([_357],
                         [_355]).
gencut__1(_174,_176,_178,_180) :- 
        [list] = _182, [_147] = _184, [_151] = _186, [list] = _188, 
        normalize_result([_182,_184,_186,_188],
                          [_174,_176,_178,_180]).
gencut__1(_480,_482,_484,_486) :- 
        [list,_439] = _488, [_443] = _490, [_447] = _492, [list,_451] = _494, 
        cut([_413],[_443],[_421],[_447],[_429]),
        gencut([_439],[_443],[_447],[_451]),
        normalize_result([_488,_490,_492,_494],
                         [_480,_482,_484,_486]).
gencut__1(_174,_176,_178,_180) :- 
        [list] = _182, [_147] = _184, [_151] = _186, [list] = _188, 
        normalize_result([_182,_184,_186,_188],
                          [_174,_176,_178,_180]).
gencut__1(_480,_482,_484,_486) :- 
        [list,_439] = _488, [_443] = _490, [_447] = _492, [list,_451] = _494, 
        cut([_413],[_443],[_421],[_447],[_429]),
        gencut([_439],[_443],[_447],[_451]),
        normalize_result([_488,_490,_492,_494],
                         [_480,_482,_484,_486]).
cut__1(_984,_986,_988,_990,_992) :- 
        [_912] = _994, [_908] = _996, [_880] = _998, [_839] = _1000, [list,list,list] = _1002, 
        horizontal([_930]),
        vertical([_934]),
        'my is'([_872],[[[_908],[_912]],[[[[_839],[_930]],[_934]],[num]]]),
        generatecut([_912],[_872],[_908],[_880],[_884],[_918]),
        'my is'([_899],[[[[[_908],[_912]],[_918]],[num]],[[[_930],[_934]],[num]]]),
        normalize_result([_994,_996,_998,_1000,_1002],
                         [_984,_986,_988,_990,_992]).
cut__1(_984,_986,_988,_990,_992) :- 
        [_912] = _994, [_908] = _996, [_880] = _998, [_839] = _1000, [list,list,list] = _1002, 
        horizontal([_930]),
        vertical([_934]),
        'my is'([_872],[[[_908],[_912]],[[[[_839],[_930]],[_934]],[num]]]),
        generatecut([_912],[_872],[_908],[_880],[_884],[_918]),
        'my is'([_899],[[[[[_908],[_912]],[_918]],[num]],[[[_930],[_934]],[num]]]),
        normalize_result([_994,_996,_998,_1000,_1002],
                         [_984,_986,_988,_990,_992]).
generatecut__1(_484,_486,_488,_490,_492,_494) :- 
        [_417] = _496, [_421] = _498, [_425] = _500, [_437] = _502, [_441] = _504, [_445] = _506, 
        generatecut([_417],[_421],[_425],[num],[num],[_437],[_441],[_445],[num]),
        normalize_result([_496,_498,_500,_502,_504,_506],
                         [_484,_486,_488,_490,_492,_494]).
generatecut__1(_484,_486,_488,_490,_492,_494) :- 
        [_417] = _496, [_421] = _498, [_425] = _500, [_437] = _502, [_441] = _504, [_445] = _506, 
        generatecut([_417],[_421],[_425],[num],[num],[_437],[_441],[_445],[num]),
        normalize_result([_496,_498,_500,_502,_504,_506],
                         [_484,_486,_488,_490,_492,_494]).
generatecutaux__1(_613,_615,_617,_619,_621,_623,_625,_627,_629) :- 
        [_463] = _631, [_553] = _633, [_529] = _635, [_533] = _637, [_544] = _639, [list,_194] = _641, [list,list] = _643, [_544] = _645, [_572] = _647, 
        'my is'([_562],[[_529],[_533]]),
        'my >='([_544],[_553]),
        nobeter([_562],[list,_194],[_572]),
        normalize_result([_631,_633,_635,_637,_639,_641,_643,_645,_647],
                         [_613,_615,_617,_619,_621,_623,_625,_627,_629]).
generatecutaux__1(_706,_708,_710,_712,_714,_716,_718,_720,_722) :- 
        [_628] = _724, [_632] = _726, [_636] = _728, [_640] = _730, [_644] = _732, [list,_194] = _734, [list,_654] = _736, [_658] = _738, [_613] = _740, 
        'my is'([_662],[[_613],[num]]),
        generatecut([_628],[_632],[_636],[_640],[_644],[list,_194],[_654],[_658],[_662]),
        normalize_result([_724,_726,_728,_730,_732,_734,_736,_738,_740],
                         [_706,_708,_710,_712,_714,_716,_718,_720,_722]).
generatecutaux__1(_613,_615,_617,_619,_621,_623,_625,_627,_629) :- 
        [_463] = _631, [_553] = _633, [_529] = _635, [_533] = _637, [_544] = _639, [list,_194] = _641, [list,list] = _643, [_544] = _645, [_572] = _647, 
        'my is'([_562],[[_529],[_533]]),
        'my >='([_544],[_553]),
        nobeter([_562],[list,_194],[_572]),
        normalize_result([_631,_633,_635,_637,_639,_641,_643,_645,_647],
                         [_613,_615,_617,_619,_621,_623,_625,_627,_629]).
generatecutaux__1(_706,_708,_710,_712,_714,_716,_718,_720,_722) :- 
        [_628] = _724, [_632] = _726, [_636] = _728, [_640] = _730, [_644] = _732, [list,_194] = _734, [list,_654] = _736, [_658] = _738, [_613] = _740, 
        'my is'([_662],[[_613],[num]]),
        generatecut([_628],[_632],[_636],[_640],[_644],[list,_194],[_654],[_658],[_662]),
        normalize_result([_724,_726,_728,_730,_732,_734,_736,_738,_740],
                         [_706,_708,_710,_712,_714,_716,_718,_720,_722]).
generatecut__1(_917,_919,_921,_923,_925,_927,_929,_931,_933) :- 
        [_830] = _935, [_834] = _937, [_838] = _939, [_773] = _941, [_707] = _943, [list,_194] = _945, [_856] = _947, [_860] = _949, [_864] = _951, 
        'my <'([_864],[num]),
        'my is'([_842],[[_773],[_777]]),
        'my is'([_846],[[_842],[_830]]),
        'my =<'([_842],[_838]),
        generatecutaux([_830],[_834],[_838],[_842],[_846],[list,_194],[_856],[_860],[_864]),
        normalize_result([_935,_937,_939,_941,_943,_945,_947,_949,_951],
                         [_917,_919,_921,_923,_925,_927,_929,_931,_933]).
generatecut__1(_589,_591,_593,_595,_597,_599,_601,_603,_605) :- 
        [_516] = _607, [_520] = _609, [_524] = _611, [_528] = _613, [_532] = _615, [list,_536] = _617, [_540] = _619, [_544] = _621, [_495] = _623, 
        generatecut([_516],[_520],[_524],[_528],[_532],[_536],[_540],[_544],[num]),
        normalize_result([_607,_609,_611,_613,_615,_617,_619,_621,_623],
                         [_589,_591,_593,_595,_597,_599,_601,_603,_605]).
generatecut__1(_917,_919,_921,_923,_925,_927,_929,_931,_933) :- 
        [_830] = _935, [_834] = _937, [_838] = _939, [_773] = _941, [_707] = _943, [list,_194] = _945, [_856] = _947, [_860] = _949, [_864] = _951, 
        'my <'([_864],[num]),
        'my is'([_842],[[_773],[_777]]),
        'my is'([_846],[[_842],[_830]]),
        'my =<'([_842],[_838]),
        generatecutaux([_830],[_834],[_838],[_842],[_846],[list,_194],[_856],[_860],[_864]),
        normalize_result([_935,_937,_939,_941,_943,_945,_947,_949,_951],
                         [_917,_919,_921,_923,_925,_927,_929,_931,_933]).
generatecut__1(_589,_591,_593,_595,_597,_599,_601,_603,_605) :- 
        [_516] = _607, [_520] = _609, [_524] = _611, [_528] = _613, [_532] = _615, [list,_536] = _617, [_540] = _619, [_544] = _621, [_495] = _623, 
        generatecut([_516],[_520],[_524],[_528],[_532],[_536],[_540],[_544],[num]),
        normalize_result([_607,_609,_611,_613,_615,_617,_619,_621,_623],
                         [_589,_591,_593,_595,_597,_599,_601,_603,_605]).
nobeter__1(_238,_240,_242) :- 
        [_212] = _244, [list,_216] = _246, [num] = _248, 
        nobeter([_212],[_216]),
        normalize_result([_244,_246,_248],
                         [_238,_240,_242]).
nobeter__1(_311,_313,_315) :- 
        [_280] = _317, [list,_106] = _319, [_262] = _321, 
        'my =='([_262],[num]),
        nobeter([_280],[list,_106]),
        normalize_result([_317,_319,_321],
                         [_311,_313,_315]).
nobeter__1(_151,_153,_155) :- 
        [_127] = _157, [list] = _159, [_135] = _161, 
        normalize_result([_157,_159,_161],
                          [_151,_153,_155]).
nobeter__1(_238,_240,_242) :- 
        [_212] = _244, [list,_216] = _246, [num] = _248, 
        nobeter([_212],[_216]),
        normalize_result([_244,_246,_248],
                         [_238,_240,_242]).
nobeter__1(_311,_313,_315) :- 
        [_280] = _317, [list,_106] = _319, [_262] = _321, 
        'my =='([_262],[num]),
        nobeter([_280],[list,_106]),
        normalize_result([_317,_319,_321],
                         [_311,_313,_315]).
nobeter__1(_151,_153,_155) :- 
        [_127] = _157, [list] = _159, [_135] = _161, 
        normalize_result([_157,_159,_161],
                          [_151,_153,_155]).
nobeter__1(_122,_124) :- 
        [_105] = _126, [list] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
nobeter__1(_278,_280) :- 
        [_251] = _282, [list,_255] = _284, 
        'my <'([_251],[_242]),
        nobeter([_251],[_255]),
        normalize_result([_282,_284],
                         [_278,_280]).
nobeter__1(_122,_124) :- 
        [_105] = _126, [list] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
nobeter__1(_278,_280) :- 
        [_251] = _282, [list,_255] = _284, 
        'my <'([_251],[_242]),
        nobeter([_251],[_255]),
        normalize_result([_282,_284],
                         [_278,_280]).
combination__1(_327,_329,_331) :- 
        [_288] = _333, [_292] = _335, [_300] = _337, 
        emptyconfig([_296]),
        combine([_288],[_292],[_296],[_300]),
        normalize_result([_333,_335,_337],
                         [_327,_329,_331]).
combination__1(_327,_329,_331) :- 
        [_288] = _333, [_292] = _335, [_300] = _337, 
        emptyconfig([_296]),
        combine([_288],[_292],[_296],[_300]),
        normalize_result([_333,_335,_337],
                         [_327,_329,_331]).
combine__1(_1014,_1016,_1018,_1020) :- 
        [list,list,list] = _1022, [_926] = _1024, [_887] = _1026, [list,list,list,list] = _1028, 
        'my ='([_891],[list,_226]),
        'my ='([_922],[list,_278]),
        split([_861],[_880]),
        my_member([list,list,list],[_880]),
        combcut([_887],[_891],[_926],[_918],[_958],[_907]),
        combcut([_918],[_922],[_926],[_930],[_962],[_938]),
        'my is'([_949],[[[_958],[_962]],[_968]]),
        normalize_result([_1022,_1024,_1026,_1028],
                         [_1014,_1016,_1018,_1020]).
combine__1(_1014,_1016,_1018,_1020) :- 
        [list,list,list] = _1022, [_926] = _1024, [_887] = _1026, [list,list,list,list] = _1028, 
        'my ='([_891],[list,_226]),
        'my ='([_922],[list,_278]),
        split([_861],[_880]),
        my_member([list,list,list],[_880]),
        combcut([_887],[_891],[_926],[_918],[_958],[_907]),
        combcut([_918],[_922],[_926],[_930],[_962],[_938]),
        'my is'([_949],[[[_958],[_962]],[_968]]),
        normalize_result([_1022,_1024,_1026,_1028],
                         [_1014,_1016,_1018,_1020]).
combcut__1(_893,_895,_897,_899,_901,_903) :- 
        [_790] = _905, [list,_810] = _907, [_814] = _909, [_818] = _911, [_833] = _913, [_806] = _915, 
        find([_774],[_814],[_763]),
        my_member([list,list,list],[_763]),
        construct([_806],[_774],[_786]),
        addliste([_786],[_790],[_802]),
        combcutaux([_802],[_806],[_810],[_814],[_818],[_846]),
        'my is'([_833],[[_842],[_846]]),
        normalize_result([_905,_907,_909,_911,_913,_915],
                         [_893,_895,_897,_899,_901,_903]).
combcut__1(_893,_895,_897,_899,_901,_903) :- 
        [_790] = _905, [list,_810] = _907, [_814] = _909, [_818] = _911, [_833] = _913, [_806] = _915, 
        find([_774],[_814],[_763]),
        my_member([list,list,list],[_763]),
        construct([_806],[_774],[_786]),
        addliste([_786],[_790],[_802]),
        combcutaux([_802],[_806],[_810],[_814],[_818],[_846]),
        'my is'([_833],[[_842],[_846]]),
        normalize_result([_905,_907,_909,_911,_913,_915],
                         [_893,_895,_897,_899,_901,_903]).
combcutaux__1(_230,_232,_234,_236,_238,_240) :- 
        [_201] = _242, [_189] = _244, [list] = _246, [_197] = _248, [_201] = _250, [num] = _252, 
        normalize_result([_242,_244,_246,_248,_250,_252],
                          [_230,_232,_234,_236,_238,_240]).
combcutaux__1(_893,_895,_897,_899,_901,_903) :- 
        [_790] = _905, [_806] = _907, [list,_810] = _909, [_814] = _911, [_818] = _913, [_833] = _915, 
        find([_774],[_814],[_763]),
        my_member([list,list,list],[_763]),
        construct([_806],[_774],[_786]),
        addliste([_786],[_790],[_802]),
        combcutaux([_802],[_806],[_810],[_814],[_818],[_846]),
        'my is'([_833],[[_842],[_846]]),
        normalize_result([_905,_907,_909,_911,_913,_915],
                         [_893,_895,_897,_899,_901,_903]).
combcutaux__1(_230,_232,_234,_236,_238,_240) :- 
        [_201] = _242, [_189] = _244, [list] = _246, [_197] = _248, [_201] = _250, [num] = _252, 
        normalize_result([_242,_244,_246,_248,_250,_252],
                          [_230,_232,_234,_236,_238,_240]).
combcutaux__1(_893,_895,_897,_899,_901,_903) :- 
        [_790] = _905, [_806] = _907, [list,_810] = _909, [_814] = _911, [_818] = _913, [_833] = _915, 
        find([_774],[_814],[_763]),
        my_member([list,list,list],[_763]),
        construct([_806],[_774],[_786]),
        addliste([_786],[_790],[_802]),
        combcutaux([_802],[_806],[_810],[_814],[_818],[_846]),
        'my is'([_833],[[_842],[_846]]),
        normalize_result([_905,_907,_909,_911,_913,_915],
                         [_893,_895,_897,_899,_901,_903]).
emptyconfig__1(_219) :- 
        [_198] = _221, 
        empty([_194]),
        genempty([_194],[_198]),
        normalize_result([_221],
                         [_219]).
emptyconfig__1(_219) :- 
        [_198] = _221, 
        empty([_194]),
        genempty([_194],[_198]),
        normalize_result([_221],
                         [_219]).
genempty__1(_116,_118) :- 
        [list] = _120, [list] = _122, 
        normalize_result([_120,_122],
                          [_116,_118]).
genempty__1(_225,_227) :- 
        [list,_201] = _229, [list,_205] = _231, 
        genempty([_201],[_205]),
        normalize_result([_229,_231],
                         [_225,_227]).
genempty__1(_116,_118) :- 
        [list] = _120, [list] = _122, 
        normalize_result([_120,_122],
                          [_116,_118]).
genempty__1(_225,_227) :- 
        [list,_201] = _229, [list,_205] = _231, 
        genempty([_201],[_205]),
        normalize_result([_229,_231],
                         [_225,_227]).
split__1(_139,_141,_143) :- 
        [list] = _145, [list] = _147, [list] = _149, 
        normalize_result([_145,_147,_149],
                          [_139,_141,_143]).
split__1(_279,_281,_283) :- 
        [list,_248] = _285, [list,_252] = _287, [_256] = _289, 
        split([_248],[_252],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
split__1(_279,_281,_283) :- 
        [list,_248] = _285, [_252] = _287, [list,_256] = _289, 
        split([_248],[_252],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
split__1(_139,_141,_143) :- 
        [list] = _145, [list] = _147, [list] = _149, 
        normalize_result([_145,_147,_149],
                          [_139,_141,_143]).
split__1(_279,_281,_283) :- 
        [list,_248] = _285, [list,_252] = _287, [_256] = _289, 
        split([_248],[_252],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
split__1(_279,_281,_283) :- 
        [list,_248] = _285, [_252] = _287, [list,_256] = _289, 
        split([_248],[_252],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
allsplit__1(_122,_124) :- 
        [_105] = _126, [list] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
allsplit__1(_283,_285) :- 
        [_256] = _287, [list,_260] = _289, 
        split([_256],[_249]),
        allsplit([_256],[_260]),
        normalize_result([_287,_289],
                         [_283,_285]).
allsplit__1(_122,_124) :- 
        [_105] = _126, [list] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
allsplit__1(_283,_285) :- 
        [_256] = _287, [list,_260] = _289, 
        split([_256],[_249]),
        allsplit([_256],[_260]),
        normalize_result([_287,_289],
                         [_283,_285]).
split__1(_334,_336) :- 
        [list,_289] = _338, [list,list,list] = _340, 
        split([_289],[_293],[_311]),
        intersection([list,_293],[_311]),
        normalize_result([_338,_340],
                         [_334,_336]).
split__1(_334,_336) :- 
        [list,_289] = _338, [list,list,list] = _340, 
        split([_289],[_293],[_311]),
        intersection([list,_293],[_311]),
        normalize_result([_338,_340],
                         [_334,_336]).
intersection__1(_273,_275) :- 
        [_239] = _277, [_250] = _279, 
        my_member([_246],[_239]),
        my_member([_246],[_250]),
        normalize_result([_277,_279],
                         [_273,_275]).
intersection__1(_273,_275) :- 
        [_239] = _277, [_250] = _279, 
        my_member([_246],[_239]),
        my_member([_246],[_250]),
        normalize_result([_277,_279],
                         [_273,_275]).
construct__1(_145,_147,_149) :- 
        [list] = _151, [_125] = _153, [list] = _155, 
        normalize_result([_151,_153,_155],
                          [_145,_147,_149]).
construct__1(_279,_281,_283) :- 
        [list,_248] = _285, [_252] = _287, [list,_256] = _289, 
        construct([_248],[_252],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
construct__1(_145,_147,_149) :- 
        [list] = _151, [_125] = _153, [list] = _155, 
        normalize_result([_151,_153,_155],
                          [_145,_147,_149]).
construct__1(_279,_281,_283) :- 
        [list,_248] = _285, [_252] = _287, [list,_256] = _289, 
        construct([_248],[_252],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
addelement__1(_295,_297,_299) :- 
        [list,list,list] = _301, [list,_116] = _303, [list,_116] = _305, 
        'my is'([_256],[[_265],[num]]),
        normalize_result([_301,_303,_305],
                         [_295,_297,_299]).
addelement__1(_368,_370,_372) :- 
        [list,list,list] = _374, [list,_116] = _376, [list,_116] = _378, 
        'my ==='([list,list,list],[list]),
        'my is'([_326],[[_335],[num]]),
        normalize_result([_374,_376,_378],
                         [_368,_370,_372]).
addelement__1(_433,_435,_437) :- 
        [_396] = _439, [list,_400] = _441, [list,_404] = _443, 
        'my ==='([_396],[list,list,list]),
        'my ==='([_396],[list,list,list]),
        addelement([_396],[_400],[_404]),
        normalize_result([_439,_441,_443],
                         [_433,_435,_437]).
addelement__1(_295,_297,_299) :- 
        [list,list,list] = _301, [list,_116] = _303, [list,_116] = _305, 
        'my is'([_256],[[_265],[num]]),
        normalize_result([_301,_303,_305],
                         [_295,_297,_299]).
addelement__1(_368,_370,_372) :- 
        [list,list,list] = _374, [list,_116] = _376, [list,_116] = _378, 
        'my ==='([list,list,list],[list]),
        'my is'([_326],[[_335],[num]]),
        normalize_result([_374,_376,_378],
                         [_368,_370,_372]).
addelement__1(_433,_435,_437) :- 
        [_396] = _439, [list,_400] = _441, [list,_404] = _443, 
        'my ==='([_396],[list,list,list]),
        'my ==='([_396],[list,list,list]),
        addelement([_396],[_400],[_404]),
        normalize_result([_439,_441,_443],
                         [_433,_435,_437]).
addliste__1(_149,_151,_153) :- 
        [list] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
addliste__1(_364,_366,_368) :- 
        [list,_330] = _370, [_318] = _372, [_338] = _374, 
        addelement([_314],[_318],[_334]),
        addliste([_330],[_334],[_338]),
        normalize_result([_370,_372,_374],
                         [_364,_366,_368]).
addliste__1(_149,_151,_153) :- 
        [list] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
addliste__1(_364,_366,_368) :- 
        [list,_330] = _370, [_318] = _372, [_338] = _374, 
        addelement([_314],[_318],[_334]),
        addliste([_330],[_334],[_338]),
        normalize_result([_370,_372,_374],
                         [_364,_366,_368]).
find__1(_167,_169,_171) :- 
        [_141] = _173, [list,_106] = _175, [_151] = _177, 
        normalize_result([_173,_175,_177],
                          [_167,_169,_171]).
find__1(_332,_334,_336) :- 
        [_298] = _338, [list,_302] = _340, [_306] = _342, 
        'my ==='([_298],[_289]),
        find([_298],[_302],[_306]),
        normalize_result([_338,_340,_342],
                         [_332,_334,_336]).
find__1(_167,_169,_171) :- 
        [_141] = _173, [list,_106] = _175, [_151] = _177, 
        normalize_result([_173,_175,_177],
                          [_167,_169,_171]).
find__1(_332,_334,_336) :- 
        [_298] = _338, [list,_302] = _340, [_306] = _342, 
        'my ==='([_298],[_289]),
        find([_298],[_302],[_306]),
        normalize_result([_338,_340,_342],
                         [_332,_334,_336]).
append__1(_149,_151,_153) :- 
        [list] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
append__1(_279,_281,_283) :- 
        [list,_248] = _285, [_252] = _287, [list,_256] = _289, 
        my_append([_248],[_252],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
append__1(_149,_151,_153) :- 
        [list] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
append__1(_279,_281,_283) :- 
        [list,_248] = _285, [_252] = _287, [list,_256] = _289, 
        my_append([_248],[_252],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
member__1(_138,_140) :- 
        [_119] = _142, [list,_106] = _144, 
        normalize_result([_142,_144],
                          [_138,_140]).
member__1(_215,_217) :- 
        [_191] = _219, [list,_195] = _221, 
        my_member([_191],[_195]),
        normalize_result([_219,_221],
                         [_215,_217]).
member__1(_138,_140) :- 
        [_119] = _142, [list,_106] = _144, 
        normalize_result([_142,_144],
                          [_138,_140]).
member__1(_215,_217) :- 
        [_191] = _219, [list,_195] = _221, 
        my_member([_191],[_195]),
        normalize_result([_219,_221],
                         [_215,_217]).
ground_list__1(_93) :- 
        [list] = _95, 
        normalize_result([_95],
                          [_93]).
ground_list__1(_204) :- 
        [list,_184] = _206, 
        my_ground([_178]),
        ground_list([_184]),
        normalize_result([_206],
                         [_204]).
horizontal__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).
horizontal__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).
horizontal__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).
vertical__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).
vertical__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).
wastev__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).
wastev__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).
wasteh__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).
wasteh__1(_93) :- 
        [num] = _95, 
        normalize_result([_95],
                          [_93]).
cutv__1(_151) :- 
        [_134] = _153, 
        ground_list([_134]),
        normalize_result([_153],
                         [_151]).
cuth__1(_151) :- 
        [_134] = _153, 
        ground_list([_134]),
        normalize_result([_153],
                         [_151]).
empty__1(_151) :- 
        [_134] = _153, 
        ground_list([_134]),
        normalize_result([_153],
                         [_151]).
quantity__1(_151) :- 
        [_134] = _153, 
        ground_list([_134]),
        normalize_result([_153],
                         [_151]).
ground__1(_99) :- 
        [_89] = _101, 
        normalize_result([_101],
                          [_99]).

%---------------- Definitions of tabled preds --------------

my_ground(_63) :- 
        ground__1(_65),
        unify_sets([_63], [_65]).
quantity(_63) :- 
        quantity__1(_65),
        unify_sets([_63], [_65]).
empty(_63) :- 
        empty__1(_65),
        unify_sets([_63], [_65]).
cuth(_63) :- 
        cuth__1(_65),
        unify_sets([_63], [_65]).
cutv(_63) :- 
        cutv__1(_65),
        unify_sets([_63], [_65]).
wasteh(_63) :- 
        wasteh__1(_65),
        unify_sets([_63], [_65]).
wastev(_63) :- 
        wastev__1(_65),
        unify_sets([_63], [_65]).
vertical(_63) :- 
        vertical__1(_65),
        unify_sets([_63], [_65]).
ground_list(_63) :- 
        ground_list__1(_65),
        unify_sets([_63], [_65]).
horizontal(_63) :- 
        horizontal__1(_65),
        unify_sets([_63], [_65]).
my_member(_63,_65) :- 
        member__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
my_append(_63,_65,_67) :- 
        append__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
find(_63,_65,_67) :- 
        find__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
addliste(_63,_65,_67) :- 
        addliste__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
addelement(_63,_65,_67) :- 
        addelement__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
construct(_63,_65,_67) :- 
        construct__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
intersection(_63,_65) :- 
        intersection__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
split(_63,_65) :- 
        split__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
allsplit(_63,_65) :- 
        allsplit__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
split(_63,_65,_67) :- 
        split__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
genempty(_63,_65) :- 
        genempty__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
emptyconfig(_63) :- 
        emptyconfig__1(_65),
        unify_sets([_63], [_65]).
combcutaux(_63,_65,_67,_69,_71,_73) :- 
        combcutaux__1(_75,_77,_79,_81,_83,_85),
        unify_sets([_63,_65,_67,_69,_71,_73], [_75,_77,_79,_81,_83,_85]).
combcut(_63,_65,_67,_69,_71,_73) :- 
        combcut__1(_75,_77,_79,_81,_83,_85),
        unify_sets([_63,_65,_67,_69,_71,_73], [_75,_77,_79,_81,_83,_85]).
combine(_63,_65,_67,_69) :- 
        combine__1(_71,_73,_75,_77),
        unify_sets([_63,_65,_67,_69], [_71,_73,_75,_77]).
combination(_63,_65,_67) :- 
        combination__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
nobeter(_63,_65) :- 
        nobeter__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
nobeter(_63,_65,_67) :- 
        nobeter__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
generatecut(_63,_65,_67,_69,_71,_73) :- 
        generatecut__1(_75,_77,_79,_81,_83,_85),
        unify_sets([_63,_65,_67,_69,_71,_73], [_75,_77,_79,_81,_83,_85]).
generatecutaux(_63,_65,_67,_69,_71,_73,_75,_77,_79) :- 
        generatecutaux__1(_81,_83,_85,_87,_89,_91,_93,_95,_97),
        unify_sets([_63,_65,_67,_69,_71,_73,_75,_77,_79], [_81,_83,_85,_87,_89,_91,_93,_95,_97]).
generatecut(_63,_65,_67,_69,_71,_73,_75,_77,_79) :- 
        generatecut__1(_81,_83,_85,_87,_89,_91,_93,_95,_97),
        unify_sets([_63,_65,_67,_69,_71,_73,_75,_77,_79], [_81,_83,_85,_87,_89,_91,_93,_95,_97]).
cut(_63,_65,_67,_69,_71) :- 
        cut__1(_73,_75,_77,_79,_81),
        unify_sets([_63,_65,_67,_69,_71], [_73,_75,_77,_79,_81]).
gencut(_63,_65,_67,_69) :- 
        gencut__1(_71,_73,_75,_77),
        unify_sets([_63,_65,_67,_69], [_71,_73,_75,_77]).
horizontalcut(_63) :- 
        horizontalcut__1(_65),
        unify_sets([_63], [_65]).
verticalcut(_63) :- 
        verticalcut__1(_65),
        unify_sets([_63], [_65]).
configuration(_63) :- 
        configuration__1(_65),
        unify_sets([_63], [_65]).
pgenconfig(_63) :- 
        pgenconfig__1(_65),
        unify_sets([_63], [_65]).

%---------------- Tp ---------------------------------------

tp :- ground__1(_64), fail.
tp :- quantity__1(_64), fail.
tp :- empty__1(_64), fail.
tp :- cuth__1(_64), fail.
tp :- cutv__1(_64), fail.
tp :- wasteh__1(_64), fail.
tp :- wastev__1(_64), fail.
tp :- vertical__1(_64), fail.
tp :- ground_list__1(_64), fail.
tp :- horizontal__1(_64), fail.
tp :- member__1(_64,_66), fail.
tp :- append__1(_64,_66,_68), fail.
tp :- find__1(_64,_66,_68), fail.
tp :- addliste__1(_64,_66,_68), fail.
tp :- addelement__1(_64,_66,_68), fail.
tp :- construct__1(_64,_66,_68), fail.
tp :- intersection__1(_64,_66), fail.
tp :- split__1(_64,_66), fail.
tp :- allsplit__1(_64,_66), fail.
tp :- split__1(_64,_66,_68), fail.
tp :- genempty__1(_64,_66), fail.
tp :- emptyconfig__1(_64), fail.
tp :- combcutaux__1(_64,_66,_68,_70,_72,_74), fail.
tp :- combcut__1(_64,_66,_68,_70,_72,_74), fail.
tp :- combine__1(_64,_66,_68,_70), fail.
tp :- combination__1(_64,_66,_68), fail.
tp :- nobeter__1(_64,_66), fail.
tp :- nobeter__1(_64,_66,_68), fail.
tp :- generatecut__1(_64,_66,_68,_70,_72,_74), fail.
tp :- generatecutaux__1(_64,_66,_68,_70,_72,_74,_76,_78,_80), fail.
tp :- generatecut__1(_64,_66,_68,_70,_72,_74,_76,_78,_80), fail.
tp :- cut__1(_64,_66,_68,_70,_72), fail.
tp :- gencut__1(_64,_66,_68,_70), fail.
tp :- horizontalcut__1(_64), fail.
tp :- verticalcut__1(_64), fail.
tp :- configuration__1(_64), fail.
tp :- pgenconfig__1(_64), fail.
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

show_facts :- ground__1(_63),
              numbervars([_63]),
              assert(results(ground(_63))), fail.
show_facts :- quantity__1(_63),
              numbervars([_63]),
              assert(results(quantity(_63))), fail.
show_facts :- empty__1(_63),
              numbervars([_63]),
              assert(results(empty(_63))), fail.
show_facts :- cuth__1(_63),
              numbervars([_63]),
              assert(results(cuth(_63))), fail.
show_facts :- cutv__1(_63),
              numbervars([_63]),
              assert(results(cutv(_63))), fail.
show_facts :- wasteh__1(_63),
              numbervars([_63]),
              assert(results(wasteh(_63))), fail.
show_facts :- wastev__1(_63),
              numbervars([_63]),
              assert(results(wastev(_63))), fail.
show_facts :- vertical__1(_63),
              numbervars([_63]),
              assert(results(vertical(_63))), fail.
show_facts :- ground_list__1(_63),
              numbervars([_63]),
              assert(results(ground_list(_63))), fail.
show_facts :- horizontal__1(_63),
              numbervars([_63]),
              assert(results(horizontal(_63))), fail.
show_facts :- member__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(member(_63,_65))), fail.
show_facts :- append__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(append(_63,_65,_67))), fail.
show_facts :- find__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(find(_63,_65,_67))), fail.
show_facts :- addliste__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(addliste(_63,_65,_67))), fail.
show_facts :- addelement__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(addelement(_63,_65,_67))), fail.
show_facts :- construct__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(construct(_63,_65,_67))), fail.
show_facts :- intersection__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(intersection(_63,_65))), fail.
show_facts :- split__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(split(_63,_65))), fail.
show_facts :- allsplit__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(allsplit(_63,_65))), fail.
show_facts :- split__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(split(_63,_65,_67))), fail.
show_facts :- genempty__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(genempty(_63,_65))), fail.
show_facts :- emptyconfig__1(_63),
              numbervars([_63]),
              assert(results(emptyconfig(_63))), fail.
show_facts :- combcutaux__1(_63,_65,_67,_69,_71,_73),
              numbervars([_63,_65,_67,_69,_71,_73]),
              assert(results(combcutaux(_63,_65,_67,_69,_71,_73))), fail.
show_facts :- combcut__1(_63,_65,_67,_69,_71,_73),
              numbervars([_63,_65,_67,_69,_71,_73]),
              assert(results(combcut(_63,_65,_67,_69,_71,_73))), fail.
show_facts :- combine__1(_63,_65,_67,_69),
              numbervars([_63,_65,_67,_69]),
              assert(results(combine(_63,_65,_67,_69))), fail.
show_facts :- combination__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(combination(_63,_65,_67))), fail.
show_facts :- nobeter__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(nobeter(_63,_65))), fail.
show_facts :- nobeter__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(nobeter(_63,_65,_67))), fail.
show_facts :- generatecut__1(_63,_65,_67,_69,_71,_73),
              numbervars([_63,_65,_67,_69,_71,_73]),
              assert(results(generatecut(_63,_65,_67,_69,_71,_73))), fail.
show_facts :- generatecutaux__1(_63,_65,_67,_69,_71,_73,_75,_77,_79),
              numbervars([_63,_65,_67,_69,_71,_73,_75,_77,_79]),
              assert(results(generatecutaux(_63,_65,_67,_69,_71,_73,_75,_77,_79))), fail.
show_facts :- generatecut__1(_63,_65,_67,_69,_71,_73,_75,_77,_79),
              numbervars([_63,_65,_67,_69,_71,_73,_75,_77,_79]),
              assert(results(generatecut(_63,_65,_67,_69,_71,_73,_75,_77,_79))), fail.
show_facts :- cut__1(_63,_65,_67,_69,_71),
              numbervars([_63,_65,_67,_69,_71]),
              assert(results(cut(_63,_65,_67,_69,_71))), fail.
show_facts :- gencut__1(_63,_65,_67,_69),
              numbervars([_63,_65,_67,_69]),
              assert(results(gencut(_63,_65,_67,_69))), fail.
show_facts :- horizontalcut__1(_63),
              numbervars([_63]),
              assert(results(horizontalcut(_63))), fail.
show_facts :- verticalcut__1(_63),
              numbervars([_63]),
              assert(results(verticalcut(_63))), fail.
show_facts :- configuration__1(_63),
              numbervars([_63]),
              assert(results(configuration(_63))), fail.
show_facts :- pgenconfig__1(_63),
              numbervars([_63]),
              assert(results(pgenconfig(_63))), fail.
show_facts.
%----------------------------------------------------------
%:- import set_unify/2 from set_unify.

set_unify(A,B) :-
	flatten(A,AF), flatten(B,BF),
        (ground1(AF), ground1(BF) -> AF==BF
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

normalize_result(Args,SortedArgsC) :- !,
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


ground1(X):-var(X),!,fail.
ground1(X):-atomic(X),!,true.
ground1([X|Xs]):-!,ground1(X),ground1(Xs).
ground1(X):-functor(X,_,N),ground1(X,N).

ground1(_,N):-N=:=0,!,true.
ground1(X,N):-N>0,arg(N,X,A),ground1(A),N1 is N-1,ground1(X,N1).

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


result([ground([A]),quantity([list]),empty([list]),cuth([list]),cutv([list]),wasteh([num]),wastev([num]),
	vertical([num]),ground_list([list]),horizontal([num]),member([A],[B,list]),append([list],[A],[A]),
	append([list],[A],[A,list]),find([A],[B,list],[C]),addliste([list],[A],[A]),addliste([list],[A,list],[A,list]),
	addliste([list],[list],[list]),addelement([list],[A,list],[A,list]),construct([list],[A],[list]),
	intersection([A,list],[B,list]),split([list],[list]),allsplit([A],[list]),allsplit([list],[list]),
	split([list],[list],[list]),genempty([list],[list]),emptyconfig([list]),combcutaux([A],[B],[list],[C],[A],[num]),
	combcutaux([A],[list],[list],[B,list],[A],[num]),combcutaux([A],[list],[list],[list],[A],[num]),
	combcutaux([A,list],[list],[list],[B,list],[A,list],[num]),
	combcutaux([A,list],[list],[list],[list],[A,list],[num]),combcutaux([list],[list],[list],[A,list],[list],[num]),
	combcutaux([list],[list],[list],[list],[list],[num]),
	combcut([A],[list],[B,list],[A],[num],[list]),combcut([A],[list],[list],[A],[num],[list]),
	combcut([A,list],[list],[B,list],[A,list],[num],[list]),combcut([A,list],[list],[list],[A,list],[num],[list]),
	combcut([list],[list],[A,list],[list],[num],[list]),combcut([list],[list],[list],[list],[num],[list]),
	combine([list],[A,list],[B],[list]),combine([list],[list],[A],[list]),combine([list],[A,list],[B,list],[list]),
	combine([list],[list],[A,list],[list]),combine([list],[A,list],[list],[list]),
	combine([list],[list],[list],[list]),combination([list],[A,list],[list]),combination([list],[list],[list]),
	nobeter([A],[list]),nobeter([num],[list]),nobeter([A],[list],[num]),nobeter([num],[list],[num]),
	nobeter([A],[list],[B]),generatecutaux([A],[num],[num],[num],[num],[list],[list],[num],[num]),
	generatecutaux([A],[num],[num],[num],[num],[list],[list],[num],[B]),gencut([list],[A],[B],[list]),
	horizontalcut([list])]).
