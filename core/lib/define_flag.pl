:- package(define_flag).
:- use_package(assertions).

:- trust pred define_flag(Flag, FlagValues, Default)
	=> (atm(Flag), flag_values(FlagValues)).

:- multifile define_flag/3.
