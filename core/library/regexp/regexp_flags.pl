:- module(regexp_flags, [], [define_flag]).

define_flag(regexp_format,[shell,posix,struct],posix).
define_flag(regexp_exact,[on,off],on).
