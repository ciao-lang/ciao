:- module(hrtime, [hrtime/1, hrfreq/1, hrmethod/1],
	    [assertions, foreign_interface]).

:- doc(author, "Edison Mera").

:- doc(title, "High resolution time predicates.").

:- doc(module, "This module contains predicates used to facilitate
	the usage of high resolution time functionality in several
	architectures.  Note that not all platforms support methods to
	capture times with high resolution, but even in such cases the
	more accurate measurement method is provided.").

:- foreign_inline("#include \"hrtime.h\"").

:- use_foreign_source(library(hrtime/hrtime)).

:- true pred hrtime(go(T)) :: c_double + (foreign(prolog_hrtime), returns(T)) -->
"
  double prolog_hrtime()
  {
    return hrtime();
  }
".

:- true pred hrmethod(go(M)) :: atm
	+ (foreign(prolog_hrmethod), returns(M), do_not_free(M)) -->
"
  char *prolog_hrmethod()
  {
    return HRTIME_METHOD;
  }
".

:- true pred hrfreq(go(T)) :: c_double + (foreign(prolog_hrfreq), returns(T)) -->
"
  double prolog_hrfreq()
  {
    return (double)hrfreq();
  }
".

:- use_module(library(system)).

:- pred scalingcpu/0 # "Verify if the cpu frequency is variable".
:- export(scalingcpu/0).
scalingcpu :-
	file_exists('/sys/devices/system/cpu/cpu0/cpufreq').

:- pred havecpuinfo/0 # "Verify if the file /proc/cpuinfo exists".
:- export(havecpuinfo/0).
havecpuinfo :-
	file_exists('/proc/cpuinfo').
