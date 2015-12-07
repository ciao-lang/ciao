:- module(results, [
        clean_benchmark_data/0,
        get_general_data/9,
        add_section/1,
        get_section/1,
        add_bench/3,
        get_bench/3,
        add_timings/5,
        get_timings/5], []).

:- use_module(library(system)).
:- use_module(library(process)).
:- use_module(library(compiler)).
:- use_module(library(lists)).
:- use_module(library(file_utils)).
:- use_module(library(prolog_sys)).
:- use_module(engine(internals)).

:- data section/1.        % General section name
:- data benchmark_name/3.  % Section, Benchmark name, Goal
:- data benchmark_timings/5. % BenchName, TotalTime, LoopTime, BenchTime, KLIPS

clean_benchmark_data:-
        retractall_fact(section(_)),
        retractall_fact(benchmark_name(_,_,_)),
        retractall_fact(benchmark_timings(_,_,_,_,_)).

get_general_data(Compiler, CompOptions,
                 CompVersion,
                 Host, OS, CPU, Arch, MHZ, Date):-
        % TODO: recover actual C compiler and options, add compiler version too
        Compiler = unknown,
	CompOptions = unknown,
	'$ciao_version'(_, _, _, _, _, CompVersion),
        %
        ( current_host(localhost) ->
            process_call(path(hostname), [], [stdout(line(HostName))]),
            atom_codes(Host, HostName)
	; current_host(Host)
        ),
        get_os(OS),
        get_arch(Arch),
        datime(_, Year, Month, Day, Hour, Min, Sec, _, _),
        Date = Year-Month-Day-Hour-Min-Sec,
        get_mhz(MHZ),
        get_cpu(CPU).

max_freq('/sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_max_freq').
cpuinfo('/proc/cpuinfo').
sun4uvsysconf('/usr/platform/sun4v/sbin/prtdiag').
sun4uvsysconf('/usr/platform/sun4u/sbin/prtdiag').

%% If we have a SpeedStep processor, we should have used the highest frequency
get_mhz(MHZ):-
        max_freq(MaxFreqFile),
        file_exists(MaxFreqFile),
        file_to_string(MaxFreqFile, MaxFreqStr),
        append(OnlyNumber, [_], MaxFreqStr),
        number_codes(MHZ, OnlyNumber),
        !.
%% Otherwise, we should have run at constant speed
get_mhz(MHZ):-
        cpuinfo(CPUInfo),
        file_exists(CPUInfo),
        file_to_string(CPUInfo, CpuInfoStr),
        append(_, "cpu MHz"||PrefixMHz, CpuInfoStr),
        append(_, ": "||PrefixNumber, PrefixMHz),
        append(CpuMHZ, [10|_], PrefixNumber),
        number_codes(MHZ, CpuMHZ),
        !.
% A Solaris machine
get_mhz(MHZ):-
        sun4uvsysconf(ConfCommand),
        file_exists(ConfCommand),
        process_call(ConfCommand, [], [stdout(string(ConfDesc)), status(0)]),
        append(_, " 0 "||MHzFirstCPU, ConfDesc),
        append(MHZStr, " MHz"||_, MHzFirstCPU),
        number_codes(MHZ, MHZStr), !.
%% In any case, we can return the model name
get_mhz(MHZ):- model_name(MHZ).
%% We do not know, otherwise
get_mhz(unknown).


model_name(Model):-
        cpuinfo(CPUInfo),
        file_exists(CPUInfo),
        file_to_string(CPUInfo, CpuInfoStr),
        append(_, "model name"||PrefixMHz, CpuInfoStr),
        append(_, ": "||PrefixNumber, PrefixMHz),
        append(CpuModel, [10|_], PrefixNumber),
        atom_codes(Model, CpuModel), !.


get_cpu(CPU):-
        process_call(path(uname), ['-m'], [stdout(line(CPUName)), status(0)]),
        atom_codes(CPU, CPUName),
        !.
get_cpu(M):- model_name(M).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_section(S):- assertz_fact(section(S)).
get_section(S):- current_fact(section(S)).

add_bench(Sec, Name, Goal):- assertz_fact(benchmark_name(Sec, Name, Goal)).
get_bench(Sec, Name, Goal):- current_fact(benchmark_name(Sec, Name, Goal)).

add_timings(BName, TotTime, LoopTime, BenchTime, KLIPS):-
        assertz_fact(
               benchmark_timings(BName, TotTime, LoopTime, BenchTime, KLIPS)).
get_timings(BName, TotTime, LoopTime, BenchTime, KLIPS):-
        current_fact(
               benchmark_timings(BName, TotTime, LoopTime, BenchTime, KLIPS)).


