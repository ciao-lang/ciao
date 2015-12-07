:- module(datetime, [], [assertions, basicmodes, nativeprops, fsyntax, hiord, regtypes]).

:- doc(title, "Basic data and time definitions"). 
% TODO: This module is a stub

:- doc(summary, "This module contains predicates to manipulate data
   and time in different formats.").

:- doc(author, "Jose F. Morales").

:- use_module(library(lists), [length/2, append/3]).
:- use_module(library(system), [datime/9]).
:- use_module(library(terms), [atom_concat/2]).

:- export(date_iso8601_to_timestamp/2).
% Obtain timestamp (seconds since 'year zero')
date_iso8601_to_timestamp(Date0, Timestamp) :-
	parse_iso8601_date(Date0, Date),
	atom_concat([AYear, '-', AMonth, '-', ADay, ' ',
		AHour, ':', AMinute, ':', ASeconds], Date),
	!,
	atom_number(AYear,    Year),
	atom_number(AMonth,   Month),
	atom_number(ADay,     Day),
	atom_number(AHour,    Hour),
	atom_number(AMinute,  Minute),
	atom_number(ASeconds, Seconds),
	datime(Timestamp, Year, Month, Day, Hour, Minute, Seconds, _, _).

%:- export(parse_iso8601_date/2).
% Parse the ISO 8601 date string `String` into an atom
% TODO: This is incomplete and incorrect (see http://en.wikipedia.org/wiki/ISO_8601)
parse_iso8601_date(Time0, TimeAtom) :-
	atom_codes(Time0, TimeString0),
	length(Date, 10),
	length(Time, 8),
	append(Date, ~append([_|Time], _), TimeString0),
	append(Date, " " || Time, TimeString),
	!,
	atom_codes(TimeAtom, TimeString).
