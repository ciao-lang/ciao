:- module(http_date, [], [assertions, isomodes, regtypes, dcg, hiord, doccomments]).

%! \title  HTTP dates
%  \author The Ciao Development Team
%
%  \module
%
%  Parser/printer for dates in the HTTP protocol.
%
%  The format of dates in HTTP/1.1 (RFC1123 and RFC850)
%  (see [RFC2616](https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.3))
%  is the following:
%
%  ```
%  HTTP-date    = rfc1123-date | rfc850-date | asctime-date
%  rfc1123-date = wkday "," SP date1 SP time SP "GMT"
%  rfc850-date  = weekday "," SP date2 SP time SP "GMT"
%  asctime-date = wkday SP date3 SP time SP 4DIGIT
%  date1        = 2DIGIT SP month SP 4DIGIT
%                 ; day month year (e.g., 02 Jun 1982)
%  date2        = 2DIGIT "-" month "-" 2DIGIT
%                 ; day-month-year (e.g., 02-Jun-82)
%  date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
%                 ; month day (e.g., Jun  2)
%  time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
%                 ; 00:00:00 - 23:59:59
%  wkday        = "Mon" | "Tue" | "Wed"
%               | "Thu" | "Fri" | "Sat" | "Sun"
%  weekday      = "Monday" | "Tuesday" | "Wednesday"
%               | "Thursday" | "Friday" | "Saturday" | "Sunday"
%  month        = "Jan" | "Feb" | "Mar" | "Apr"
%               | "May" | "Jun" | "Jul" | "Aug"
%               | "Sep" | "Oct" | "Nov" | "Dec"
%  ```

% ---------------------------------------------------------------------------

:- use_module(library(http/http_grammar), 
	[ http_sp/2,
	  'PARSING'/2, 'PRINTING'/2
	]).

% ---------------------------------------------------------------------------
% Term representation of HTTP dates

:- export(http_date/1).
:- doc(http_date(Date), "@var{Date} is a term defined as
   @includedef{http_date/1}.").
:- regtype http_date(Date) # "@var{Date} is a term denoting a date.".

http_date(date(WeekDay,Day,Month,Year,Time)) :-
        weekday(WeekDay),
        int(Day),
        month(Month),
        int(Year),
        hms_time(Time).

:- export(weekday/1).
:- regtype weekday(WeekDay) # "@var{WeekDay} is a term
   denoting a weekday.".

weekday('Monday').
weekday('Tuesday').
weekday('Wednesday').
weekday('Thursday').
weekday('Friday').
weekday('Saturday').
weekday('Sunday').

:- export(month/1).
:- regtype month(Month) # "@var{Month} is a term denoting
   a month.".

month('January').
month('February').
month('March').
month('April').
month('May').
month('June').
month('July').
month('August').
month('September').
month('October').
month('November').
month('December').

:- export(hms_time/1).
:- regtype hms_time(Time) # "@var{Time} is an atom of the form
   @tt{hh:mm:ss}".

hms_time(T) :- atm(T).

% ---------------------------------------------------------------------------

:- export(http_date_str/3).
http_date_str(Date) --> 'PRINTING', !,
	{ Date = date(WeekDay,Day,Month,Year,Time) },
	% (HTTP 1.1 requires writing only in rfc1123 format)
        http_wkday(WeekDay), " ",
        http_day(Day, 0'0), " ",
        http_month(Month), " ",
        http_year(Year), " ",
	http_time(Time), " ",
	"GMT".
http_date_str(Date) --> parse_rfc1123_date(Date), !.
http_date_str(Date) --> parse_rfc850_date(Date), !.
http_date_str(Date) --> parse_asctime_date(Date), !.

parse_rfc1123_date(date(WeekDay,Day,Month,Year,Time)) -->
        http_wkday(WeekDay),
	( "," -> [] ; [] ), % try parse ','
        http_sp,
	http_date1(Day, Month, Year),
        http_sp,
	http_time(Time),
        http_sp,
	"GMT".

parse_rfc850_date(date(WeekDay,Day,Month,Year,Time)) -->
        http_weekday(WeekDay),
	",",
        http_sp,
	http_date2(Day, Month, Year),
        http_sp,
	http_time(Time),
        http_sp,
	"GMT".

parse_asctime_date(date(WeekDay,Day,Month,Year,Time)) -->
        http_wkday(WeekDay),
        http_sp,
	http_date3(Day, Month),
        http_sp,
	http_time(Time),
        http_sp,
	http_year(Year).

http_date1(Day, Month, Year) -->
        http_day(Day, 0'0),
        http_sp,
        http_month(Month),
        http_sp,
        http_year(Year).

http_date2(Day, Month, Year) -->
        http_day(Day, 0'0),
        "-",
        http_month(Month),
        "-",
        http_year2(Year).

http_date3(Day, Month) -->
        http_month(Month),
	http_sp,
        http_day(Day, 0' ).

http_wkday('Monday') --> "Mon", !.
http_wkday('Tuesday') --> "Tue", !.
http_wkday('Wednesday') --> "Wed", !.
http_wkday('Thursday') --> "Thu", !.
http_wkday('Friday') --> "Fri", !.
http_wkday('Saturday') --> "Sat", !.
http_wkday('Sunday') --> "Sun", !.

http_weekday('Monday') --> "Monday", !.
http_weekday('Tuesday') --> "Tuesday", !.
http_weekday('Wednesday') --> "Wednesday", !.
http_weekday('Thursday') --> "Thursday", !.
http_weekday('Friday') --> "Friday", !.
http_weekday('Saturday') --> "Saturday", !.
http_weekday('Sunday') --> "Sunday", !.

% Parse/unparse 2DIGIT day. Left zeroes are represented with LeftZ char.
%   E.g., "02", "12", etc. (LeftZ = 0'0)
%   E.g., " 2", "12", etc. (LeftZ = 0' )
http_day(Day, LeftZ) --> 'PRINTING', !,
	{ number_codes(Day, Ds) },
        { Ds = [D1,D2] -> true ; Ds = [D2], D1 = LeftZ },
	[D1,D2].
http_day(Day, LeftZ) --> [D1,D2], !,
	{ D1 = LeftZ -> number_codes(Day, [D2])
	; number_codes(Day, [D1,D2])
	}.

http_month('January') --> "Jan".
http_month('February') --> "Feb".
http_month('March') --> "Mar".
http_month('April') --> "Apr".
http_month('May') --> "May".
http_month('June') --> "Jun".
http_month('July') --> "Jul".
http_month('August') --> "Aug".
http_month('September') --> "Sep".
http_month('October') --> "Oct".
http_month('November') --> "Nov".
http_month('December') --> "Dec".

% Assumes Year > 999
http_year(Year) --> 'PRINTING', !,
        { number_codes(Year,[Y1,Y2,Y3,Y4]) },
        [Y1,Y2,Y3,Y4].
http_year(Year) -->
        [Y1,Y2,Y3,Y4],
        { number_codes(Year,[Y1,Y2,Y3,Y4]) }.

% 2DIGIT year
% On parsing, converts to 4DIGIT using POSIX conventions:
%   - >=70 -> 19xx
%   - =<69 -> 20xx
% (70->1970, ..., 99->1999, 00->2000, ..., 69->2069)

http_year2(Year) --> 'PRINTING', !,
        { number_codes(Year,[_,_,Y3,Y4]) },
        [Y3,Y4].
http_year2(Year) -->
        [Y3,Y4],
        { number_codes(Year0,[Y3,Y4]) },
	{ Year0 =< 69 -> Year is 2000 + Year0
	; Year is 1900 + Year0
	}.

http_time(Time) --> 'PRINTING', !,
        { atom_codes(Time,Time0),
	  time_field(Time0,[H1,H2,0':|Time1]),
          time_field(Time1,[M1,M2,0':|Time2]),
          time_field(Time2,[S1,S2])
	},
        [H1,H2,0':,M1,M2,0':,S1,S2].
http_time(Time) -->
	% (some clients generate this kind of time format)
	http_2dig_or_1dig(H1,H2), [0':],
	http_2dig_or_1dig(M1,M2), [0':],
	http_2dig_or_1dig(S1,S2),
        { atom_codes(Time,[H1,H2,0':,M1,M2,0':,S1,S2]) }.

http_2dig_or_1dig(H1,H2) -->
        ( [H1,H2], { digit(H2) } -> []
	; [H2], { H1 = 0'0 }
	).

digit(X) :- X >= 0'0, X =< 0'9.

time_field(Pattern,Pattern):- !.
time_field(Pattern,[0'0|Pattern]).

