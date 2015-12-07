%% ---- character codes ----

iso_latex([161], "°", "!\`"). %% begin exclamation mark
iso_latex([162], "¢", "").
iso_latex([163], "£", "").
iso_latex([164], "§", "").
iso_latex([165], "•", "").
iso_latex([166], "¶", "").
iso_latex([167], "ß", "").
iso_latex([168], "®", "").
iso_latex([169], "©", "").
iso_latex([170], "™", "").
iso_latex([171], "´", "").
iso_latex([172], "¨", "").
iso_latex([173], "≠", "").
iso_latex([174], "Æ", "").
iso_latex([175], "Ø", "").
iso_latex([176], "∞", "").
iso_latex([177], "±", "").
iso_latex([178], "≤", "").
iso_latex([179], "≥", "").
iso_latex([180], "¥", "").
iso_latex([181], "µ", "").
iso_latex([182], "∂", "").
iso_latex([183], "∑", "").
iso_latex([184], "∏", "").
iso_latex([185], "π", "").
iso_latex([186], "∫", "").
iso_latex([187], "ª", "").
iso_latex([188], "º", "").
iso_latex([189], "Ω", "").
iso_latex([190], "æ", "").
iso_latex([191], "ø", "").
iso_latex([192], "¿", "\\`{A}"). %% A`
iso_latex([193], "¡", "\\'{A}"). %% A'
iso_latex([194], "¬", "\\^{A}"). %% A^
iso_latex([195], "√", "\\~{A}"). %% A~
iso_latex([196], "ƒ", "\\\"{A}"). %% A:
iso_latex([197], "≈", "").
iso_latex([198], "∆", "").
iso_latex([199], "«", "").
iso_latex([200], "»", "").
iso_latex([201], "…", "\\'{E}"). %% E'
iso_latex([202], " ", "").
iso_latex([203], "À", "").
iso_latex([204], "Ã", "").
iso_latex([205], "Õ", "").
iso_latex([206], "Œ", "\\'{I}"). %% I'
iso_latex([207], "œ", "").
iso_latex([208], "–", "").
iso_latex([209], "—", "\\~{N}"). %% N~
iso_latex([210], "“", "").
iso_latex([211], "”", "\\'{O}"). %% O'
iso_latex([212], "‘", "").
iso_latex([213], "’", "").
iso_latex([214], "÷", "").
iso_latex([215], "◊", "").
iso_latex([216], "ÿ", "").
iso_latex([217], "Ÿ", "").
iso_latex([218], "⁄", "\\'{U}"). %% U'
iso_latex([219], "€", "").
iso_latex([220], "‹", "\\\"{U}"). %% U:
iso_latex([221], "›", "").
iso_latex([222], "ﬁ", "").
iso_latex([223], "ﬂ", "").
iso_latex([224], "‡", "").
iso_latex([225], "·", "\\'{a}"). %% a'
iso_latex([226], "‚", "").
iso_latex([227], "„", "").
iso_latex([228], "‰", "").
iso_latex([229], "Â", "").
iso_latex([230], "Ê", "").
iso_latex([231], "Á", "").
iso_latex([232], "Ë", "").
iso_latex([233], "È", "\\'{e}"). %% e'
iso_latex([234], "Í", "").
iso_latex([235], "Î", "").
iso_latex([236], "Ï", "").
iso_latex([237], "Ì", "\\'{\\i}"). %% i'
iso_latex([238], "Ó", "").
iso_latex([239], "Ô", "").
iso_latex([240], "", "").
iso_latex([241], "Ò", "\\~{n}"). %% n~
iso_latex([242], "Ú", "").
iso_latex([243], "Û", "\\'{o}"). %% o'
iso_latex([244], "Ù", "").
iso_latex([245], "ı", "").
iso_latex([246], "ˆ", "").
iso_latex([247], "˜", "").
iso_latex([248], "¯", "").
iso_latex([249], "˘", "").
iso_latex([250], "˙", "\\'{u}"). %% u'
iso_latex([251], "˚", "").
iso_latex([252], "¸", "\\\"{u}"). %% u:
iso_latex([253], "˝", "").
iso_latex([254], "˛", "").
iso_latex([255], "ˇ", "").

win_latex("†", "\\'{a}"). %% a' 
win_latex("Ç", "\\'{e}"). %% e'
win_latex("°", "\\'{\\i}"). %% i'
win_latex("¢", "\\'{o}"). %% o'
win_latex("£", "\\'{u}"). %% u'
win_latex("Å", "\\\"{u}"). %% u:
win_latex("§", "\\~{n}"). %% n~ 
win_latex("µ", "\\'{A}"). %% A' 
win_latex("ê", "\\'{E}"). %% E'
win_latex("÷", "\\'{I}"). %% I'
win_latex("‡", "\\'{O}"). %% O'
win_latex("È", "\\'{U}"). %% U'
win_latex("•", "\\~{N}"). %% N~ 
win_latex("ö", "\\\"{U}"). %% U:

latex_html("\\'{a}",   "&aacute;"). %% a' 
latex_html("\\'{e}",   "&eacute;"). %% e'
latex_html("\\'{\\i}", "&iacute;"). %% i'
latex_html("\\'{o}",   "&oacute;"). %% o'
latex_html("\\'{u}",   "&uacute;"). %% u'
latex_html("\\~{n}",   "&ntilde;"). %% n~ 

:- use_module(library(format)).

print_win_code_table :-
	win_latex(Char, Latex),
	Latex \== "",
	format("latex_wincode(~s, ~w). %%~n", [Latex, Char]),
	fail.
print_win_code_table :-
	format("~n~n", []).

print_iso_html :-
	iso_latex(Code, Char, Latex),
	latex_html(Latex, Html),
	format("~w  ~s  ~s  ~s~n", [Code, Char, Latex, Html]),
	fail.
print_iso_html.

print_iso_latex :-
	iso_latex(Code, Char, Latex),
	format("~w  ~s  ~s~n", [Code, Char, Latex]),
	fail.
print_iso_latex.


print_iso_code_table :-
	print_iso_code_table(161).

print_iso_code_table(256).
print_iso_code_table(C) :-
	C < 256,
	format("iso_latex([~w], ""~s"", """").~n", [C, [C]]),
	C1 is C+1,
	print_iso_code_table(C1).
