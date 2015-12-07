% Rfuzzy approach for a film database.
% Copyright @2010; S.Munoz, S.Egilmez

:-module(filmsDB,_,[rfuzzy, clpr]).

% Activate/Deactivate debug.
% :- activate_rfuzzy_debug.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% POPULATION OF THE DATABASE %%%%%%%%%%%%%%%%%%%%%%%%%

% format: film(TITLE, YEAR, MINUTES, GENRE, LANGUAGE)

film('The Godfather', 1972, 207, drama, english).

film('Casablanca', 1946, 172, romance, english).

film('Cera una volta il West', 1968, 165, western, italian).

film('El laberinto del fauno', 2006, 107, drama, spanish).

film('Il buono, il brutto, il cattivo', 1967, 141, adventure, italian).

film('Finding Nemo', 2003, 112, comedy, english).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%  FUNCTIONS OF ATTRIBUTES%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% modern is an example of a numerical attribute
%rfuzzy_type_for(modern/1, [year/1]).
%rfuzzy_default_value_for(modern/1, 0).
modern :# ([ (1970, 0), (2000, 1), (2010, 1)]) .


% attribute concerning duration of the films
% rfuzzy_default_value_for(mlength/1, 0).
mlength :# ([ (120, 0), (180, 1), (600, 1)]) .

genre('drama').
genre('romance').
genre('western').
genre('adventure').
genre('comedy').

% funny is an example of a discrete attribute
rfuzzy_type_for(funny/1, [genre/1]).

funny('drama') value 0 .
funny('romance') value 0.4 .
funny('western') value 0.2 .
funny('adventure') value 0.2 .
funny('comedy') value 1 .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% SIMILARITY DEFINITIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% similarity over genres
similarG(drama, drama) value 1.
similarG(drama, romance) value 0.6 .
similarG(drama, western) value 0.1 .
similarG(drama, adventure) value 0.1 .
similarG(drama, comedy) value 0 .
similarG(romance, drama) value 0.6 .
similarG(romance, romance) value 1 .
similarG(romance, western) value 0.4 .
similarG(romance, adventure) value 0.3 .
similarG(romance, comedy) value 0.3 .
similarG(western, drama) value 0.1 .
similarG(western, romance) value 0.4 .
similarG(western, western) value 1 .
similarG(western, adventure) value 0.8 .
similarG(western, comedy) value 0.1 .
similarG(adventure, drama) value 0.1 .
similarG(adventure, romance) value 0.3 .
similarG(adventure, western) value 0.8 .
similarG(adventure, adventure) value 1 .
similarG(adventure, comedy) value 0.2 .
similarG(comedy, drama) value 0 .
similarG(comedy, romance) value 0.3 .
similarG(comedy, western) value 0.1 .
similarG(comedy, adventure) value 0.2.
similarG(comedy, comedy) value 1.


% similarity over languages
similarL(english, english) value 1 .
similarL(english, spanish) value 0.2 .
similarL(english, italian) value 0.2 .
similarL(spanish, english) value 0.2 .
similarL(spanish, spanish) value 1 .
similarL(spanish, italian) value 0.7 .
similarL(italian, english) value 0.2 .
similarL(italian, spanish) value 0.7 .
similarL(italian, italian) value 1 .



similarY(X, Y, V) :-  V .=. 1 / ((abs(X-Y)+1) * 5). % similarity over years

similarD(X, Y, V) :-  V .=. 1 / (abs(X-Y)+1). % similarity over durations





%%%cop

similar1(X, Y, V) :-  V .=. (abs(X-Y)+1).

similar2(X, Y, V) :-  V .=. 1 / (abs(X-Y)).

similarD2(X, Y, V) :-  similar1(X, Y, V1), V .=. 1 / V1. % instead of similarD
rfuzzy_non_rfuzzy_fuzzy_rule(similarD2/3).

similar4(X, Y, V) :-  V .=. ((abs(X-Y)+1)*5).

similarY2(X, Y, V) :-  similar4(X, Y, V1), V .=. 1 / V1. % instead of similarY
rfuzzy_non_rfuzzy_fuzzy_rule(similarY2/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% QUERY REALATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

modern_film(Title) :- film(Title, Year, _Minutes, _Genre, _Language), modern(Year, V), V .>. 0.7 .        % query on a numeral(continuious) valued attribute.

classic_film(Title) :- film(Title, Year, _Minutes, _Genre, _Language), modern(Year, V), V .<. 0.05 .        % query on a numeral(continuious) valued attribute.

funny_film(Title) :- film(Title, _Year, _Minutes, Genre, _Language), funny(Genre, V), V .>. 0.7 .         % query on a discrete valued attribute.

saddening_film(Title) :- film(Title, _Year, _Minutes, Genre, _Language), funny(Genre, V),  0.3 .>. V .    % similar to the former query, with almost inverse semantic.

spanish_film(Title) :- film(Title, _Year, _Minutes, _Genre, spanish).                                        % query on a binary attribute.

film_from_1972(Title) :- film(Title, 1972, _Minutes, _Genre, _Language).                                        % query on a binary attribute.

short_film(Title) :-  film(Title, _Year, Minutes, _Genre, _Language), mlength(Minutes, V),  0.2 .>. V .

long_film(Title) :-  film(Title, _Year, Minutes, _Genre, _Language), mlength(Minutes, V),  0.8 .<. V .

%%%similarity queries

%films that share a similar genre of the given spesific genre
similar_genre(Title, Genre, V) :- film(Title, _Year, _Minutes, Genre2, _Language), similarG(Genre, Genre2, V),  V .>. 0.5 .  

%films that share a similar language of the given spesific language
similar_language(Title, Language, V) :- film(Title, _Year, _Minutes, _Genre, Language2), similarL(Language, Language2, V),  V .>. 0.5 .

%films that share a similar modernity level of the given spesific year
similar_year(Title, Year, V) :- film(Title, Year2, _Minutes, _Genre, _Language), similarY(Year, Year2, V),  V .>. 0.1 .


%%% *** main similar query i.e. similarity over films

similar(Title1, Title2, V) :-  film(Title1, Year1, Minutes1, Genre1, Language1),  film(Title2, Year2, Minutes2, Genre2, Language2), similarG(Genre1, Genre2, Vg), similarL(Language1, Language2, Vl), similarY2(Year1, Year2, Vy), similarD2(Minutes1, Minutes2, Vd), V .=. (Vg * Vl * Vy * Vd).


similarTry(Title1, Title2, V) :- film(Title1, Year1, Minutes1, Genre1, Language1),  film(Title2, Year2, Minutes2, Genre2, Language2), similarFuzzy(Title1, Year1, Minutes1, Genre1, Language1,Title2, Year2, Minutes2, Genre2, Language2,V).

similarFuzzy(_Title1, Year1, Minutes1, Genre1, Language1, _Title2, Year2, Minutes2, Genre2, Language2) :~ prod((similarG(Genre1, Genre2), similarL(Language1, Language2),  similarY2(Year1, Year2), similarD2(Minutes1, Minutes2))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



