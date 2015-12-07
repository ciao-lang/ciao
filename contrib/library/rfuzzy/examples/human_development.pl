:- module(human_development,_,[rfuzzy, clpr]).

type_Country(japan).
type_Country(spain).
type_Country(china).
type_Country(australia).
type_Country(cuba).
type_Country(brasil).
type_Country(egypt).
type_Country(kenya).
type_Country(senegal).

rfuzzy_type_for(living_standard/1, [type_Country/1]).
rfuzzy_type_for(literacy_rate/1, [type_Country/1]).
rfuzzy_type_for(long_life/1, [type_Country/1]).
rfuzzy_type_for(human_development/1, [type_Country/1]).

rfuzzy_default_value_for(living_standard/1, 0.48).
rfuzzy_default_value_for(literacy_rate/1, 0.99).
rfuzzy_default_value_for(long_life/1, 0.72).

living_standard(japan) value 0.95.
living_standard(spain) value 0.9.
living_standard(china) value 0.65.
living_standard(australia) value 0.95.
living_standard(brasil) value 0.63.
living_standard(kenya) value 0.13.
living_standard(senegal) value 0.25.

literacy_rate(spain) value 0.97.
literacy_rate(china) value 0.9.
literacy_rate(cuba) value 0.96.
literacy_rate(brasil) value 0.88.
literacy_rate(egypt) value 0.55.
literacy_rate(kenya) value 0.73.
literacy_rate(senegal) value 0.39.

long_life(japan) value 0.82.
long_life(spain) value 0.79.
long_life(australia) value 0.8.
long_life(cuba) value 0.77.
long_life(kenya) value 0.48.
long_life(senegal) value 0.59.

human_development(C) :~ prod((literacy_rate(C),long_life(C),living_standard(C))).

