:- module(cars2, _, [rfuzzy, clpr]).


car(vw_caddy).
car(alfa_romeo_gt).
car(aston_martin_bulldog).
car(lamborghini_urraco).

expensive_car(vw_caddy) value 0.7 .
expensive_car(alfa_romeo_gt) value 0.6 .

rfuzzy_type_for(has_lower_price/2, [car/1, car/1]).

rfuzzy_type_for(expensive_car/1, [car/1]).
rfuzzy_default_value_for(expensive_car/1, 0.9) if expensive_type/1.
rfuzzy_default_value_for(expensive_car/1, 0.5).

expensive_type(lamborghini_urraco).
expensive_type(aston_martin_bulldog).