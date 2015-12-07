:- module(restaurant,_,[rfuzzy, pkgs_output_debug, clpr]).
% or clpq. We can use clpr or clpq.
% debug with pkgs_output_debug, 

% Define the file where we want debug msgs.
% :- define_pkgs_output_debug_file('~/secured/negation_and_fuzzy_logic/fuzzy_logic/rfuzzy/rfuzzy_ciao/debug_restaurant.pl').

% Activate/Deactivate debug.
% :- activate_rfuzzy_debug.

restaurant(kenzo).
restaurant(burguer_king).
restaurant(pizza_jardin).
restaurant(subway).
restaurant(derroscas).
restaurant(il_tempietto).
restaurant(kono_pizza).
restaurant(paellador).
restaurant(tapasbar).
restaurant(meson_del_jamon).
restaurant(museo_del_jamon).
restaurant(zalacain).

expensive_restaurant(zalacain).

rfuzzy_type_for('fuzzy_rule', traditional/1, [restaurant/1]).
rfuzzy_default_value_for(traditional/1, 1).

traditional(kenzo) value 0.5.
traditional(paellador) value 0.87.

% :- set_prop low_distance_function/1 => restaurant/1.
rfuzzy_default_value_for(low_distance/1, 0).

rfuzzy_type_for('crisp_rule', distance_to_the_city_center/2, [restaurant/1, rfuzzy_truth_value_type/1]).
distance_to_the_city_center(meson_del_jamon, 100).
distance_to_the_city_center(museo_del_jamon, 150).

near_function :# ([ (0, 1), (100, 1), (1000, 0.1) ]) .
rfuzzy_define_fuzzification(near_the_city_center/1, distance_to_the_city_center/2, near_function/2).
near_the_city_center(burguer_king) value 1.

% before:
% :- set_prop cheap/1 => restaurant/1.
rfuzzy_type_for('fuzzy_rule', cheap/1, [restaurant/1]).
% before: 
% :- default(cheap/1, 0.5).
rfuzzy_default_value_for(cheap/1, 0.5).
rfuzzy_default_value_for(cheap/1, 0.2) if thershold(near_the_city_center/1, over, 0.7).
rfuzzy_default_value_for(cheap/1, 0.1) if expensive_restaurant/1.

cheap(kenzo) value 0.3.
cheap(subway) value 1.
cheap(derroscas) value 1.

rfuzzy_synonym(cheap/1, unexpensive/1, prod, 1).
rfuzzy_antonym(cheap/1, expensive/1, prod, 1).

rfuzzy_quantifier(very/1, over, 0.7).

rfuzzy_type_for('fuzzy_rule', tempting_restaurant/1, [restaurant/1]).
rfuzzy_default_value_for(tempting_restaurant/1, 0.1).
tempting_restaurant(R) cred (min, 0.7) :~ min((low_distance(R), fnot(very(expensive(R))), traditional(R))).
tempting_restaurant(R) cred (min, 0.5) :~ low_distance(R).

distance_to_us(kenzo, 150).
distance_to_us(burguer_king, 500).
distance_to_us(il_tempietto, 100).
distance_to_us(pizza_jardin, 250).
% distance_to(unknown, 800).

rfuzzy_define_fuzzification(near_to_us/1, distance_to_us/2, near_function/2).

max_with_min_a_half(X, Y, Z) :- max(X, Y, W), min(W, 0.5, Z).
rfuzzy_aggregator(max_with_min_a_half/3).

adequate_restaurant(R) :~ fnot(very(expensive(R))).
rfuzzy_type_for('fuzzy_rule', preferred_restaurant/2, [restaurant/1, restaurant/1]).
