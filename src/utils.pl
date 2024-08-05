:- module(utils, [
    switch_player/2,
    uppercase_to_lowercase/2,
    convert_to_correct_color/3
]).
%some common nice to have utilities that multiple modules use


% simpel wisselspeler predikaat
switch_player(white, black).
switch_player(black, white).

%zet stuk om naar juiste kleur (low of uppercase)
convert_to_correct_color(P,black,C):- uppercase_to_lowercase(P,C).
convert_to_correct_color(P,white,P).

uppercase_to_lowercase(U, L) :-
    char_code(U, CodeU),
    CodeL is CodeU + 32,
    char_code(L, CodeL).
