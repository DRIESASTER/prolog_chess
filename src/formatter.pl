:- module(formatter, [
    format_previous_moves/5,
    print_possible_moves/2
]).
%deze module zorgt voor het formatteren van de output van de verschillende spelvarianten en opties


% format previous moves zal alle gepaseerde moves uit het pgn bestand deftig noteren in pgn notatie en nog extra turn number zetten wanneer dit nodig is
format_previous_moves(['*'], Turn, Color, State, Str):- !,
    format_previous_moves([], Turn, Color, State, Str).

format_previous_moves([], _, white, checkmate, ''):- !.
format_previous_moves([], _, white, stalemate, ''):- !.
format_previous_moves([], _, white, koth, ''):- !.

format_previous_moves([], Turn, white, _, Str):-
    format(string(TurnStr), "~d.", [Turn]),
    Str = TurnStr.

format_previous_moves([], _, black, _, '').

format_previous_moves([Move|T], Turn, white, State, Str) :-
    format(string(TurnMoveStr), "~d. ~w", [Turn, Move]),
    NextTurn is Turn + 1,
    format_previous_moves(T, NextTurn, black, State, RestStr),
    (RestStr = "" -> Str = TurnMoveStr; string_concat(TurnMoveStr, " ", TempStr), string_concat(TempStr, RestStr, Str)).

format_previous_moves([Move|T], Turn, black, State, Str) :-
    format(string(MoveStr), "~w", [Move]),
    format_previous_moves(T, Turn, white, State, RestStr),
    (RestStr = "" ->
        (State = checkmate -> Str = MoveStr; 
        format(string(NextTurnStr), "~d.", [Turn + 1]), string_concat(MoveStr, " ", NextTurnStr), Str = NextTurnStr)
    ; 
        string_concat(MoveStr, " ", TempStr), string_concat(TempStr, RestStr, Str)).

%S stelt alle vorige moves voor, zal deze pgn lijn elke keer opnieuw printen voor elke mogelijke move momenteel (TEST notatie) of slechts 1 keer met de beste move (AI keuze)
print_possible_moves([],_).
print_possible_moves([Move|Moves],S):-
    write(S),write(' '),write(Move),nl,
    print_possible_moves(Moves,S).
