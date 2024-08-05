:- module(knight, [
    is_valid_knight_move/5,
    threat_from_knight/4
]).

:- use_module('../board').
:- use_module('../rules').

%checked of L vormige zet
is_valid_knight_move(_, FromRow, FromCol, ToRow, ToCol) :-
    (abs(ToRow - FromRow) =:= 2, abs(ToCol - FromCol) =:= 1);
    (abs(ToRow - FromRow) =:= 1, abs(ToCol - FromCol) =:= 2).

%bepaald alle mogelijke L vormige vakken die paard kan bereiken
threat_from_knight(Board, KingRow, KingCol, EnemyColor) :-
    member((DRow, DCol), [(2, 1), (2, -1), (-2, 1), (-2, -1), (1, 2), (1, -2), (-1, 2), (-1, -2)]),
    TargetRow is KingRow + DRow,
    TargetCol is KingCol + DCol,
    piece_at(Board, TargetRow, TargetCol, Piece),
    piece_color(Piece, EnemyColor),
    piece_type(Piece, knight).
