:- module(bishop, [
    is_valid_bishop_move/5,
    threat_from_bishop/4
]).

:- use_module('../board').
:- use_module('../rules').

%beweging moet diagonaal zijn over vrij pad
is_valid_bishop_move(Board, FromRow, FromCol, ToRow, ToCol) :-
    abs(ToRow - FromRow) =:= abs(ToCol - FromCol),
    is_path_clear(Board, FromRow-FromCol, ToRow-ToCol).

%bishop checked alle diagonale lijnen
threat_from_bishop(Board, KingRow, KingCol, EnemyColor) :-
    (check_line(Board, KingRow, KingCol, EnemyColor, 1, 1,bishop);
     check_line(Board, KingRow, KingCol, EnemyColor, 1, -1,bishop);
     check_line(Board, KingRow, KingCol, EnemyColor, -1, 1,bishop);
     check_line(Board, KingRow, KingCol, EnemyColor, -1, -1,bishop)).