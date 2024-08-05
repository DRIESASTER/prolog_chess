:- module(queen, [
    is_valid_queen_move/5,
    threat_from_queen/4
]).

:- use_module('../board').
:- use_module('../rules').

%loper + toren
is_valid_queen_move(Board, FromRow, FromCol, ToRow, ToCol) :-
    ((FromRow = ToRow; FromCol = ToCol); abs(ToRow - FromRow) =:= abs(ToCol - FromCol)),
    rules:is_path_clear(Board, FromRow-FromCol, ToRow-ToCol).

%loper + toren 
threat_from_queen(Board, KingRow, KingCol, EnemyColor) :-
    %horizontaal
    (check_line(Board, KingRow, KingCol, EnemyColor, 0, 1,queen);
     check_line(Board, KingRow, KingCol, EnemyColor, 0, -1,queen);
     check_line(Board, KingRow, KingCol, EnemyColor, 1, 0,queen);
     check_line(Board, KingRow, KingCol, EnemyColor, -1, 0,queen)),
    %diagonaal
    (check_line(Board, KingRow, KingCol, EnemyColor, 1, 1,queen);
     check_line(Board, KingRow, KingCol, EnemyColor, 1, -1,queen);
     check_line(Board, KingRow, KingCol, EnemyColor, -1, 1,queen);
     check_line(Board, KingRow, KingCol, EnemyColor, -1, -1,queen)).
