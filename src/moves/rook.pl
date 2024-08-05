:- module(rook, [
    is_valid_rook_move/5,
    threat_from_rook/4
]).

:- use_module('../board').
:- use_module('../rules').

%ofwel veranderd rij ofwel kolom, niet beide.
is_valid_rook_move(Board, FromRow, FromCol, ToRow, ToCol) :-
    (FromRow = ToRow; FromCol = ToCol),
    is_path_clear(Board, FromRow-FromCol, ToRow-ToCol).

%horizontaal + verticaal
threat_from_rook(Board, KingRow, KingCol, EnemyColor) :-
    (check_line(Board, KingRow, KingCol, EnemyColor, 0, 1,rook);
     check_line(Board, KingRow, KingCol, EnemyColor, 0, -1,rook);
     check_line(Board, KingRow, KingCol, EnemyColor, 1, 0,rook);
     check_line(Board, KingRow, KingCol, EnemyColor, -1, 0,rook)).
