:- module(king, [
    is_valid_king_move/7
]).

:- use_module('../board').
:- use_module('../rules').

is_valid_king_move(_,_, FromRow, FromCol, ToRow, ToCol, _) :-
    (   abs(ToRow - FromRow) =< 1,
        abs(ToCol - FromCol) =< 1
    ->  true
    ).

% check voor rokade
is_valid_king_move(Board, Piece, FromRow, FromCol, ToRow, ToCol, History) :-
    FromCol == 5,
    member(ToCol, [7, 3]),
    piece_color(Piece, Color),
    correct_row_for_color(FromRow, Color),
    correct_row_for_color(ToRow, Color),
    not(has_moved('K', Color, FromRow, FromCol, History)),
    determine_rookcol(ToCol, RookCol),
    not(has_moved('R', Color, FromRow, RookCol, History)),
    path_clear_for_castling(Board, FromRow, FromCol, ToCol),
    no_threats_during_castling(Board, FromRow, FromCol, ToCol, Color).

%kijkt of pad vrij is voor rokade beweging
path_clear_for_castling(Board, FromRow, FromCol, ToCol) :-
    castling_range(FromCol, ToCol, StartCol, EndCol),
    foreach_column_empty(Board, FromRow, StartCol, EndCol).

foreach_column_empty(Board, Row, StartCol, EndCol) :-
    forall(between(StartCol, EndCol, Col),empty_at(Board, Row, Col)).

% bepaald welke rijen leeg moeten zijn voor deze rokade zet (queenside, kingside castling)
castling_range(FromCol, ToCol, StartCol, EndCol) :-
    (   FromCol < ToCol
    ->  StartCol is FromCol + 1,
        EndCol is ToCol
    ;   EndCol is ToCol + 1,
        StartCol is FromCol - 3
    ).

% mogen geen bedreigingen op pad zijn tijdens rokade
no_threats_during_castling(Board, KingFromRow, KingFromCol, KingToCol, Color) :-
    (KingFromCol < KingToCol ->
        Start = KingFromCol, End = KingToCol
    ;   Start = KingToCol, End = KingFromCol),
    between(Start, End, Col),
    not(is_king_under_attack(Board, KingFromRow, Col, Color)),
    fail.
no_threats_during_castling(_, _, _, _, _).


correct_row_for_color(Row, white) :- Row = 1.
correct_row_for_color(Row, black) :- Row = 8.

determine_rookcol(7,8).
determine_rookcol(_, 1).