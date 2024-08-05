:- module(rules_pawn, [
    is_valid_pawn_move/7, 
    valid_en_passant/7,
    threat_from_pawn/4
]).

:- use_module('../board').
:- use_module('../rules').


%test voor geldige pion zet, moet vooruit zijn, mag 2 stappen zijn vanuit startpositie, als diagonaal is moet het een capture zijn of en passant
is_valid_pawn_move(Board, Piece, FromRow, FromCol, ToRow, ToCol, History) :-
    (
        (Piece = 'p',
            NewToRow is FromRow - 1,
            (
                (NewToRow =:= ToRow, FromCol = ToCol, empty_at(Board, ToRow, ToCol));
                (
                    NewToRow =:= ToRow, abs(ToCol - FromCol) =:= 1, \+ empty_at(Board, ToRow, ToCol),
                    piece_at(Board,NewToRow,ToCol,TargetPiece),
                    \+same_color(_,Piece,TargetPiece)
                );
                (FromRow = 7, ToRow =:= FromRow - 2, FromCol = ToCol, empty_at(Board, ToRow, ToCol),
                 MidRow is FromRow - 1, empty_at(Board, MidRow, FromCol));
                (History \= [], (ToRow =:= FromRow - 1, abs(ToCol - FromCol) =:= 1, valid_en_passant(Board, FromRow, FromCol, ToRow, ToCol, black, History)))
            )
        )
    ;
        (Piece = 'P',
            NewToRow is FromRow + 1,
            (
                (NewToRow =:= ToRow, FromCol = ToCol, empty_at(Board, ToRow, ToCol));
                (
                    NewToRow =:= ToRow, abs(ToCol - FromCol) =:= 1, \+ empty_at(Board, ToRow, ToCol),
                    piece_at(Board,NewToRow,ToCol,TargetPiece),
                    \+same_color(_,Piece,TargetPiece)
                );
                (FromRow = 2, ToRow =:= FromRow + 2, FromCol = ToCol, empty_at(Board, ToRow, ToCol),
                 MidRow is FromRow + 1, empty_at(Board, MidRow, FromCol));
                (History \= [], (ToRow =:= FromRow + 1, abs(ToCol - FromCol) =:= 1, valid_en_passant(Board, FromRow, FromCol, ToRow, ToCol, white, History)))
            )
        )
    ).

%kijkt voor geldige en passant op basis van vorige zet uit de geschiedenis
valid_en_passant(Board, FromRow, FromCol, ToRow, ToCol, Color, History) :-
    History = [(LastFromRow-_, LastToRow-LastToCol, LastPiece, LastColor)|_],
    LastColor \= Color,
    ((Color = white, LastPiece = 'p', LastFromRow = 7, LastToRow = 5);
     (Color = black, LastPiece = 'P', LastFromRow = 2, LastToRow = 4)),
    LastToRow =:= FromRow,
    abs(LastToCol - FromCol) =:= 1,
    (Color = black -> ToRow =:= LastToRow - 1; ToRow =:= LastToRow + 1),
    ToCol =:= LastToCol,
    empty_at(Board, ToRow, ToCol).

%moet diagonaal vooraan zijn van pion voor threatened te zijn
threat_from_pawn(Board, KingRow, KingCol, EnemyColor) :-
    (EnemyColor ==  black -> DRow is 1; DRow is -1),
    member(DCol, [-1, 1]),
    TargetRow is KingRow + DRow,
    TargetCol is KingCol + DCol,
    piece_at(Board, TargetRow, TargetCol, Piece),
    piece_color(Piece, EnemyColor),
    piece_type(Piece, pawn).
