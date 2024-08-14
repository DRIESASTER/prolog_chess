:-module(rules, [
    legal_move/5,
    legal_move/6,
    is_valid_move/7,
    same_color/3,
    is_king_under_attack/4,
    is_path_clear/3,
    check_line/7,
    piece_color/2,
    piece_type/2
]).

:- use_module(board).
:- use_module('moves/pawn').
:- use_module('moves/king').
:- use_module('moves/rook').
:- use_module('moves/queen').
:- use_module('moves/bishop').
:- use_module('moves/knight').
:- use_module(checks).


% test of move legaal is
legal_move(Board, Color, FromRow-FromCol, ToRow-ToCol, History) :-
    %check of piece niet leeg is
    piece_at(Board, FromRow, FromCol, Piece),
    Piece \= '.',
    %check of piece effectief juiste kleur is
    piece_color(Piece,Color2),
    Color2 == Color,
    valid_target(Board, ToRow, ToCol, Piece, Color),
    is_valid_move(Board, Piece, FromRow, FromCol, ToRow, ToCol, History),
    \+ would_move_put_king_in_check(Board, Color, FromRow-FromCol, ToRow-ToCol).


legal_move(Board, Color, FromRow-FromCol, ToRow-ToCol, PromotionPiece, History) :-
    % Check if the piece is not empty
    piece_at(Board, FromRow, FromCol, Piece),
    piece_type(Piece,pawn),
    (ToRow == 1 ; ToRow == 8),
    piece_color(Piece, Color2),
    Color2 == Color,
    % Check if the target square is valid
    valid_target(Board, ToRow, ToCol, Piece, Color),
    % Check if the move itself is valid
    is_valid_move(Board, Piece, FromRow, FromCol, ToRow, ToCol, History),
    % Ensure the move doesn't put the king in check
    \+ would_move_put_king_in_check(Board, Color, FromRow-FromCol, ToRow-ToCol,PromotionPiece),
    % Check for valid promotion moves
    piece_type(PromotionPiece,_).


would_move_put_king_in_check(Board, Color, FromRow-FromCol, ToRow-ToCol,PromotionPiece) :-
    set_piece(Board, FromRow, FromCol, '.', TempBoard1),
    set_piece(TempBoard1, ToRow, ToCol, PromotionPiece, TempBoard2),
    checks:find_king(TempBoard2, Color, KingRow, KingCol),
    is_king_under_attack(TempBoard2, KingRow, KingCol, Color).


would_move_put_king_in_check(Board, Color, FromRow-FromCol, ToRow-ToCol) :-
    piece_at(Board, FromRow, FromCol, Piece),
    set_piece(Board, FromRow, FromCol, '.', TempBoard1),
    set_piece(TempBoard1, ToRow, ToCol, Piece, TempBoard2),
    checks:find_king(TempBoard2, Color, KingRow, KingCol),
    is_king_under_attack(TempBoard2, KingRow, KingCol, Color).

%als target leeg is valid
valid_target(Board,ToRow,ToCol,_,_):-
    empty_at(Board, ToRow, ToCol),!.

%als target enemy piece is valid
valid_target(Board,ToRow,ToCol,_,Color):-
    piece_at(Board, ToRow, ToCol, TargetPiece),
    piece_color(TargetPiece,TargetColor),
    Color \= TargetColor,!.


% parsed piece en verwijst naar correcte move eval code
is_valid_move(Board, Piece, FromRow, FromCol, ToRow, ToCol, History) :-
    (Piece = 'P' ; Piece = 'p') -> is_valid_pawn_move(Board, Piece, FromRow, FromCol, ToRow, ToCol, History);
    (Piece = 'N' ; Piece = 'n') -> is_valid_knight_move(Board, FromRow, FromCol, ToRow, ToCol);
    (Piece = 'B' ; Piece = 'b') -> is_valid_bishop_move(Board, FromRow, FromCol, ToRow, ToCol);
    (Piece = 'R' ; Piece = 'r') -> is_valid_rook_move(Board, FromRow, FromCol, ToRow, ToCol);
    (Piece = 'Q' ; Piece = 'q') -> is_valid_queen_move(Board, FromRow, FromCol, ToRow, ToCol);
    (Piece = 'K' ; Piece = 'k') -> is_valid_king_move(Board,Piece, FromRow, FromCol, ToRow, ToCol, History).


% test of pad tussen 2 stukken vrij is om te schuiven (niet relevant voor knight dus)
%als rij niet veranderd
is_path_clear(Board, FromRow-FromCol, FromRow-ToCol) :-
    is_path_clear_horizontal(Board, FromRow, FromCol, ToCol),!.

%als col niet veranderd
is_path_clear(Board, FromRow-FromCol, ToRow-FromCol) :-
    is_path_clear_vertical(Board, FromRow, FromCol, ToRow),!.

%als beide veranderen
is_path_clear(Board, FromRow-FromCol, ToRow-ToCol) :-
    is_path_clear_diagonal(Board, FromRow, FromCol, ToRow, ToCol),!.

%naar rechts kijken
is_path_clear_horizontal(Board, Row, FromCol, ToCol) :-
    FromCol < ToCol,!, Start is FromCol + 1, End is ToCol - 1,
    forall(between(Start, End, Col), empty_at(Board, Row, Col)).

%links kijken
is_path_clear_horizontal(Board, Row, FromCol, ToCol) :-
    Start is ToCol + 1, End is FromCol - 1,
    forall(between(Start, End, Col), empty_at(Board, Row, Col)).

%boven
is_path_clear_vertical(Board, FromRow, Col, ToRow) :-
    FromRow < ToRow,!,Start is FromRow + 1, End is ToRow - 1,
    forall(between(Start, End, Row), empty_at(Board, Row, Col)).

%onder
is_path_clear_vertical(Board, FromRow, Col, ToRow) :-
    Start is ToRow + 1, End is FromRow - 1,
    forall(between(Start, End, Row), empty_at(Board, Row, Col)).

is_path_clear_diagonal(Board, FromRow, FromCol, ToRow, ToCol) :-
    DiffRow is ToRow - FromRow,
    DiffCol is ToCol - FromCol,
    abs(DiffRow) =:= abs(DiffCol),
    sign(DiffRow, RowStep),
    sign(DiffCol, ColStep),
    NextRow is FromRow + RowStep,
    NextCol is FromCol + ColStep,
    check_diagonal_path(Board, NextRow, NextCol, ToRow, ToCol, RowStep, ColStep).

%einde gevonden
check_diagonal_path(_, Row, Col, Row, Col, _, _) :- !.

check_diagonal_path(Board, Row, Col, ToRow, ToCol, RowStep, ColStep) :-
    empty_at(Board, Row, Col),
    NextRow is Row + RowStep,
    NextCol is Col + ColStep,
    check_diagonal_path(Board, NextRow, NextCol, ToRow, ToCol, RowStep, ColStep).

sign(Number, Sign) :- Number > 0,!, Sign is 1.
sign(Number, Sign) :- Number < 0,!, Sign is -1.

% Check if two pieces are of the same color
same_color(white, Piece1, Piece2) :- char_type(Piece1, upper), char_type(Piece2, upper).
same_color(black, Piece1, Piece2) :- char_type(Piece1, lower), char_type(Piece2, lower).

% Check if the king is under attack
is_king_under_attack(Board, KingRow, KingCol, Color) :-
    % Check threats from all directions and pieces
    enemy_color(Color, EnemyColor),
    (   threat_from_knight(Board, KingRow, KingCol, EnemyColor);
        threat_from_pawn(Board, KingRow, KingCol, EnemyColor);
        threat_from_queen(Board, KingRow, KingCol, EnemyColor);
        threat_from_bishop(Board, KingRow, KingCol, EnemyColor);
        threat_from_rook(Board, KingRow, KingCol, EnemyColor)
    ).

enemy_color(white, black).
enemy_color(black, white).

% ga rij af in richting van rowInc, colInc tot out of bound of enemy piece dat we zoeken of queen (kan altijd aanvallen vanaf directe lijn)
check_line(Board, KingRow, KingCol, EnemyColor, RowInc, ColInc, PieceType) :-
    next_square(KingRow, KingCol, RowInc, ColInc, NextRow, NextCol), 
    not_out_of_bounds(NextRow, NextCol),
    piece_at(Board, NextRow, NextCol, '.'),!,
    check_line(Board, NextRow, NextCol, EnemyColor, RowInc, ColInc, PieceType).


check_line(Board, KingRow, KingCol, EnemyColor, RowInc, ColInc, PieceType) :-
    next_square(KingRow, KingCol, RowInc, ColInc, NextRow, NextCol),
    not_out_of_bounds(NextRow, NextCol) ,
        piece_at(Board, NextRow, NextCol, Piece),
        piece_type(Piece, FoundType),
        piece_color(Piece, PieceColor),
        PieceColor == EnemyColor, 
        (FoundType == PieceType ; FoundType == queen).

% volgende vakje bepalen a.h.v meegegeven waarden (rook horizontaal, bishop diagonaal etc..)
next_square(CurRow, CurCol, RowInc, ColInc, NextRow, NextCol) :-
    NextRow is CurRow + RowInc,
    NextCol is CurCol + ColInc.

%simpele out of bounds check
not_out_of_bounds(Row, Col) :-
    Row > 0; Row < 9; Col > 0; Col < 9.

% simple char to name conversion
piece_type(Piece, king) :- downcase_atom(Piece, Lower), sub_atom(Lower, 0, 1, _, 'k').
piece_type(Piece, queen) :- downcase_atom(Piece, Lower), sub_atom(Lower, 0, 1, _, 'q').
piece_type(Piece, rook) :- downcase_atom(Piece, Lower), sub_atom(Lower, 0, 1, _, 'r').
piece_type(Piece, bishop) :- downcase_atom(Piece, Lower), sub_atom(Lower, 0, 1, _, 'b').
piece_type(Piece, knight) :- downcase_atom(Piece, Lower), sub_atom(Lower, 0, 1, _, 'n').
piece_type(Piece, pawn) :- downcase_atom(Piece, Lower), sub_atom(Lower, 0, 1, _, 'p').

% bepaalt kleur van schaakstuk
piece_color(Piece,white):- char_type(Piece, upper).
piece_color(Piece, black) :- char_type(Piece, lower).

% test voor castling trheats
no_threats_during_castling(Board, KingFromRow, KingFromCol, KingToCol, Color) :-
    between(KingFromCol, KingToCol, Col),
    not(is_king_under_attack(Board, KingFromRow, Col, Color)).