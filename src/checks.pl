 :- module(checks, [
    is_in_check/2,
    is_checkmate/3,
    is_stalemate/3,
    coords_to_san/2,
    check_game_state/4
]).

:- use_module(board).
:- use_module(rules).
:- use_module(converter).

%test verschillende game condities (cut niet nodig bij check want kan checkmate zijn)
check_game_state(Board, Color,History, checkmate) :-
    is_checkmate(Board, Color, History),!.

check_game_state(Board, Color,_, check) :-
    is_in_check(Board, Color),!.

check_game_state(Board, Color,History, is_stalemate) :-
    is_stalemate(Board, Color, History),!.

check_game_state(Board, Color, _, koth) :-
    is_king_in_center(Board, Color).

check_game_state(_, _, _, normal).


% test of koning schaakt staat
is_in_check(Board, Color) :-
    find_king(Board, Color, KingRow, KingCol),
    is_king_under_attack(Board, KingRow, KingCol, Color).


% simpele recursie om koning te vinden
find_king(Board, Color, KingRow, KingCol) :-
    king_char(Color, KingChar),
    nth1(KingRow, Board, Row),
    nth1(KingCol, Row, KingChar).

%nodig om koning char te vinden op bord
king_char(white, 'K').
king_char(black, 'k').


is_king_in_center(Board, Color) :-
    find_king(Board, Color, KingRow, KingCol),
    king_in_center(KingRow, KingCol).

% Helper predicate to define the central squares
king_in_center(Row, Col) :-
    member(Row, [4, 5]),
    member(Col, [4, 5]).


% test voor schaakmat voor koning in kleur Color
is_checkmate(Board, Color, History) :-
    is_in_check(Board, Color),
    not(can_resolve_check(Board, Color, History)).

% zoekt of er manier is om uit schaak te geraken
can_resolve_check(Board, Color, History) :-
    generate_all_moves(Board, Color, History, AllMoves),
    member([From,To,Promotion], AllMoves),
    (Promotion = none ->
        apply_move(Board, Color, From, To,History, _, NewBoard)
    ;   apply_move(Board, Color, From, To, Promotion, History, _, NewBoard)
    ),
    not(is_in_check(NewBoard, Color)).

coords_to_san(Row-Col, Notation) :-
    ColCode is 96 + Col,  % 'a' is 97 in ASCII
    char_code(Column, ColCode),
    InvertedRow is Row,
    number_string(InvertedRow, RowStr),
    string_concat(Column, RowStr, Notation).

% test voor elke mogelijke legale move van koning
has_legal_move(Board, KingRow, KingCol, Color, History) :-
    % Generate candidate move positions within the board limits
    between(1, 8, MoveRow),
    between(1, 8, MoveCol),
    % Check if the move is legal (handling all conditions including check scenarios)
    rules:legal_move(Board, Color, KingRow-KingCol, MoveRow-MoveCol, History).

% stalemate check
is_stalemate(Board, Color, History) :-
    not(is_in_check(Board, Color)),  % Ensure the king is not in check
    generate_all_moves(Board, Color, History, AllMoves),
    all_moves_illegal(Board, Color, AllMoves, History).

% Test if there are no legal moves left for the player
all_moves_illegal(Board, Color, AllMoves, History) :-
    \+ (
        member([From, To, Promotion], AllMoves),  % Extract each move directly from the list
        (Promotion = none ->
            apply_move(Board, Color, From, To, History, _, NewBoard)
        ;   apply_move(Board, Color, From, To, Promotion, History, _, NewBoard)
        ),
        not(is_in_check(NewBoard, Color))
    ).
% zoekt alle speelstukken op het bord van X kleur
all_pieces(Board, Color, Pieces) :-
    findall(Row-_, (member(Row, Board), member(Piece, Row), piece_color(Piece, Color)), Pieces).

%%%move generation
% Generate all legal moves for a single piece
generate_moves_for_piece(Board, Color, FromRow, FromCol, History, Moves) :-
    piece_at(Board, FromRow, FromCol, Piece),
    findall([FromRow-FromCol, ToRow-ToCol, PromotionPiece], % Capture PromotionPiece in the result
        (   between(1, 8, ToRow),
            between(1, 8, ToCol),
            determine_move(Board, Color, FromRow, FromCol, ToRow, ToCol, Piece, PromotionPiece, History)
        ), Moves).

% Determine the type of move based on piece and position
determine_move(Board, Color, FromRow, FromCol, ToRow, ToCol, Piece, PromotionPiece, History) :-
    (piece_type(Piece, pawn), % Check if the piece is a pawn
    (ToRow == 1 ; ToRow == 8) -> % Pawn is at the promotion row
    member(Promotion, ['Q','R','B','N']), 
    converter:convert_to_correct_color(Promotion,Color,PromotionPiece),
    legal_move(Board, Color, FromRow-FromCol, ToRow-ToCol, PromotionPiece));
    % Regular move for pawns or other pieces
    (PromotionPiece = none,
    legal_move(Board, Color, FromRow-FromCol, ToRow-ToCol, History)).

% Utility predicate to get the symbol of a piece based on its type and color
piece_symbol(Color, Type, Symbol) :-
    % Assuming piece_type/2 maps a piece to its type as shown earlier
    piece_type(Symbol, Type),
    piece_color(Symbol, Color).

% Generate all legal moves for all pieces of a specific color
generate_all_moves(Board, Color, History, AllMoves) :-
    findall(Move,
            (   piece_at(Board, Row, Col, Piece),
                piece_color(Piece, Color),
                generate_moves_for_piece(Board, Color, Row, Col, History, PieceMoves),
                member(Move, PieceMoves)
            ), AllMoves).