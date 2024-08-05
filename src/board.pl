:- module(board, [
    initial_board/1,
    piece_at/4,
    empty_at/3,
    set_piece/5,
    apply_move/7,
    apply_move/8,
    has_moved/5,
    print_board/1
]).

:- use_module(rules).
:- use_module(checks).
:- use_module('moves/pawn').

% schaakbord, hoofdletters zijn wit, lowercase zwart
initial_board([
    ['R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R'],
    ['P', 'P', 'P', 'P', 'P', 'P', 'P', 'P'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p'],
    ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r']
]).


%aantal predicaten om makkelijk bij te houden wie en welke moves er gedaan zijn
:- dynamic moves/1.
%is nodig als meerdere spellen na elkaar om te clearen
init_moves :-
    retractall(moves(_)),
    assertz(moves([]-[])).

%simpelweg Move toevoegen door dynamische moves staart van de lijst in te vullen
add_move(Move) :-
    moves(Moves-T),
    retract(moves(Moves-T)),
    NewTail = [Move|T2],
    assertz(moves(Moves-NewTail)),
    T = T2.

%vraagt lijst op van alle moves zonder tail
get_all_moves(Moves) :-
    moves(Moves-[]).

% vraagt stuk op specifieke coordinaat aan, BoardRow stelt effectieve lijst van die rij van het bord voor
piece_at(Board, Row, Col, Piece) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Piece).

% simpele check of op X,Y een '.' staat => leeg vakje
empty_at(Board, Row, Col) :-
    piece_at(Board, Row, Col, '.').

% zet een stuk op het bord, zal eerst stuk in rij vervangen en dan die nieuwe rij in ons bord steken
set_piece(Board, Row, Col, Piece, NewBoard) :-
    nth1(Row, Board, BoardRow),
    replace_in_row(BoardRow, Col, Piece, NewRow),
    replace_in_board(Board, Row, NewRow, NewBoard).

% tussen aanroep om rij te vervangen
replace_in_row(Row, Col, Piece, NewRow) :-
    replace_at_index(Row, Col, Piece, NewRow).

% recursieve oproep, trekt index af tot die 1 is en hangt dan de stukken aan elkaar
replace_at_index([_|T], 1, E, [E|T]).
replace_at_index([H|T], I, E, [H|R]) :-
    I > 1,
    I2 is I - 1,
    replace_at_index(T, I2, E, R).

% nu hebben ge aangepaste rij maar moeten nog zorgen dat deze effectief in ons bord staat
replace_in_board([_|T], 1, NewRow, [NewRow|T]).
replace_in_board([H|T], I, NewRow, [H|R]) :-
    I > 1,
    NI is I - 1,
    replace_in_board(T, NI, NewRow, R).

% een move uitvoeren door middel van om te zetten van PGN naar coordinatensysteem en te checken of de move legaal is en dan uiteindelijk te executen
apply_move(Board,Color, FromRow-FromCol, ToRow-ToCol,History,NewHistory, NewBoard) :-
    piece_at(Board,FromRow,FromCol,Piece),
    rules:legal_move(Board, Color, FromRow-FromCol, ToRow-ToCol,History),
    execute_move(Board, FromRow, FromCol, ToRow, ToCol, Piece, Color,History,NewHistory, NewBoard).

%optie voor promotie
apply_move(Board, Color, FromRow-FromCol, ToRow-ToCol, PromotionPiece, History, NewHistory, NewBoard) :-
    piece_at(Board, FromRow, FromCol, Piece),
    rules:legal_move(Board, Color, FromRow-FromCol, ToRow-ToCol, PromotionPiece, History),
    execute_move(Board, FromRow, FromCol, ToRow, ToCol, Piece, Color, PromotionPiece,History, NewHistory, NewBoard).

%check voor rokade
execute_move(Board, FromRow, FromCol, ToRow, ToCol, Piece, Color,History,NewHistory, NewBoard) :-
    downcase_atom(Piece, LoPiece),(LoPiece == 'k'),castling_move(FromRow-FromCol, ToRow-ToCol, RookFrom-RookTo), apply_castling(Board, FromRow-FromCol, ToRow-ToCol, RookFrom, RookTo, NewBoard), update_move_history(FromRow-FromCol, ToRow-ToCol, Piece, Color, History, NewHistory).

%check voor en passant
execute_move(Board, FromRow, FromCol, ToRow, ToCol, Piece, Color,History, NewHistory, NewBoard) :-
    %we weten al dat move legaal is, dit is simpelste detectie in board of move en passant zal zijn (test niet op legaliteit)
    valid_en_passant(Board,FromRow,FromCol,ToRow,ToCol,Color, History),
    apply_en_passant(Board, FromRow-FromCol, ToRow-ToCol, Piece, Color,History, NewHistory, NewBoard),
    update_move_history(FromRow-FromCol, ToRow-ToCol, Piece, Color, History, NewHistory).

%anders gewone move
execute_move(Board, FromRow, FromCol, ToRow, ToCol, Piece, Color,History, NewHistory, NewBoard) :-
    set_piece(Board, FromRow, FromCol, '.', TempBoard),
    set_piece(TempBoard, ToRow, ToCol, Piece, NewBoard),
    update_move_history(FromRow-FromCol, ToRow-ToCol, Piece, Color, History, NewHistory).


execute_move(Board, FromRow, FromCol, ToRow, ToCol, Piece, Color, PromotionPiece, History, NewHistory, NewBoard) :-
    set_piece(Board, FromRow, FromCol, '.', TempBoard),
    set_piece(TempBoard, ToRow, ToCol, PromotionPiece, NewBoard),
    update_move_history(FromRow-FromCol, ToRow-ToCol, Piece, Color, History, NewHistory).


apply_en_passant(Board, FromRow-FromCol, ToRow-ToCol, Piece, Color,History,NewHistory, NewBoard) :-
    (Color = 'white', CapturedRow is ToRow - 1; Color = 'black', CapturedRow is ToRow + 1),
    set_piece(Board, FromRow, FromCol, '.', TempBoard),
    set_piece(TempBoard, CapturedRow, ToCol, '.', TempBoard2),
    set_piece(TempBoard2, ToRow, ToCol, Piece, NewBoard),
    update_move_history(FromRow-FromCol, ToRow-ToCol, Piece, Color, History, NewHistory).

% bepaalt rook coord op basis van koning castling
castling_move(FromRow-FromCol, ToRow-ToCol, RookFrom-RookTo) :-
    (FromRow = 1, FromCol = 5, ToRow = 1, ToCol = 7, RookFrom = 1-8, RookTo = 1-6);
    (FromRow = 1, FromCol = 5, ToRow = 1, ToCol = 3, RookFrom = 1-1, RookTo = 1-4);
    (FromRow = 8, FromCol = 5, ToRow = 8, ToCol = 7, RookFrom = 8-8, RookTo = 8-6);
    (FromRow = 8, FromCol = 5, ToRow = 8, ToCol = 3, RookFrom = 8-1, RookTo = 8-4).

apply_castling(Board, KingFromRow-KingFromCol, KingToRow-KingToCol, RookFromRow-RookFromCol, RookToRow-RookToCol, NewBoard) :-
    piece_at(Board, KingFromRow, KingFromCol, King),
    piece_at(Board, RookFromRow, RookFromCol, Rook),
    set_piece(Board, KingFromRow, KingFromCol, '.', TempBoard1),
    set_piece(TempBoard1, KingToRow, KingToCol, King, TempBoard2),
    set_piece(TempBoard2, RookFromRow, RookFromCol, '.', TempBoard3),
    set_piece(TempBoard3, RookToRow, RookToCol, Rook, NewBoard).

update_move_history(From, To, Piece, Color, History, [(From, To, Piece, Color)|History]).

has_moved(Type, Color, FromRow, FromCol, History) :-
    member((FromRow-FromCol, _, Type, Color), History).



% Functie om het bord te printen met behulp van ANSI Escape codes
print_board(Board) :-
    clear_screen,
    print_header,
    reverse(Board, FlippedBoard),
    print_rows(FlippedBoard, 8),
    write('  a   b   c   d   e   f   g   h'), nl.

% ANSI Escape code om het hele scherm te wissen
clear_screen :-
    write('\x1B[2J'), % Clear the screen
    write('\x1B[H').  % Move cursor to home position

% Print kolom headers
print_header :-
    write('+---+---+---+---+---+---+---+---+'), nl.

% Print alle rijen met rijnummers
print_rows([], _).
print_rows([Row|Rest], Num) :-
    write('|'),
    maplist(print_piece, Row),
    write(' '), write(Num), nl,
    write('+---+---+---+---+---+---+---+---+'), nl,
    NextNum is Num - 1,
    print_rows(Rest, NextNum).

% Print individueel schaakstuk
print_piece(Piece) :-
    Piece = '.',
    !,
    write('   |'). % Geen punt tonen voor lege plekken
print_piece(Piece) :-
    write(' '), write(Piece), write(' |').