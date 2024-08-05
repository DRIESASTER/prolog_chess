:- module(move_tests, [run_tests/0]).


:- use_module('../../src/board').

run_tests :-
    run_tests([chess_game]).

:- begin_tests(chess_game).

test(standard_move) :-
    initial_board(Board),
    apply_move(Board,white, 2-5,4-5,[],_, NewBoard),
    piece_at(NewBoard, 4, 5, Piece),
    print(NewBoard),
    assertion(Piece == 'P').


test(castling) :-
    initial_board(Board),
    set_piece(Board, 1, 2, '.', Temp1),
    set_piece(Temp1, 1, 3, '.', Temp2),
    set_piece(Temp2, 1, 4, '.', Temp3), 
    print_board(Temp3),
    apply_move(Temp3, white, 1-5,1-3,[],_, NewBoard),
    piece_at(NewBoard,1,3,Piece),
    assertion(Piece=='K'),
    piece_at(NewBoard,1,4,Piece2),
    assertion(Piece2=='R').

test(en_passant) :-
    initial_board(Board),
    apply_move(Board, white, 2-5,4-5,[],H, TempBoard1),
    apply_move(TempBoard1, black, 7-1,6-1,H,H2, TempBoard2),
    apply_move(TempBoard2, white, 4-5, 5-5,H2,H3, TempBoard3),
    apply_move(TempBoard3, black,7-4,5-4,H3,H4,TempBoard4),
    apply_move(TempBoard4, white,5-5,6-4,H4,_, NewBoard),
    piece_at(NewBoard, 6, 4, Piece),
    assertion(Piece == 'P'),
    empty_at(NewBoard, 5, 4).


test(illegal_move) :-
    initial_board(Board),
    \+ apply_move(Board, white, 1-5,5-5,[],_, _).



test(castling_through_check) :-
    initial_board(Board),
    set_piece(Board, 1, 6, 'q', TempBoard),
    \+ apply_move(TempBoard, white, 1-5,1-7,[],_, _).

test(pawn_capture) :-
    initial_board(Board),
    apply_move(Board, white, 2-5, 4-5,[],_, Temp),
    apply_move(Temp, black, 7-4,5-4,[],_, Temp2),
    apply_move(Temp2, white,4-5,5-4,[],_, NewBoard),
    print_board(NewBoard),
    piece_at(NewBoard, 5, 4, Piece),
    assertion(Piece == 'P').


test(knight_standard_move) :-
    initial_board(Board),
    apply_move(Board, white,1-2,3-3,[],_, NewBoard),
    piece_at(NewBoard, 3, 3, Piece),
    assertion(Piece == 'N'),
    apply_move(NewBoard, white, 3-3,4-5,[],_, NextBoard),
    piece_at(NextBoard, 4, 5, NextPiece),
    assertion(NextPiece == 'N').


test(knight_move_blocked) :-
    initial_board(Board),
    set_piece(Board, 3, 3, 'B', TempBoard),
    \+ apply_move(TempBoard, white, 1-2, 3-3,[],_, _).

test(knight_illegal_move) :-
    initial_board(Board),
    \+ apply_move(Board, white, 1-2, 3-2,[],_, _).

test(knight_capture) :-
    initial_board(Board),
    set_piece(Board, 3, 3, 'p', TempBoard),
    apply_move(TempBoard, white, 1-2,3-3,[],_, NewBoard),
    piece_at(NewBoard, 3, 3, Piece),
    assertion(Piece == 'N').

test(knight_jump_over_pieces) :-
    initial_board(Board),
    set_piece(Board, 2, 2, 'P', TempBoard),
    apply_move(TempBoard, white, 1-2,3-3,[],_, NewBoard),
    piece_at(NewBoard, 3, 3, Piece),
    assertion(Piece == 'N').

:- end_tests(chess_game).

