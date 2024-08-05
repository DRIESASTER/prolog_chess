:- module(rules_tests, [run_tests/0]).
:- use_module(library(plunit)).
:- use_module('../../src/board').
:- use_module('../../src/rules').
:- use_module('../../src/converter').

run_tests :-
    run_tests([pawn_moves, knight_moves, rook_moves, queen_moves, king_moves]).

:- begin_tests(pawn_moves).

test(valid_white_pawn_move) :-
    initial_board(Board),
    convert_move('e4',Board,white, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    rules:legal_move(Board, white, FromRow-FromCol, ToRow-ToCol, 'P').

test(invalid_white_pawn_move) :-
    initial_board(Board),
    convert_move('e5',Board,white, FromRow-FromCol, ToRow-ToCol, IsLegal, Promotion, History),
    \+ rules:legal_move(Board, white, FromRow-FromCol, ToRow-ToCol, 'P').

test(valid_black_pawn_move) :-
    initial_board(Board),
    convert_move('e5',Board,black, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    rules:legal_move(Board, black, FromRow-FromCol, ToRow-ToCol, 'p').

test(invalid_black_pawn_move) :-
    initial_board(Board),
    convert_move('e8',Board,black, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    \+ rules:legal_move(Board, black, FromRow-FromCol, ToRow-ToCol, 'p').

:- end_tests(pawn_moves).

:- begin_tests(knight_moves).

test(valid_white_knight_move) :-
    initial_board(Board),
    convert_move('Nf3',Board,white, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    rules:legal_move(Board, white, FromRow-FromCol, ToRow-ToCol, 'N').

test(invalid_white_knight_move) :-
    initial_board(Board),
    convert_move('Ng6',Board,white, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    \+ rules:legal_move(Board, white, FromRow-FromCol, ToRow-ToCol, 'N').

:- end_tests(knight_moves).

:- begin_tests(rook_moves).

test(valid_black_rook_move) :-
    initial_board(Board),
    set_piece(Board, 8, 2, '.', TempBoard),
    convert_move('Rb8',Board,black, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    rules:legal_move(TempBoard, black, FromRow-FromCol, ToRow-ToCol, 'r').

test(invalid_black_rook_move) :-
    initial_board(Board),
    convert_move('a8', Board,black,FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    \+ rules:legal_move(Board, black, FromRow-FromCol, ToRow-ToCol, 'r').

:- end_tests(rook_moves).

:- begin_tests(queen_moves).

test(valid_queen_move_diagonal_clear_path) :-
    initial_board(Board),
    set_piece(Board, 7, 5, '.', TempBoard),
    convert_move('Qh4',Board,black, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    rules:legal_move(TempBoard, black, FromRow-FromCol, ToRow-ToCol, 'q').

test(invalid_queen_move_blocked_path) :-
    initial_board(Board),
    convert_move('Qh4',Board,black, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    \+ rules:legal_move(Board, black, FromRow-FromCol, ToRow-ToCol, 'q').

:- end_tests(queen_moves).

:- begin_tests(king_moves).

test(valid_black_king_castling) :-
    initial_board(Board),
    set_piece(Board, 8, 6, '.', TempBoard1),
    set_piece(TempBoard1, 8, 7, '.', TempBoard2),
    convert_move('O-O',TempBoard2,black, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    rules:legal_move(TempBoard2, black, FromRow-FromCol, ToRow-ToCol, 'k').

test(invalid_white_king_castling) :-
    initial_board(Board),
    convert_move('O-O',Board,white, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    \+ rules:legal_move(Board, white, FromRow-FromCol, ToRow-ToCol, 'K').

test(invalid_black_king_castling) :-
    initial_board(Board),
    convert_move('O-O',Board,black, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    \+ rules:legal_move(Board, black, FromRow-FromCol, ToRow-ToCol, 'k').

test(invalid_white_castling_through_check) :-
    initial_board(Board),
    set_piece(Board, 1, 6, '.', TempBoard1),
    set_piece(TempBoard1, 1, 7, '.', TempBoard2),
    set_piece(TempBoard2, 1, 5, '.', TempBoard3),
    set_piece(TempBoard3, 5, 6, 'q', FinalBoard),
    convert_move('O-O',FinalBoard,white, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    \+ rules:legal_move(FinalBoard, white, FromRow-FromCol, ToRow-ToCol, 'K').

test(valid_white_king_castling) :-
    initial_board(Board),
    set_piece(Board, 1, 6, '.', TempBoard1),
    set_piece(TempBoard1, 1, 7, '.', TempBoard2),
    convert_move('O-O',TempBoard2,white, FromRow-FromCol, ToRow-ToCol,IsLegal, Promotion, History),
    rules:legal_move(TempBoard2, white, FromRow-FromCol, ToRow-ToCol, 'K').

:- end_tests(king_moves).
