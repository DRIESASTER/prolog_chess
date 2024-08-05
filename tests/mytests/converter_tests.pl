:- module(converter_tests, [run_tests/0]).
:- use_module(library(plunit)).
:- use_module('../../src/board').
:- use_module('../../src/converter_to_san').
:- use_module('../../src/converter').


:- begin_tests(converter_to_san).

test(en_passant) :-
    initial_board(Board),
    apply_move(Board, white, 2-5, 4-5, [], H1, TempBoard1),
    apply_move(TempBoard1, black, 7-3, 5-3, H1, H2, TempBoard2),
    apply_move(TempBoard2, white, 4-5, 5-5, H2, H3, TempBoard3),
    apply_move(TempBoard3, black, 7-4, 5-4, H3, H4, TempBoard4),
    print_board(TempBoard4),nl,
    write(H4),
    convert_to_san(TempBoard4, white, [5, 5], [6, 4],none, H4, SAN),
    write(SAN),nl,
    assertion(SAN == 'exd6').

test(castling_kingside) :-
    initial_board(Board),
    apply_move(Board, white, 1-2, 3-1, [], H1, TempBoard1),
    apply_move(TempBoard1, black, 8-7, 6-6, H1, H2, TempBoard2),
    apply_move(TempBoard2, white, 2-2, 3-2, H2, H3, TempBoard3),
    apply_move(TempBoard3, black, 7-1, 6-1, H3, H4, TempBoard4),
    apply_move(TempBoard4, white, 2-3, 4-3, H4, H5, TempBoard5),
    apply_move(TempBoard5, black, 7-3, 5-3, H5, H6, TempBoard6),
    apply_move(TempBoard6, white, 1-3, 2-2, H6, H7, TempBoard7),
    apply_move(TempBoard7, black, 7-7, 5-7, H7, H8, TempBoard8),
    apply_move(TempBoard8, white, 1-4, 2-3,H8, H9, TempBoard9),
    apply_move(TempBoard9, black, 7-8, 6-8, H9, H10, TempBoard10),
    convert_to_san(TempBoard10, white, [1, 5], [1, 3], none, H10, SAN),
    assertion(SAN == 'O-O-O').

test(promotion_with_checkmate) :-
    Board = [
        ['R', 'N', 'B', 'Q', 'K', '.', '.', '.'],
        ['P', 'P', 'P', 'P', 'P', 'P', '.', 'p'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['p', 'p', 'p', 'p', 'p', 'p', 'p', '.'],
        ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r']
    ],
    convert_to_san(Board, black, [2, 8], [1, 8], 'q', [], SAN),
    assertion(SAN == 'h1=Q#').

test(capture) :-
    initial_board(Board),
    apply_move(Board, white, 2-5, 4-5, [], H1, TempBoard1),
    apply_move(TempBoard1, black, 7-4, 5-4, H1, H2, TempBoard2),
    apply_move(TempBoard2, white, 1-2, 3-3, H2, H3, TempBoard3),
    apply_move(TempBoard3, black, 8-7, 6-6, H3, H4, TempBoard4),
    convert_to_san(TempBoard4, white, [4, 5], [5, 4], none, H4, SAN),
    write(SAN),nl,
    assertion(SAN == 'exd5').

test(disambiguating_moves) :-
    Board = [
        ['.', '.', '.', '.', 'K', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', 'N', '.', '.', '.', 'N', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', 'k', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p'],
        ['r', 'n', 'b', 'q', '.', 'b', 'n', 'r']
    ],
    convert_to_san(Board, white, [3, 3], [4, 5], none, [], SAN),
    assertion(SAN == 'Nce4').

test(check) :-
    Board = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', 'P', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', 'k', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.']
    ],
    convert_to_san(Board, white, [2, 3], [3, 3], none, [], SAN),
    assertion(SAN == 'c3+').

test(kingside_castling_white) :-
    convert_move('O-O', [], white, 1-5, 1-7, true, none, []).

test(kingside_castling_black) :-
    convert_move('O-O', [], black, 8-5, 8-7, true, none, []).

test(queenside_castling_white) :-
    convert_move('O-O-O', [], white, 1-5, 1-3, true, none, []).

test(queenside_castling_black) :-
    convert_move('O-O-O', [], black, 8-5, 8-3, true, none, []).

test(pawn_promotion) :-
    Board = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', 'P', '.', '.', '.', '.', 'P'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', 'k', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', 'P', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.']
    ],
    convert_move('e8=Q', Board, white, 7-5, 8-5, true, 'Q', []).

test(ambiguous_move) :-
    Board = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['R', '.', '.', '.', '.', '.', '.', 'R'],
        ['.', '.', '.', '.', '.', '.', '.', '.'],
        ['P', 'P', 'P', 'P', 'P', 'P', 'P', 'P'],
        ['.', 'N', 'B', 'Q', '.', 'B', 'N', '.']
    ],
    convert_move('Rad5', Board, white, 5-1, 5-4, _, _, []),
    assertion(\+ convert_move('Rad5', Board, white, 5-8, 5-4, _, _, [])).