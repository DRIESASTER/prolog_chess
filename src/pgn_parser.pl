% Module declaration and import
:- module(pgn_parser, [read_pgn/2]).
:- use_module(library(dcg/basics)).

% Main function to read a PGN file
read_pgn(File, Game) :-
    open(File, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    (   phrase(parse_pgn(Game), Codes)
    ->  true
    ;   write('Failed to parse the PGN file.'), nl, fail).

% PGN file parser
parse_pgn(game(Tags, Moves)) -->
    tags(Tags),
    moves(Moves).

% Parse tags
tags([Tag|Tags]) -->
    tag(Tag),
    !, % Cut to avoid backtracking
    tags(Tags).
tags([]) --> [].

% Parse a single tag
tag(tag(Key, Value)) -->
    "[", symbol(Key), " \"", string(ValueCodes), "\"]", blanks,
    {atom_codes(Value, ValueCodes)}.

% Parse a symbol (like "White" or "Black")
symbol(Key) -->
    string_without(" \"", KeyCodes),
    { atom_codes(Key, KeyCodes) }.

% Parse moves
% Parse moves
moves(Moves) -->
    move_sequence(Moves).

% Parse a sequence of moves, ignoring move numbers
move_sequence([Move|MoreMoves]) -->
    optional_move_number, % Skip move number
    move_piece(Move),
    !, % Commit to the move once parsed
    blanks, % Handle any spacing
    move_sequence(MoreMoves).
move_sequence([]) --> [].

% Optional move number (e.g., "1.", "2.", etc.)
optional_move_number -->
    integer(_),
    ".",
    blanks,
    !. % Use cut to avoid backtracking once a number is found
optional_move_number --> [].

% Parse move piece (Chess SAN notation)
move_piece(Move) -->
    string_without(" \n{}[]", MoveCodes),
    { MoveCodes \= [], % Ensure it's not just an empty string
      atom_codes(Move, MoveCodes)}.
