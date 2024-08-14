:- module(pgn_parser, [read_pgn/2]).
:- use_module(library(dcg/basics)).

%main functie die pgn omzal zetten naar een spel bestaande uit een aantal tags 
read_pgn(File, Game) :-
    open(File, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    (   phrase(parse_pgn(Game), Codes)
    ->  true
    ;   write('Failed to parse the PGN file.'), nl, fail).

parse_pgn(game(Tags, Moves)) -->
    tags(Tags),
    moves(Moves).

% parsed elke tag
tags([Tag|Tags]) -->
    tag(Tag),
    tags(Tags).
tags([]) --> [].

% parse 1 tag
tag(tag(Key, Value)) -->
    "[", symbol(Key), " \"", string(ValueCodes), "\"]", blanks,
    {atom_codes(Value, ValueCodes)}.

% parse sleutel
symbol(Key) -->
    string_without(" \"", KeyCodes),
    { atom_codes(Key, KeyCodes) }.

% Parse moves
moves(Moves) -->
    move_sequence(Moves).

% parse een lijst van moves en negeer de zet nummers
move_sequence([Move|MoreMoves]) -->
    optional_move_number, 
    move_piece(Move),
    !,
    blanks,
    move_sequence(MoreMoves).
move_sequence([]) --> [].

optional_move_number -->
    integer(_),
    ".",
    blanks,
    !.
optional_move_number --> [].

%parsed SAN notatie
move_piece(Move) -->
    string_without(" \n{}[]", MoveCodes),
    { MoveCodes \= [],
      atom_codes(Move, MoveCodes)}.
