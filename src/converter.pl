% Module declaration
:- module(converter, [
    convert_move/8,
    convert_to_correct_color/3
    ]).
%module om SAN moves naar mijn intern coordinaten systeem te vertalen

:- use_module(rules).
:- use_module(board).
:- use_module(utils).

% castling checks
convert_move('O-O', _, Color, FromCoord, ToCoord, true,none,_) :-!,
    % Determine coordinates based on player color for kingside castling
    (   Color = white -> FromCoord = 1-5, ToCoord = 1-7
    ;   Color = black -> FromCoord = 8-5, ToCoord = 8-7
    ).

%queenside
convert_move('O-O-O', _, Color, FromCoord, ToCoord, true,none,_) :-
    (   Color = white -> FromCoord = 1-5, ToCoord = 1-3
    ;   Color = black -> FromCoord = 8-5, ToCoord = 8-3
    ),!.

% anders normaal converten op basis van file en rank (en promotie)
convert_move(MoveStr, Board, Player, FromCoord, ToCoord, IsLegal,Promotion, History) :-
    parse_san(MoveStr, Piece, Player, Dest, Disambiguator, Promotion),
    translate_coord(Dest, ToCoord),
    generate_moves(Board, Player, Piece, Disambiguator, ToCoord, MovesList,Promotion, History),
    select_move(MovesList, FromCoord, IsLegal).


% alle moves genereren die op die 'to' coordinaat kan komen want intern coordinaten systeem heeft from coordinaten nodig
generate_moves(Board, Player, PieceType, Disambiguator, ToRow-ToCol, MovesList, none, History) :-!,
    findall(FromRow-FromCol,
        (
            piece_at(Board, FromRow, FromCol, Piece),
            rules:piece_type(Piece, PieceType),
            rules:piece_color(Piece, Player),
            matches_disambiguator(FromRow-FromCol, Disambiguator),
            rules:legal_move(Board, Player, FromRow-FromCol, ToRow-ToCol, History)
        ),
        MovesList).

%idem maar met promoties bij
generate_moves(Board, Player, PieceType, Disambiguator, ToRow-ToCol, MovesList, Promotion, History) :-
    findall(FromRow-FromCol,
        (
            piece_at(Board, FromRow, FromCol, Piece),
            rules:piece_type(Piece, PieceType),
            rules:piece_color(Piece, Player),
            matches_disambiguator(FromRow-FromCol, Disambiguator),
            rules:legal_move(Board, Player, FromRow-FromCol, ToRow-ToCol, Promotion, History)
        ),
        MovesList).

%basis parsing van SAN movestring
parse_san(MoveStr, Piece, Color, Dest, Disambiguator, Promotion) :-
    filter_special_chars(MoveStr, FilteredMoveStr),
    string_chars(FilteredMoveStr, Chars),
    (   extract_piece(Chars, PieceChar, RestChars)
    ->  identify_piece(PieceChar, Piece),
        extract_promotion(RestChars, Promotion, Color, CleanStr),
        extract_destination_disambiguator(CleanStr, Disambiguator, DestChars),
        identify_destination(DestChars, Dest)
    ;   Piece = pawn, % als der geen stuk char gevonden is is het sws een pion
        extract_promotion(Chars, Promotion, Color, CleanStr),
        extract_destination_disambiguator(CleanStr, Disambiguator, DestChars),
        identify_destination(DestChars, Dest)
    ).

% als promo char aanwezig is zal hij hier extracten anders none
extract_promotion(MoveStr, Promotion,Color, CleanChars) :-
    string_chars(MoveStr, Chars),
    ( extract_promotion_char(Chars, PromotionChar, CleanChars) ->
        convert_to_correct_color(PromotionChar,Color,C),
        Promotion = C  %
    ;   Promotion = none,
    CleanChars = Chars).

%helper voor promo char (promo char staat altijd achter '=')
extract_promotion_char(Chars, PromotionChar, CleanChars) :-
    append(Pre, ['='|Post], Chars),
    Post = [PromotionChar|Rest],
    append(Pre, Rest, CleanChars).

%ik bepaal zelf de speciale betekenissen (schaak, capture, ...) en filter chars uit SAN gewoon weg dus
filter_special_chars(Str, FilteredStr) :-
    string_chars(Str, Chars),
    exclude(special_char, Chars, FilteredChars),
    string_chars(FilteredStr, FilteredChars).

special_char(Char) :-
    member(Char, ['+', 'x', '#']).


extract_disambiguator(Chars, Disamb, Rest) :-
    partition(is_disamb_char, Chars, Disamb, Rest).

is_disamb_char(Char) :-
    char_type(Char, alpha); char_type(Char, digit).


%spreekt voor zich, zoekt piece char waar aanwezig
extract_piece([P|Rest], P, Rest) :-
    member(P, ['K', 'Q', 'R', 'B', 'N']), 
    !. 
extract_piece(Chars, [], Chars). 

identify_piece([], pawn):- !.
identify_piece(Char, Piece) :-
    !, 
    rules:piece_type(Char, Piece).


identify_destination([RowChar, ColChar], Dest) :-
    atom_chars(Dest, [RowChar, ColChar]).

%als er meerdere moves mogelijk zijn met die SAN notatie (ambigous) of er geen legale move is zal hij dit laten weten
select_move([Move], From, true) :- !,
    From = Move.
select_move([], _, false) :-
    write('Illegal move, no valid moves found.'), nl.
select_move([_|_], _, false) :-
    write('Ambiguous move, please specify more details.'), nl.

%vertaald SAN coord (a1 bvb naar 1-1) naar inwendig coord systeem 
translate_coord(Dest, Coord) :-
    atom_chars(Dest, [FileChar, RankChar]),
    file_to_index(RankChar, File),
    char_code('1', Base),
    char_code(FileChar, RankCode),
    Rank is RankCode - Base + 1,
    Coord = (Rank-File).

file_to_index(FileChar, Index) :-
    char_code('a', Base),
    char_code(FileChar, Code),
    Index is Code - Base + 1.


%check of file de meegegeven disambiguator matched zoja correcte move
matches_disambiguator(_-FromCol, file(FileChar)) :-
    file_to_index(FileChar, FromCol).

matches_disambiguator(_-FromCol, rank(RankChar)) :-
    char_code('1', Base),
    char_code(RankChar, RankCode),
    FromCol is RankCode - Base + 1.
matches_disambiguator(_, none).

%reverse lijst want makkelijker om gewone move a1 weg te filteren zonder alles te overlopen meerder keren
extract_destination_disambiguator(Chars, Disambiguator, Dest) :-
    reverse(Chars, RevChars),
    [RankChar, FileChar | RevRest] = RevChars,
    reverse([FileChar, RankChar], Dest),
    (   RevRest = []
    ->  Disambiguator = none 
    ;   identify_disambiguator(RevRest, Disambiguator)
    ).

%test of file of rank is op basis van letter of getal
identify_disambiguator([D], file(D)) :-
    char_type(D, alpha), !.

identify_disambiguator([D], rank(D)) :-
    char_type(D, digit), !.

identify_disambiguator([F, R], file(F)) :-
    char_type(F, alpha),
    char_type(R, digit), !.

identify_disambiguator([R, F], rank(R)) :-
    char_type(F, alpha),
    char_type(R, digit), !.

%geen specificatie aanwezig
identify_disambiguator([], '').

% test voor legale specificatie
identify_disambiguator([Char], file(Char)) :- 
    member(Char, ['a', 'b', 'c', 'd', 'e', 'f', 'g']), !.

identify_disambiguator([Char], rank(Char)) :- 
    member(Char, ['1', '2', '3', '4', '5', '6', '7', '8']), !.

identify_disambiguator([], _).
