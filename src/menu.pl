:- module(menu, [
    display_start_menu/3,
    initialize_game_settings/5,
    extract_settings/5,
    set_defaults/0,
    initialize_game_state/2,
    player/2,
    score/2,
    update_and_display_scores/1,
    handle_special_commands/4,
    set_filename/1
    ]).
%deze module heeft onder andere code voor het menu van de interactieve modus, het extracten van tags etc...
:- use_module(board).
:- use_module(formatter).

:- dynamic player/2.   % player(Color, Name)
:- dynamic score/2.    % score(Name, Points)
:- dynamic filename/1.

set_filename(Filename):-
    retractall(filename(_)),
    assert(filename(Filename)).

%simpel start menutje voor opties te selecteren
display_start_menu(Variant, Color, Name) :-
    (var(Variant) -> write('Variant: Classic (default) or King of the Hill'), nl; true),
    (var(Color) -> write('Color: White (default) or Black'), nl; true),
    (var(Name) -> write('Enter your name (default "Player")'), nl; true),
    write('Please select your options:'), nl.

% kies variant van het spel
choose_variant(Variant) :-
    write('Variant: Classic (default) or King of the Hill'), nl,
    read_line_to_string(user_input, VariantInput),
    (VariantInput = "2" -> Variant = koth; Variant = classic).

% kies spelerkleur
choose_color(Color) :-
    write('Color: White (default) or Black'), nl,
    read_line_to_string(user_input, ColorInput),
    (ColorInput = "2" -> Color = black; Color = white).

% kies spelernaam
choose_name(Name) :-
    write('Enter your name (default "Player")'), nl,
    read_line_to_string(user_input, NameInput),
    (NameInput = "" -> Name = "Player"; Name = NameInput).

% initaliseerd een aantal basis game instellingen
initialize_game_settings(Variant, Color, Name, Player, AI) :-
    (var(Variant) -> choose_variant(Variant); true),
    (var(Color) -> choose_color(Color); true),
    (var(Name) -> choose_name(Name); true),
    determine_players(Color, Player, AI),
    (Player = white -> initialize_game_state(Name, 'ai') ; initialize_game_state('ai', Name)).

%spelerkleur is omgekeerd van AI kleur basically
determine_players(Color, Player, AI) :-
    (Color = white -> Player = white, AI = black; Player = black, AI = white).
    
%haalt elke relevante settings uit de tags van het pgn bestand
extract_settings([], 'Player', none, classic, '*') :- !,
    set_defaults.

extract_settings(Tags, PlayerWhite, PlayerBlack, Variant, Result) :-
    ( member(tag('White', PlayerWhiteTemp), Tags) -> PlayerWhite = PlayerWhiteTemp; PlayerWhite = 'Player' ),
    ( member(tag('Black', PlayerBlackTemp), Tags) -> PlayerBlack = PlayerBlackTemp; PlayerBlack = 'ai' ),
    ( member(tag('Rules', VariantCode), Tags) -> Variant = VariantCode; Variant = classic ),
    ( member(tag('Result', ResultTemp), Tags) -> Result = ResultTemp; Result = '*' ),
    initialize_game_state(PlayerWhite, PlayerBlack).


%zal  defaultwaarden ingegeven als er geen pgn tags meegeleverd zijn
set_defaults :-
    initialize_game_state('Player', 'ai').

% initaliseerd de spel status op basis van meegegeven tags en zal een aantal waarden als dynamische prediakten opslagen (handing want niet altijd meegeven en veranderen zeer weinig tot nooit in dit geval)
initialize_game_state(PlayerWhite, PlayerBlack) :-
    retractall(player(_, _)),
    retractall(score(_, _)),
    assert(player(white, PlayerWhite)),
    assert(player(black, PlayerBlack)),
    assert(score(PlayerWhite, 0)),
    assert(score(PlayerBlack, 0)).


% update de scores van het spel en print uit
update_and_display_scores(_) :-
    player(white, PlayerWhite),
    player(black, PlayerBlack),
    score(PlayerWhite, ScoreWhite),
    score(PlayerBlack, ScoreBlack),
    format('~w-~w~n', [ScoreWhite, ScoreBlack]).

% speciale commandos regelen
handle_special_commands("save", Variant, State, Moves) :-!,
    format_previous_moves(Moves, 1, white, stalemate, MovesStr),
    get_filename(F),
    save_game(MovesStr, Variant, State, F),
    halt.

handle_special_commands("quit", _, _, _) :-!,
    write('Quitting game without saving...'), nl,
    abort.

handle_special_commands("resign", _, _, _) :-!,
    write('Quitting game without saving...'), nl,
    abort.

handle_special_commands(_, _, _, _).

get_filename(Name):-
    filename(Name),!.

get_filename(Name) :-
    write('Please enter the filename you want to save to: '),
    read_line_to_codes(user_input, Codes),
    string_codes(InputName, Codes),
    split_string(InputName, ".", "", [Base|_]),
    string_concat(Base, ".pgn", Name),nl,
    write('Saving game to: '),
    write(Name).


save_game(MovesStr, Variant,State, Filename) :-
    player(white, PW),
    player(black, PB),
    open(Filename, write, Stream),

    format(Stream, '[White "~w"]\n', [PW]),
    format(Stream, '[Black "~w"]\n', [PB]),
    format(Stream, '[Rules "~w"]\n', [Variant]),
    write_result(Stream, State),
    write(Stream, MovesStr),
    close(Stream).


write_result(Stream, checkmate):-!,
    format(Stream, '[Result "~w"]\n', ["0-1"]).

write_result(Stream, stalemate):-!,
    format(Stream, '[Result "~w"]\n', ["1/2-1/2"]).

write_result(Stream, _):-!,
    format(Stream, '[Result "~w"]\n', ["*"]).