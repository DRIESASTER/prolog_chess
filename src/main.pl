:- module(main, [start_game/0]).
:- use_module(pgn_parser).
:- use_module(menu).
:- use_module(game_handler).

% Entry point for the application
main :-
    current_prolog_flag(argv, Args),
    handle_args(Args).

% Handle command line arguments to decide on the game start mode
handle_args([Filename, 'TEST']) :- !,
    set_filename(Filename),
    read_pgn(Filename, Game),
    test_game(Game),
    halt.

handle_args([Filename, 'GAME']) :- !,
    set_filename(Filename),
    read_pgn(Filename, Game),
    play_loaded_game(Game),
    halt.

handle_args(['GAME']) :- !,
    start_game.

handle_args([Filename | _]) :-
    set_filename(Filename),
    read_pgn(Filename, Game),
    process_game(Game),
    halt.

handle_args([]) :-
    write('No PGN file provided. Starting a standard game.'), nl,
    start_game.


%start een spel als er geen argumenten meegegevene zijn (interactieve modus 2e zit)
start_game :-
    play_default_game.

% start het spel als opgeroepen vanuit swipl [main].
game(game(Tags, Moves), Tags, Moves).
:- initialization(main).