:- module(game_handler, [
        test_game/1,
        process_game/1,
        switch_player/2,
        play_game/8,
        play_loaded_game/1,
        play_default_game/0
    ]).
%deze module behandeld het verloop van het spel
:- use_module(menu).
:- use_module(board).
:- use_module(formatter).
:- use_module(chess_bot).
:- use_module(converter_to_san).
:- use_module(converter).
:- use_module(utils).

play_default_game():-
    initialize_game_settings(Variant, _, PWhite, _, AI),
    initial_board(Board),
    play_game(Board, white, AI, Variant, PWhite, [], [], _).

play_loaded_game(game(Tags, Moves)) :-
    extract_settings(Tags, PWhite, PBlack, Variant, _),
    board:initial_board(InitialBoard),
    execute_moves(InitialBoard, Moves, FinalBoard, [], History, white, CurrentPlayer),
    (PWhite = 'ai' -> Color = black ; Color = white),
    switch_player(Color, AI),
    initialize_game_state(PWhite, PBlack),
    play_game(FinalBoard, CurrentPlayer, AI, Variant, PWhite, Moves, History, _).

% start main spel loop voor interactieve modus, print startbord doet move, loop...
play_game(Board, Player, AI, Variant, Name, Moves, History, FinalHistory) :-
    print_board(Board),
    checks:check_game_state(Board, Player, History, State),
    handle_game_state_interactive(State, Player, Moves, Variant),
    handle_move(Board, Player, AI, Variant,State, Name, Move, Moves, History, NewHistory, NewBoard),
    append(Moves, [Move], NewMoves),
    switch_player(Player, NextPlayer),
    play_game(NewBoard, NextPlayer, AI, Variant, Name, NewMoves, NewHistory, FinalHistory).

%game dat gestart word met het test commando (zal alle mogelijke moves outputten)
test_game(game(Tags, Moves)) :-
    extract_settings(Tags, _, _, Variant, _),
    board:initial_board(InitialBoard),
    execute_moves(InitialBoard, Moves, FinalBoard, [], History, white, LastPlayer),
    checks:check_game_state(FinalBoard, LastPlayer, History, State),
    handle_game_state(State,LastPlayer,Moves, Variant),
    checks:generate_all_moves(FinalBoard, LastPlayer,History, AllMoves),
    convert_all_moves_to_san(FinalBoard, LastPlayer, AllMoves, History,SANMoves),
    formatter:format_previous_moves(Moves,1,white, State, Str),
    print_possible_moves(SANMoves,Str).


% game waarbij geen TEST commando meegegeven word, zal best volgende move outputten
process_game(game(Tags,Moves)) :-
    extract_settings(Tags, _, _, Variant, _),
    board:initial_board(InitialBoard),
    execute_moves(InitialBoard, Moves, FinalBoard, [], History, white, CurrentPlayer),
    checks:check_game_state(FinalBoard, CurrentPlayer, History, State),
    handle_game_state(State, CurrentPlayer,Moves, Variant),
    choose_move(FinalBoard,CurrentPlayer, History, Variant, Move),
    Move = [From, To, Promotion],
    (Promotion = none ->
        apply_move(FinalBoard, CurrentPlayer, From, To, History, NewHistory, NewBoard)
    ;   apply_move(FinalBoard, CurrentPlayer, From, To, Promotion, History, NewHistory, NewBoard)
    ),
    switch_player(CurrentPlayer, Next),
    checks:check_game_state(NewBoard, Next, NewHistory, NewState),
    convert_all_moves_to_san(FinalBoard, CurrentPlayer, [Move], History, SANMoves),
    append(Moves, SANMoves, AllMoves),
    handle_game_state(NewState,CurrentPlayer, AllMoves, Variant),
    formatter:format_previous_moves(Moves,1,white,NewState,Str),
    print_possible_moves(SANMoves,Str).


handle_move(Board, AI, AI, Variant,_, _, Move, _, History, NewHistory, NewBoard) :- !,
    ai_move(Board, AI, Variant, Move, History, NewHistory, NewBoard).

handle_move(Board, Player, _, Variant,State, Name, Move, Moves, History, NewHistory, NewBoard) :-
    prompt_and_move(Board, Player, Variant,State, Name, Move, Moves, History, NewHistory, NewBoard).

% ai move beurt
ai_move(Board, Player, Variant, SAN, History, NewHistory, NewBoard) :-
    write('choosing move'),nl,
    chess_bot:choose_move(Board, Player, History, Variant, Move),
    Move = [From,To,Promotion],
    ( Promotion = none ->
        apply_move(Board, Player, From, To, History, NewHistory, NewBoard)
    ;   apply_move(Board, Player, From, To, Promotion, History, NewHistory, NewBoard)
    ),
    convert_all_moves_to_san(Board, Player, [Move], History, [SAN]).

% lees input prompt en voer move uit als geldig anders herhaal
prompt_and_move(Board, Player, Variant,State, _, SAN, Moves, History, NewHistory, NewBoard) :-
    repeat,
    format('~w to move: ', [Player]),
    read_user_input(MoveStr),
    handle_special_commands(MoveStr, Variant,State, Moves),
    process_move(MoveStr,SAN, Board, Player, History, NewHistory, NewBoard).

% user input verwerken als move
read_user_input(Move) :-
    read_line_to_codes(user_input, Codes),
    string_codes(Move, Codes).


% convert van SAN naar inwendig coord system en voert move uit ik converteer dubbel zodat SAN notatie zeker correct is ie als speler e6 speelt en dit schaakmat zou zijn word dit intern opgeslagen als e6# bvb
process_move(MoveStr,CorrectedMove, Board, Player, History, NewHistory, NewBoard):-
    convert_move(MoveStr, Board, Player, From, To, _,Promotion, History),
    convert_all_moves_to_san(Board,Player,[[From,To,Promotion]], History, [CorrectedMove]),
    ( Promotion = none ->
        board:apply_move(Board,Player, From, To, History, NewHistory, NewBoard)
    ;   board:apply_move(Board,Player, From, To, Promotion, History, NewHistory, NewBoard)
    ).


% simpele helper om mbv het convert_to_san module elke move naar SAN notatie om te zetten
convert_all_moves_to_san(Board, Player, Moves, History, SANMoves) :-
    findall(SAN, (member(Move, Moves), move_to_san(Board, Player, Move, History, SAN)), SANMoves).

% idem maar deel 2
move_to_san(Board, Player, [FromRow-FromCol, ToRow-ToCol,Promotion],History, SAN) :-
    converter_to_san:convert_to_san(Board, Player, [FromRow, FromCol], [ToRow, ToCol],Promotion, History, SAN).


%helper om een lijst aan SAN moves volledig uit te voeren en het gespeelde bord te retourneren
execute_moves(Board, Moves, FinalBoard,History, NewHistory, Player, LastPlayer) :-
    execute_moves_helper(Board, Moves, FinalBoard, History, NewHistory, Player, LastPlayer).

%base cases
execute_moves_helper(Board, ['*'], Board, History, History, Player, Player) :- !.
execute_moves_helper(Board, [], Board, History, History, Player, Player) :- !.

execute_moves_helper(Board, [MoveStr | RestMoves], FinalBoard,History, FinalHistory, Player, LastPlayer) :-
    convert_move(MoveStr, Board, Player, FromCoord, ToCoord, _, Promotion, History),
    ( Promotion = none ->
        apply_move(Board, Player, FromCoord, ToCoord, History, NewHistory, NewBoard)  % Apply move without promotion
    ;   apply_move(Board, Player, FromCoord, ToCoord, Promotion, History, NewHistory, NewBoard)  % Apply move with promotion
    ),
    switch_player(Player, NextPlayer),
    execute_moves_helper(NewBoard, RestMoves, FinalBoard, NewHistory, FinalHistory, NextPlayer, LastPlayer).

handle_game_state_interactive(checkmate, WinnerColor, Moves, Variant):- !,
    write('Game has ended due to checkmate!'),nl,
    player(WinnerColor, WinnerName),
    retract(score(WinnerName, _)),
    assert(score(WinnerName, 1)),
    format_previous_moves(Moves, 1, white,checkmate, MovesStr),
    write(MovesStr),
    update_and_display_scores(checkmate),
    read_user_input(Command),
    handle_special_commands(Command, Variant, checkmate, Moves).

handle_game_state_interactive(koth, WinnerColor, Moves, Variant):- !,
    write('Game has ended due to king of the hill!'),nl,
    player(WinnerColor, WinnerName),
    score(WinnerName, CurrentScore),
    NewScore is CurrentScore + 1,
    retract(score(WinnerName, _)),
    assert(score(WinnerName, NewScore)),
    format_previous_moves(Moves, 1, white,checkmate, MovesStr),
    write(MovesStr),
    update_and_display_scores(checkmate),
    read_user_input(Command),
    handle_special_commands(Command, Variant, checkmate, Moves).


handle_game_state_interactive(stalemate, _, Moves, Variant):- !,
    write('Game has ended due to stalemate!'), nl,
    retract(score(_,_)),
    player(white,Pw),
    player(black,Pb),
    assert(Pw,0.5),
    assert(Pb,0.5),
    format_previous_moves(Moves, 1, white, stalemate, MovesStr),
    write(MovesStr),
    update_and_display_scores(stalemate),
    read_user_input(Command),
    handle_special_commands(Command, Variant, stalemate, Moves).

handle_game_state_interactive(_,_,_,_).


% niks te doen
handle_game_state(normal, _, _, _).
handle_game_state(check, _, _, _).

% spel afronden
handle_game_state(checkmate, WinnerColor, Moves, _) :-
    player(WinnerColor, WinnerName),
    retract(score(WinnerName, _)),
    assert(score(WinnerName, 1)),
    format_previous_moves(Moves, 1, white,checkmate, MovesStr),
    write(MovesStr),
    update_and_display_scores(checkmate),
    halt.

%zelfde als schaakmat
handle_game_state(koth, WinnerColor, Moves, koth):-
    write('Game has ended due to king of the hill!'),nl,
    player(WinnerColor, WinnerName),
    retract(score(WinnerName, _)),
    assert(score(WinnerName, 1)),
    format_previous_moves(Moves, 1, white,checkmate, MovesStr),
    write(MovesStr),
    update_and_display_scores(checkmate),
    halt.