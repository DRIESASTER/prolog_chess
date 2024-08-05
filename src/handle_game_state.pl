% handle_game_state.pl
:- module(handle_game_state, [
    handle_game_state/3
]).

:- use_module(board).
:- use_module(rules).
:- use_module(checks).

% Handle different game states
handle_game_state(checkmate, _, Player) :-
    format('~w is in checkmate. Game over.\n', [Player]), !.
handle_game_state(check, Board, Player) :-
    format('~w is in check. Please move carefully.\n', [Player]),
    prompt_and_move(Board, Player).
handle_game_state(stalemate, _, _) :-
    write('Stalemate. Game over.\n'), !.
handle_game_state(draw, _, _) :-
    write('The game is a draw. Game over.\n'), !.
handle_game_state(normal, Board, Player) :-
    prompt_and_move(Board, Player).

% Prompt user for a move and process it
prompt_and_move(Board, Player) :-
    repeat,
    format('~w to move (e.g., e2 e4): ', [Player]),
    read_line_to_string(user_input, MoveStr),
    (   parse_and_execute_move(Board, Player, MoveStr, NewBoard)
    ->  true; 
        write('Invalid move, please try again.\n'), fail),
    switch_player(Player, NextPlayer),
    play_game(NewBoard, NextPlayer).

% Parse the move string and execute it
parse_and_execute_move(Board, Player, MoveStr, NewBoard) :-
    split_string(MoveStr, " ", "", [FromStr, ToStr]),
    san_to_coords(FromStr, From),
    san_to_coords(ToStr, To),
    convert_move(From, To, FromRow-FromCol, ToRow-ToCol),
    move(Board, Player, FromRow-FromCol, ToRow-ToCol, NewBoard).

% Execute the move if legal and apply it to the board
move(Board, Player, FromCoords, ToCoords, NewBoard) :-
    rules:legal_move(Board, Player, FromCoords, ToCoords, Piece),
    execute_move(Board, FromCoords, ToCoords, Piece, Player, NewBoard).

% Switch player helper
switch_player(white, black).
switch_player(black, white).

% Execute the move on the board
execute_move(Board, FromCoords, ToCoords, Piece, Player, NewBoard) :-
    % Details on how to update the board state go here.
    % This could involve capturing a piece, promoting a pawn, etc.
    true.

% Add utility functions as necessary, e.g., san_to_coords, convert_move, etc.
