:- module(chess_bot, [
    choose_move/5
]).

:- use_module(board).
:- use_module(rules).
:- use_module(checks).

%mijn bord evaluatie functie, telt alle stukken op en kent waarde toe (positief wit negatief zwart), neutrale eval is 0. Verder ook nog positiebonus voor middencontrole
evaluate_board(Board, Color, Value, Variant) :-
    findall(Piece-Row-Col, (board:piece_at(Board, Row, Col, Piece), Piece \= '.'), Pieces),
    sum_values(Pieces, Color, MaterialValue),
    positional_value(Pieces, Color, PositionValue),
    king_of_the_hill_bonus(Board, Color, Variant, KOTHBonus),
    Value is MaterialValue + PositionValue + KOTHBonus.

king_of_the_hill_bonus(Board, Color, koth, Bonus) :-
    board:piece_at(Board, Row, Col, Piece),
    piece_color(Piece, Color),
    piece_type(Piece, king),
    (   (Row = 4; Row = 5), (Col = 4; Col = 5)
    ->  Bonus = 1000
    ;   Bonus = 0
    ).
king_of_the_hill_bonus(_, _, classic, 0).

sum_values([], _, 0).
sum_values([Piece-_-_|T], Color, Value) :-
    piece_value(Piece, V),
    sum_values(T, Color, RestValue),
    adjust_sign(Piece, Color, V, AdjustedV),
    Value is AdjustedV + RestValue.

adjust_sign(Piece, Color, Value, AdjustedValue) :-
    piece_color(Piece, PieceColor),
    (   Color = PieceColor
    ->  AdjustedValue = Value
    ;   AdjustedValue is -Value).

%positiebonus, bonuspunten voor stukken op de vier middenste vakken
positional_value([], _, 0).
positional_value([Piece-Row-Col|T], Color, PositionValue) :-
    positional_bonus(Piece, Row, Col,Color, Bonus),
    positional_value(T, Color, RestBonus),
    PositionValue is Bonus + RestBonus.

positional_bonus(_, Row, Col, Color, Bonus) :-
    (Row == 4; Row == 5), (Col == 4; Col == 5),
    !,
    BonusValue = 0.5,
    (   Color = white
    ->  Bonus = -BonusValue
    ;   Bonus = BonusValue
    );
    Bonus = 0.

piece_value('P', 1).
piece_value('N', 3).
piece_value('B', 3).
piece_value('R', 5).
piece_value('Q', 9).
piece_value('K', 0).
piece_value('p', 1).
piece_value('n', 3).
piece_value('b', 3).
piece_value('r', 5).
piece_value('q', 9).
piece_value('k', 0).


%minimax algoritme met alpha beta snoeien voor extra optimalisatie (zie verslag) 
minimax(Board, Variant, Color, 0, _, _, _, Value, _) :-
    !,
    evaluate_board(Board, Color, Value, Variant).

minimax(Board, Variant, Color, Depth, Alpha, Beta, BestMove, Value, History) :-
    Depth > 0,
    checks:generate_all_moves(Board, Color, History, AllMoves),
    (   AllMoves = []
    ->  handle_no_moves(Board, Color, Value), %schieten geen geldige zetten meer over
        BestMove = none
    ;   (   Color = white
        ->  max_value(Board, Variant, AllMoves, Alpha, Beta, Color, Depth, BestMove, Value, History)
        ;   min_value(Board, Variant, AllMoves, Alpha, Beta, Color, Depth, BestMove, Value, History)
        )
    ).

%situatie waar geen geldige zetten zijn, schaakmat is dood dus heeft oneindig negatieve waarde
handle_no_moves(Board, Color, Value) :-
    checks:is_in_check(Board, Color),
    (   checks:is_in_check(Board, Color)
    ->  best_score(Color,Value) %schaakmat is ultieme scenario voor ons
    ;   Value = 0
    ).

best_score(white,-1000).
best_score(black,1000).

max_value(_, _,[], Alpha, _, _, _, _, Value, _) :- 
    Value = Alpha.

max_value(Board, Variant, [Move|Moves], Alpha, Beta, Color, Depth, BestMove, Value, History) :-
    Move = [From, To, Promotion],
    ( Promotion = none ->
        board:apply_move(Board, Color, From, To, History, NewHistory, NewBoard)
    ;
        board:apply_move(Board, Color, From, To, Promotion, History, NewHistory, NewBoard)
    ),
    NewDepth is Depth - 1,
    opponent(Color, OpponentColor),
    minimax(NewBoard, Variant, OpponentColor, NewDepth, Alpha, Beta, _, Eval, NewHistory),
    (   Eval >= Beta
    ->  Value = Eval, BestMove = Move
    ;   NewAlpha is max(Alpha, Eval),
        max_value(Board,Variant, Moves, NewAlpha, Beta, Color, Depth, Move1, Val1, History),
        (   Val1 > Eval
        ->  Value = Val1, BestMove = Move1
        ;   Value = Eval, BestMove = Move
        )
    ).

min_value(_, _,[], _, Beta, _, _, _, Value, _) :-
    Value = Beta.

min_value(Board, Variant,[Move|Moves], Alpha, Beta, Color, Depth, BestMove, Value, History) :-
    Move = [From, To, Promotion],
    ( Promotion = none ->
        board:apply_move(Board, Color, From, To, History,NewHistory, NewBoard)
    ;   
        board:apply_move(Board, Color, From, To, Promotion, History, NewHistory, NewBoard)
    ),
    NewDepth is Depth - 1,
    opponent(Color, OpponentColor),
    minimax(NewBoard, Variant, OpponentColor, NewDepth, Alpha, Beta, _, Eval, NewHistory),
    (   Eval =< Alpha
    ->  Value = Eval, BestMove = Move
    ;   NewBeta is min(Beta, Eval),
        min_value(Board, Variant, Moves, Alpha, NewBeta, Color, Depth, Move1, Val1, History),
        (   Val1 < Eval
        ->  Value = Val1, BestMove = Move1
        ;   Value = Eval, BestMove = Move
        )
    ).

opponent(white, black).
opponent(black, white).

choose_move(Board, Color, History, Variant, Move) :-
    Depth is 3,
    minimax(Board,Variant, Color, Depth, -1000, 1000, [From,To,Promotion], _, History),
    Move = [From,To,Promotion].

