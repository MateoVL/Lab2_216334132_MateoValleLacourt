:- module(tdagame_21633413_vallelacourt, [get_board/2, game_with_history/6, jugada_P1/3, jugada_P2/3, comprobacion_P1/5, comprobacion_P2/5]).
:- use_module("TDAboard_21633413_ValleLacourt").
:- use_module("TDAplayer_21633413_ValleLacourt").
:- use_module("main_21633413_ValleLacourt").


%Constructores

/*
Descripcion: Predicado que construye un juego con el historial
actualizado.
Dominio: Game(game) X Columna(int) X Pieza(piece) X NewGame(game)
MP: game_with_history/4
MS:
*/
game_with_history(Player1, Player2, Board, Curent_Turn, Historial,
                  [Player1, Player2, Board, Curent_Turn, Historial]).




%Selectores

/*
Descripcion: Predicado que entrega el tablero de un juego.
Dominio: Game(game) X Board(board)
MP: game_get_board/2
MS:
*/
get_board([_, _, Board|_], Board).


jugada_P1([P1, P2, Board, _, History], Column, JuegoJugado):-
    get_piece_player(P1, Piece),
    play_piece(Board, Column, Piece, Board_jugado),
    actualizar_Fichas(P1, NewP1),
    board_with_players(Board_jugado, NewP1, P2, NewBoard),
    game_with_history(NewP1, P2, NewBoard, 2, [[Column, Piece]|History], JuegoJugado).

jugada_P2([P1, P2, Board, _, History], Column, JuegoJugado):-
    get_piece_player(P2, Piece),
    play_piece(Board, Column, Piece, Board_jugado),
    actualizar_Fichas(P2, NewP2),
    board_with_players(Board_jugado, P1, NewP2, NewBoard),
    game_with_history(P1, NewP2, NewBoard, 1, [[Column, Piece]|History], JuegoJugado).


comprobacion_P1(P1, Player, Board, Column, Current_Turn):-
    Current_Turn =:= 1,
    get_id_player(P1, IdP1),
    get_id_player(Player, IdPlayer),
    IdP1 =:= IdPlayer,
    can_play(Board),
    Column > -1,
    Column < 7.

comprobacion_P2(P2, Player, Board, Column, Current_Turn):-
    Current_Turn =:= 2,
    get_id_player(P2, IdP2),
    get_id_player(Player, IdPlayer),
    IdP2 =:= IdPlayer,
    can_play(Board),
    Column > -1,
    Column < 7.

