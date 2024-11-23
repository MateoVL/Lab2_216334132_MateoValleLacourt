:- module(tdagame_21633413_vallelacourt, [game_with_history/6, jugada_P1/3, jugada_P2/3, comprobacion_P1/5, comprobacion_P2/5]).
:- use_module("TDAboard_21633413_ValleLacourt").
:- use_module("TDAplayer_21633413_ValleLacourt").
:- use_module("TDApiece_21633413_ValleLacourt").
:- use_module("main_21633413_ValleLacourt").

% TDA game: juego representado por un a lista que contiene los
% jugadores, un tablero, el turno actual del juego y un historial
% game(Player1, Player2, Board, Current_turn, Historial).

%Constructores

/*
Descripcion: Predicado que construye un juego con el historial
actualizado.
Dominio: Game(game) X Columna(int) X Pieza(piece) X NewGame(game)
MP: game_with_history/6
MS:
*/
game_with_history(Player1, Player2, Board, Curent_Turn, Historial,
                  [Player1, Player2, Board, Curent_Turn, Historial]).



%Modificadores

/*
Descripcion: Predicado que realiza la jugada del jugador 1.
Dominio: Player(player) X Column(int) X JuegoJugado(game)
MP: jugada_P1/3
MS: get_piece_player/2,
    get_color/2,
    play_piece/4,
    actualizar_Fichas/2,
    board_with_players/4,
    game_with_history/6.

*/
jugada_P1([P1, P2, Board, _, History], Column, JuegoJugado):-
    get_piece_player(P1, Piece),
    get_color(Piece, Color),
    play_piece(Board, Column, Piece, Board_jugado),
    actualizar_Fichas(P1, NewP1),
    board_with_players(Board_jugado, NewP1, P2, NewBoard),
    game_with_history(NewP1, P2, NewBoard, 2, [[Column, Color]|History], JuegoJugado).


/*
Descripcion: Predicado que realiza la jugada del jugador 2.
Dominio: Player(player) X Column(int) X JuegoJugado(game)
MP: jugada_P2/3
MS: get_piece_player/2,
    get_color/2,
    play_piece/4,
    actualizar_Fichas/2,
    board_with_players/4,
    game_with_history/6.

*/
jugada_P2([P1, P2, Board, _, History], Column, JuegoJugado):-
    get_piece_player(P2, Piece),
    get_color(Piece, Color),
    play_piece(Board, Column, Piece, Board_jugado),
    actualizar_Fichas(P2, NewP2),
    board_with_players(Board_jugado, P1, NewP2, NewBoard),
    game_with_history(P1, NewP2, NewBoard, 1, [[Column, Color]|History], JuegoJugado).



%Otros

/*
Descripcion: Predicado que comprueba si la jugada sera de jugador 1 y si
esta es posible.
Dominio: P1(player) X Player(player) X Board(board) X Column(int) X
Current_Turn(int)
MP: comprobacion_P1/5
MS: get_id_player/2,
    can_play/1

*/
comprobacion_P1(P1, Player, Board, Column, Current_Turn):-
    Current_Turn =:= 1,
    get_id_player(P1, IdP1),
    get_id_player(Player, IdPlayer),
    IdP1 =:= IdPlayer,
    can_play(Board),
    Column > -1,
    Column < 7.

/*
Descripcion: Predicado que comprueba si la jugada sera de jugador 2 y si
esta es posible.
Dominio: P2(player) X Player(player) X Board(board) X Column(int) X
Current_Turn(int)
MP: comprobacion_2/5
MS: get_id_player/2,
    can_play/1

*/
comprobacion_P2(P2, Player, Board, Column, Current_Turn):-
    Current_Turn =:= 2,
    get_id_player(P2, IdP2),
    get_id_player(Player, IdPlayer),
    IdP2 =:= IdPlayer,
    can_play(Board),
    Column > -1,
    Column < 7.

