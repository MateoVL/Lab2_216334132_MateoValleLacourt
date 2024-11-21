:- module(main_21633413_vallelacourt, [player/8, piece/2, board/1, can_play/1, play_piece/4, check_vertical_win/2, check_horizontal_win/2, check_diagonal_win/2, who_is_winner/2, game/5, game_history/2, is_draw/1, update_stats/3, get_current_player/2, game_get_board/2, end_game/2, player_play/4]).
:- use_module("TDAplayer_21633413_ValleLacourt").
:- use_module("TDAboard_21633413_ValleLacourt").
:- use_module("TDAgame_21633413_ValleLacourt").


/*
RF02 TDA Player - constructor
Descripcion: Predicado que permite crear un jugador.
Dominio: id (int) X name (string) X color (string) X wins (int) X losses
(int) X draws (int) X remaining_pieces (int) X Player
MP: player/8.
*/
player(Id, Name, Color, Wins, Losses, Draws, Remaining_pieces,
       [Id, Name, Color, Wins, Losses, Draws, Remaining_pieces]).


/*
RF03 TDA Piece - constructor
Descripcion: Predicado que permite crear una ficha de conecta4.
Dominio: color (string)
MP: piece/2.
*/
piece(Color, [Color]).


/*
RF04 TDA Board - constructor
Descripcion: Crear un tablero de conecta4.
Dominio: sin parametros de entrada.
MP: board/1.
*/
board([[[0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0]],
        [],
        []]).


/*
RF05 TDA Board - otros - sePuedeJugar?
Descripcion: Predicado que permite verificar si se puede realizar más jugadas en el tablero.
Dominio: Board (board)
MP: can_play/1.
MS: member/2.
*/
can_play([Board|_]):-
    get_fila(Board, 0, Fila0),
    member(0, Fila0),
    !.


/*
RF06 TDA Board - modificador - jugar ficha
Descripcion: Predicado que permite jugar una ficha en el tablero.
Dominio: Board (board) X Column (int) X Piece (piece) X NewBoard (board)
MP: play_piece/4.
MS: board_with_players/4
    get_columna/3,
    fila_baja/2,
    get_fila/3,
    set_pieza_fila/4,
    set_fila_board/4.
*/
play_piece([Tablero, P1, P2], Column, Piece, NewBoard):-
    get_columna(Tablero, Column, ListColumn),
    fila_baja(ListColumn, PosFila),
    get_fila(Tablero, PosFila, Fila),
    set_pieza_fila(Fila, Column, Piece, NewFila),
    set_fila_board(Tablero, PosFila, NewFila, NewTablero),
    board_with_players([NewTablero, _, _],P1, P2, NewBoard),
    !.


/*
RF07 TDA Board - otros - verificar victoria vertical
Descripcion: Predicado que permite verificar el estado actual del tablero y
entregar el posible ganador que cumple con la regla de conectar 4 fichas de
forma vertical.
Recursividad: Si.
Dominio: Board(board) X Winner(int)
MP: check_vertical_win/2.
MS: cuatro_seguidos/4,
    check_vertical_win/2,
    Winner is Acc + WinnerRec.
*/
check_vertical_win([[[], [], [], [], [], []], _, _], 0).
check_vertical_win([Tablero, P1, P2], Winner):-
    sacar_columna(Tablero, Columna0, RestoTablero),
    cuatro_seguidos_fila(Columna0, P1, P2, Acc),
    check_vertical_win([RestoTablero,P1,P2], WinnerRec),
    Winner is Acc + WinnerRec,
    !.


/*
RF08 TDA Board - otros - verificar victoria horizontal
Descripcion: Predicado que permite verificar el estado actual del tablero y
entregar el posible ganador que cumple con la regla de conectar 4 fichas de
forma horizontal.
Recursividad: Si.
Dominio: Board(board) X Winner(int)
MP: check_horizontal_win/2.
MS: cuatro_seguidos/4,
    check_hoprizontal_win/2,
    Winner is Acc + WinnerRec.
*/
check_horizontal_win([[], _, _], 0).
check_horizontal_win([[Car|Cdr], P1, P2], Winner):-
    cuatro_seguidos_fila(Car, P1, P2, Acc),
    check_horizontal_win([Cdr,P1,P2], WinnerRec),
    Winner is Acc + WinnerRec,
    !.

/*
RF09 TDA Board - otros - verificar victoria diagonal
Descripcion: Predicado que permite verificar el estado actual del tablero y
entregar el posible ganador que cumple con la regla de conectar 4 fichas de
forma diagonal.
Recursividad: Si.
Dominio: Board(board) X Winner(int)
MP: check_horizontal_win/2.
MS: cuatro_seguidos/4,
    check_hoprizontal_win/2,
    Winner is Acc + WinnerRec.
*/
check_diagonal_win([[_, _, _], _, _], 0).
check_diagonal_win([[Car|Cdr], P1, P2], Winner):-
    set_inv_filas([Car|Cdr], TableroInv),
    verificar_fila_diagonal(TableroInv, P1, P2, AccIzq),
    verificar_fila_diagonal([Car|Cdr], P1, P2, AccDer),
    verificacion_DerIzq(AccIzq, AccDer, Acc),
    check_diagonal_win([Cdr, P1, P2], WinnerRec),
    Winner is WinnerRec + Acc,
    !.


/*
RF10 TDA Board - otros - entregarGanador
Descripcion: Predicado que permite verificar el estado actual del tablero y
entregar el posible ganador que cumple con la regla de conectar 4 fichas de
forma vertical, horizontal o diagonal.
Dominio: Board(board) X Winner(int)
MP: check_horizontal_win/2.
MS: cuatro_seguidos/4,
    check_hoprizontal_win/2,
    Winner is Acc + WinnerRec.
*/
who_is_winner(Board, Winner):-
    check_vertical_win(Board, WinnerV),
    check_horizontal_win(Board, WinnerH),
    check_diagonal_win(Board, WinnerD),
    verificacion_tipo_victoria(WinnerV, WinnerH, WinnerD, Winner),
    !.


/*
RF11 TDA Game - constructor
Descripcion: Predicado que permite crear una nueva partida.
Dominio: Player1(player) X Player2(player) X Board(board) X
current_turn(int) X Game(game)
MP: game/5
MS: board_with_players/4
*/
game(Player1, Player2, Board, Current_turn, [Player1, Player2, NewBoard, Current_turn, []]):-
    board_with_players(Board, Player1, Player2, NewBoard).

/*
RF12 TDA Game - otros - history
Descripcion: Predicado que genera un historial de movimientos de la
partida.
Dominio: Game(game) X CurrentHistory(lista)
MP: game_history/2
MS:
*/
game_history([_, _, _, _, History], CurrentHistory):-
    reverse(History, CurrentHistory).


/*
RF13 TDA Game - otros - esEmpate?
Descripcion: Predicado que verifica si el estado actual del juego es empate.
Dominio: Game(game)
MP: is_draw/1
MS:
*/
is_draw([P1, P2, Board|_]):-
    \+ can_play(Board),
    !;
    get_remaining_pieces(P1, Pieces1),
    get_remaining_pieces(P2, Pieces2),
    Pieces1 =:= 0,
    Pieces2 =:= 0,
    !.


/*
RF14 TDA Player - otros - actualizarEstadisticas
Descripcion: Predicado que actualiza las estadísticas del jugador, ya sea victoria, derrotas o empates.
Dominio: Game(game) X OldStats X NewStats
MP: update_stats/3
MS: Resultado = "win",
    actualizarWin/2;
    Resultado = "loss",
    actualizarLoss/2;
    Resultado = "draw",
    actualizarDraw/2.
*/
update_stats(Player, Resultado, NewPlayer):-
    Resultado = "win",
    actualizarWin(Player, NewPlayer);
    Resultado = "loss",
    actualizarLoss(Player, NewPlayer);
    Resultado = "draw",
    actualizarDraw(Player, NewPlayer).


/*
RF15 TDA Game - selector - getCurrentPlayer
Descripcion: Predicado que obtiene el jugador cuyo turno está en curso.
Dominio: Game(game) X Player(player)
MP: get_current_player/2
MS: Current_Turn =:= 1,
    Player is P1;
    Current_Turn =:= 2,
    Player is P2.
*/
get_current_player([P1, P2, _, Current_Turn, _], Player):-
    Current_Turn =:= 1,
    Player = P1,
    !;
    Current_Turn =:= 2,
    Player = P2,
    !.


/*
RF16 TDA Game - selector - board_get_state
Descripcion: Predicado que entrega por pantalla el estado actual del tablero en el juego.
Dominio: Game(game) X Board(board)
MP: game_get_board/2
MS: write(F1), nl,
    write(F2), nl,
    write(F3), nl,
    write(F4), nl,
    write(F5), nl,
    write(F6), nl.
*/
game_get_board([_, _, [[F1, F2, F3, F4, F5, F6], P1, P2]|_], [[F1, F2, F3, F4, F5, F6], P1, P2]):-
    write(F1), nl,
    write(F2), nl,
    write(F3), nl,
    write(F4), nl,
    write(F5), nl,
    write(F6), nl.


/*
RF17 TDA Game - modificador - game-set-end
Descripcion: Predicado finaliza el juego actualizando las estadísticas
de los jugadores según el resultado.
Dominio: Game(game) X EndedGame(game)
MP: game_set_end/2
MS: who_is_winner/2,
    update_stats/3,
    board_with_players/4,
    game_with_history/6.
*/
end_game([P1, P2, Board, Current_Turn, Historial], EndedGame):-
    who_is_winner(Board, Winner),
    Winner =:= 1,
    update_stats(P1, "win", NewP1),
    update_stats(P2, "loss", NewP2),
    board_with_players(Board, NewP1, NewP2, NewBoard),
    game_with_history(NewP1, NewP2, NewBoard, Current_Turn, Historial, EndedGame),
    !;

    who_is_winner(Board, Winner),
    Winner =:= 2,
    update_stats(P1, "loss", NewP1),
    update_stats(P2, "winn", NewP2),
    board_with_players(Board, NewP1, NewP2, NewBoard),
    game_with_history(NewP1, NewP2, NewBoard, Current_Turn, Historial, EndedGame),
    !;

    who_is_winner(Board, Winner),
    Winner =:= 0,
    update_stats(P1, "draw", NewP1),
    update_stats(P2, "draw", NewP2),
    board_with_players(Board, NewP1, NewP2, NewBoard),
    game_with_history(NewP1, NewP2, NewBoard, Current_Turn, Historial, EndedGame),
    !.


/*
RF18 TDA Game - modificador - realizarMovimiento
Descripcion: Predicado que realiza un movimiento.
Dominio: Game(game) X Player(player) X Column(int) X NewGame(game)
MP: player_play/4
MS: comprobacion_P1/5,
    jugada_P1/3,
    get_board/2,
    who_is_winner/2,
    end_game/2,
    comprobacion_P2/5,
    jugada_P2/3,
    is_draw/1,
    get_id_player/2.
*/
player_play([P1, P2, Board, Current_Turn, History], Player, Column, NewGame):-
    %Juega P1 y gana.
    comprobacion_P1(P1, Player, Board, Column, Current_Turn),
    jugada_P1([P1, P2, Board, Current_Turn, History], Column, JuegoJugado),
    get_board(JuegoJugado, NewBoard),
    who_is_winner(NewBoard, Winner),
    Winner =:= 1,
    end_game(JuegoJugado, NewGame),
    !;
    %Juega P1 y la partida sigue.
    comprobacion_P1(P1, Player, Board, Column, Current_Turn),
    jugada_P1([P1, P2, Board, Current_Turn, History], Column, JuegoJugado),
    get_board(JuegoJugado, NewBoard),
    who_is_winner(NewBoard, Winner),
    Winner =:= 0,
    NewGame = JuegoJugado,
    !;
    %Juega P1 y es empate.
    comprobacion_P1(P1, Player, Board, Column, Current_Turn),
    jugada_P1([P1, P2, Board, Current_Turn, History], Column, JuegoJugado),
    is_draw(JuegoJugado),
    end_game(JuegoJugado, NewGame),
    !;

    %Juega P2 y gana
    comprobacion_P2(P2, Player, Board, Column, Current_Turn),
    jugada_P2([P1, P2, Board, Current_Turn, History], Column, JuegoJugado),
    get_board(JuegoJugado, NewBoard),
    who_is_winner(NewBoard, Winner),
    Winner =:= 2,
    end_game(JuegoJugado, NewGame),
    !;
    %Juega P2 y la partida sigue.
    comprobacion_P2(P2, Player, Board, Column, Current_Turn),
    jugada_P2([P1, P2, Board, Current_Turn, History], Column, JuegoJugado),
    get_board(JuegoJugado, NewBoard),
    who_is_winner(NewBoard, Winner),
    Winner =:= 0,
    NewGame = JuegoJugado,
    !;
    %Juega P2 y es empate
    comprobacion_P2(P2, Player, Board, Column, Current_Turn),
    jugada_P2([P1, P2, Board, Current_Turn, History], Column, JuegoJugado),
    is_draw(JuegoJugado),
    end_game(JuegoJugado, NewGame),
    !;
    %Caso Juega P1 en turno de P2
    Current_Turn =:= 2,
    get_id_player(P2, IdP2),
    get_id_player(Player, IdPlayer),
    IdP2 =\= IdPlayer,
    NewGame = [P1, P2, Board, Current_Turn, History],
    !;
    %Caso Juega P2 en turno de P1
    Current_Turn =:= 1,
    get_id_player(P1, IdP1),
    get_id_player(Player, IdPlayer),
    IdP1 =\= IdPlayer,
    NewGame = [P1, P2, Board, Current_Turn, History],
    !;
    %Caso Columna mala
    Column > -1,
    NewGame = [P1, P2, Board, Current_Turn, History],
    !;
    %Caso Columna mala
    Column < 7,
    NewGame = [P1, P2, Board, Current_Turn, History],
    !.
