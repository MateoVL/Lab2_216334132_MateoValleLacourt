:- use_module("TDAboard_21633413_ValleLacourt").


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
board([[0, 0, 0, 0, 0, 0, 0],
       [0, 0, 0, 0, 0, 0, 0],
       [0, 0, 0, 0, 0, 0, 0],
       [0, 0, 0, 0, 0, 0, 0],
       [0, 0, 0, 0, 0, 0, 0],
       [0, 0, 0, 0, 0, 0, 0]]).


/*
RF05 TDA Board - otros - sePuedeJugar?
Descripcion: Predicado que permite verificar si se puede realizar más jugadas en el tablero.
Dominio: Board (board)
MP: can_play/1.
MS: member/2.
*/
can_play([Fila0|_]):-
    member(0, Fila0).


/*
RF06 TDA Board - modificador - jugar ficha
Descripcion: Predicado que permite jugar una ficha en el tablero.
Dominio: Board (board) X Column (int) X Piece (piece) X NewBoard (board)
MP: play_piece/4.
MS: get_columna/3,
    fila_baja/2,
    get_fila/3,
    set_pieza_fila/4,
    set_fila_board/4.
*/
play_piece(Board, Column, Piece, NewBoard):-
    get_columna(Board, Column, ListColumn),
    fila_baja(ListColumn, PosFila),
    get_fila(Board, PosFila, Fila),
    set_pieza_fila(Fila, Column, Piece, NewFila),
    set_fila_board(Board, PosFila, NewFila, NewBoard).
