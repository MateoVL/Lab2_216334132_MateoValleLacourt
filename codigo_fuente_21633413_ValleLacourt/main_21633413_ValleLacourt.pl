:- use_module("TDAboard_21633413_ValleLacourt").
:- use_module("TDAplayer_21633413_ValleLacourt").



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
    member(0, Fila0).


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
play_piece(Board, Column, Piece, NewBoard):-
    board_with_players(Tablero, Player1, Player2, Board),
    get_columna(Tablero, Column, ListColumn),
    fila_baja(ListColumn, PosFila),
    get_fila(Tablero, PosFila, Fila),
    set_pieza_fila(Fila, Column, Piece, NewFila),
    set_fila_board(Tablero, PosFila, NewFila, NewTablero),
    board_with_players(NewTablero,Player1, Player2, NewBoard).


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
%check_vertical_win([[], _, _], 0).
%check_vertical_win([[Car|Cdr], P1, P2], Winner):-
%    get_columna([Car|Cdr], 0, Columna0),
%    cuatro_seguidos(Columna0, P1, P2, Acc),
%    check_vertical_win([Cdr,P1,P2], WinnerRec),
%    Winner is Acc + WinnerRec.



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
    cuatro_seguidos(Car, P1, P2, Acc),
    check_horizontal_win([Cdr,P1,P2], WinnerRec),
    Winner is Acc + WinnerRec.
