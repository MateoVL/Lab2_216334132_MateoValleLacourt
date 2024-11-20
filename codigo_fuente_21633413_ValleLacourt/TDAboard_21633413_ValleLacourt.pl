:- module(tdaboard_21633413_vallelacourt, [board_with_players/4, get_elem_fila/3,get_fila/3,get_columna/3,set_pieza_fila/4,set_fila_board/4,fila_baja/2, cuatro_seguidos_fila/4, sacar_columna/3, verificar_fila_diagonal/4, verificar_diagonal/4, set_inv_filas/2, verificacion_DerIzq/3, verificacion_tipo_victoria/4]).
:- use_module("TDAplayer_21633413_ValleLacourt").

% TDA Board: Board es una lista con 3 elementos, una lista de listas
% representando el tablero, y jugadores participantes del tablero,
% siendo P1 y P2.


%Constructores:
/*
Descripcion: Predicado que construye un board dado un tablero y dos
jugadores.
Dominio: Tablero(listade listas) X Player1(player) X Player2(player) X
Board(board)
MP: board_with_players/4
MS:
*/
board_with_players([Tablero|_], Player1, Player2, [Tablero, Player1, Player2]).



%Selectores:

/*
Descripcion: Predicado que dada una fila, retorna el elemento en la
enesima posicion.
Dominio: Fila(lista) X Pos(int) X Ficha(piece)
Recursividad: Si.
MP: get_elem_fila/3.
MS: get_elem_fila/3.
*/
get_elem_fila([Car|_], 0, Car).
get_elem_fila([_|Cdr], Pos, Elem):-
    Pos > 0,
    PosRec is Pos - 1,
    get_elem_fila(Cdr, PosRec, Elem).


/*
Descripcion: Predicado que dado un tablero y una posicion, retorna la4
fila de dicha posicion.
Dominio: Board(board) X Pos(int) X FilaOut(lista)
Recursividad: Si.
MP: get_fila/3.
MS: get_fila/3.
*/
get_fila([Car|_], 0, Car).
get_fila([_|Cdr], FilaPos, Fila):-
    Pos is FilaPos - 1,
    get_fila(Cdr, Pos, Fila).


/*
Descripcion: Predicado que dado un tablero y una posicion, retorna la
columna de esa posicion en forma de lista.
Dominio: Board(board) X ColumnaPos(int) X ColumnaOut(lista)
Recursividad: Si
MP: get_columna/3
MS: get_elem_fila/3,
    get_columna/3.
*/
get_columna([], _, []).
get_columna([Car|Cdr], ColumnaPos,[Elem|ElemSig]):-
    get_elem_fila(Car, ColumnaPos, Elem),
    get_columna(Cdr, ColumnaPos, ElemSig).




%MODIFICADORES:

/*
Descripcion: Predicado que cambia una pieza en una fila, dada una
posicion.
Dominio: Fila(lista )X Columna(int) X Elemento(piece) X NewFila(lista)
Recursividad: Si.
MP: set_pieza_fila/4.
MS: set_pieza_fila/4.
*/
set_pieza_fila([_|Cdr]  ,      0      ,   Elem  , [Elem|Cdr]).
set_pieza_fila([Car|Cdr],   Columna   , Elemento, [Car|CdrRec]):-
    Columna > 0,
    ColumnaRec is Columna - 1,
    set_pieza_fila(Cdr, ColumnaRec, Elemento, CdrRec).


/*
Descripcion: Predicado que cambia una fila en un tablero dada una
posicion.
Dominio: Board(board) X PosFila(int) X Fila(lista) X NewBoard(board)
Recursividad: Si
MP: set_fila_board/4.
MS: set_fila_board/4.
*/
set_fila_board([_|Cdr], 0, Fila, [Fila|Cdr]).
set_fila_board([Car|Cdr], PosFila, Fila, [Car|CdrRec]):-
    PosFila > 0,
    PosFilaRec is PosFila - 1,
    set_fila_board(Cdr, PosFilaRec, Fila, CdrRec).


/*
Descripcion: Predicado que toma un tablero y aplica reverse a cada fila.
Dominio: Tablero(lista de listas) X TableroInv(lista de listas)
Recursividad: Si.
MP: set_inv_filas/2.
MS: reverse/2,
    set_inv_filas/2.
*/
set_inv_filas([], []).
set_inv_filas([Car|Cdr], [NewCar|NewCdr]):-
    reverse(Car, NewCar),
    set_inv_filas(Cdr, NewCdr).



%OTROS:

/*
Descripcion: Predicado que dada una columna, determian cual es la
posicion mas baja a la que se puede colocar una ficha.
Dominio: Columna(lista) X PosFila(int)
Recursividad: Si.
MP:fila_baja/2
MS: member/2.
    fila_baja/2.
*/
fila_baja([_, _, _, _, _, 0], 5).
fila_baja([Car1,Car2|Cdr], 0):-
    Car1 == 0,
    Car2 \== 0,
    member(0, [Car1, Car2, Cdr]).
fila_baja([_|Cdr], PosFila):-
    fila_baja(Cdr, Acc),
    PosFila is Acc + 1.

/*
Descripcion: Predicado que dada una fila,determina si hay cuatro fichas
seguidas de un jugador o no, si hay 4 seguidas de P1, regresa 1,
si hay del P2, entrega 2, si no hay 4 fichas seguidas en la
fila, entrega 0.
Dominio: Tablero(lista de listas) X P1(player) X P2(player) X
Winner(int)
Recursividad: Si.
MP: cuatro_seguidos_fila/4
MS: get_piece_player/2
    Primero = Pieza,
    Primero = Segundo,
    Primero = Tercero,
    Primero = Cuarto,
    cuatro_seguidos_fila/4
*/
cuatro_seguidos_fila([Primero, Segundo, Tercero, Cuarto|_], P1, _, 1):-
    get_piece_player(P1, Pieza),
    Primero = Pieza,
    Primero = Segundo,
    Primero = Tercero,
    Primero = Cuarto.
cuatro_seguidos_fila([Primero, Segundo, Tercero, Cuarto|_], _, P2, 2):-
    get_piece_player(P2, Pieza),
    Primero = Pieza,
    Primero = Segundo,
    Primero = Tercero,
    Primero = Cuarto.
cuatro_seguidos_fila([_, _, _], _, _, 0).
cuatro_seguidos_fila([_|Cdr], P1, P2, Winner):-
    cuatro_seguidos_fila(Cdr, P1, P2, Winner).


/*
Descripcion: Predicado que dado un tablero, calcula la primera columna
como una lista, y deja el tablero sin la primera columna.
Dominio: Tablero(lista de listas) X Columna0(lista) X
TableroSinColumna0(lista de listas)
Recursividad: Si.
MP: sacar_columna/3
MS: sacar_columna/3
*/
sacar_columna([], [], []).
sacar_columna([[Elem0|RestoFila]|Filas],[Elem0|Columna0],[RestoFila|RestoFilas]):-
    sacar_columna(Filas, Columna0, RestoFilas).


/*
Descripcion: Predicado que dado un tablero y dos jugadores, transforma
la diagonal principal de 4 caracteres de largo en una lista y verifica
si hay cuatro fichas seguidas.
Dominio: Tablero(lista de listas) X P1(player) X P2(player) X
Winner(int)
MP: verificar_digonal/4
MS: get_fila/3,
    get_elem_fila/3,
    cuatro_seguidos_fila/4
*/
verificar_diagonal(Tablero, P1, P2, Winner):-
    get_fila(Tablero, 0, Fila0),
    get_elem_fila(Fila0, 0, Elem0),
    get_fila(Tablero, 1, Fila1),
    get_elem_fila(Fila1, 1, Elem1),
    get_fila(Tablero, 2, Fila2),
    get_elem_fila(Fila2, 2, Elem2),
    get_fila(Tablero, 3, Fila3),
    get_elem_fila(Fila3, 3, Elem3),
    cuatro_seguidos_fila([Elem0, Elem1, Elem2, Elem3], P1, P2, Winner).


/*
Descripcion: Predicado que dado un tablero y dos jugadores, aplica la
funcion verificar_diagonal a todos los elementos de la primera fila de
un tablero.
Dominio: Tablero(lista de listas) X P1(player) X P2(player)
X Winner(int)
Recursividad: Si.
MP: verificar_fila_digonal/4
MS: verificar_diagonal/4,
    sacar_columna/3,
    verificar_fila_digonal/4,
    Winner is WinnerRec + Acc.
*/
verificar_fila_diagonal([[_, _, _]|_], _, _, 0).
verificar_fila_diagonal(Tablero, P1, P2, Winner):-
    verificar_diagonal(Tablero, P1, P2, Acc),
    sacar_columna(Tablero, _, NewTablero),
    verificar_fila_diagonal(NewTablero, P1, P2, WinnerRec),
    Winner is WinnerRec + Acc.


/*
Descripcion: Predicado hecho para el caso de obtener una victoria
diagonal hacia la derecha e izquierda al mismo tiempo, en vez de dar la
suma, solo obtiene un ganador.
Dominio: ResDiagonalIzq(int) X ResDiagonalDer(int) X Res(int)
MP: verificacion_DerIzq/3
MS:  Der > Izq,
     Res = Der;
     Der < Izq,
     Res = Izq;
     Der = Izq,
     Res = Der.
*/
verificacion_DerIzq(Der, Izq, Res):-
    Der > Izq,
    Res = Der.
verificacion_DerIzq(Der, Izq, Res):-
    Der < Izq,
    Res = Izq.
verificacion_DerIzq(Der, Izq, Res):-
    Der = Izq,
    Res = Der.


/*
Descripcion: Predicado hecho para el caso de obtener mas de un
tipo de victoria al mismo tiempo, en ese caso solo tomara un
caso de victoria.
Dominio: WinnerV(int) X WinnerH(int) X WinnerD(int) X Winner(int)
MP: verificacion_tipo_victoria/4
MS:  WinnerV >= WinnerH,
     WinnerV >= WinnerD,
     Winner = WinnerV;
     WinnerH >= WinnerV,
     WinnerH >= WinnerD,
     Winner = WinnerH;
     WinnerD >= WinnerV,
     WinnerD >= WinnerH,
     Winner = WinnerD.
*/
verificacion_tipo_victoria(WinnerV, WinnerH, WinnerD, Winner):-
    WinnerV >= WinnerH,
    WinnerV >= WinnerD,
    Winner = WinnerV.
verificacion_tipo_victoria(WinnerV, WinnerH, WinnerD, Winner):-
    WinnerH >= WinnerV,
    WinnerH >= WinnerD,
    Winner = WinnerH.
verificacion_tipo_victoria(WinnerV, WinnerH, WinnerD, Winner):-
    WinnerD >= WinnerV,
    WinnerD >= WinnerH,
    Winner = WinnerD.
