:- module(tdaboard_21633413_vallelacourt, [get_elem_fila/3,get_fila/3,get_columna/3,set_pieza_fila/4,set_fila_board/4,fila_baja/2]).

%Selectores:

/*
Descripcion: Predicado que dada una fila, retorna el elemento en la
enesima posicion.
Dominio: Fila(lista) X Pos(int) X Ficha(piece)
MP: get_elem_fila/3.
MS: get_elem_fila/3.
*/
get_elem_fila([Car|_], 0, Car).
get_elem_fila([_|Cdr], Pos, Elem):-
    Pos > 0,
    PosRec is Pos - 1,
    get_elem_fila(Cdr, PosRec, Elem).


/*
Descripcion: Predicado que dado un tablero y una posicion, retorna la
fila de dicha posicion.
Dominio: Board(board) X Pos(int) X FilaOut(lista)
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
MP: set_fila_board/4.
MS: set_fila_board/4.
*/
set_fila_board([_|Cdr], 0, Fila, [Fila|Cdr]).
set_fila_board([Car|Cdr], PosFila, Fila, [Car|CdrRec]):-
    PosFila > 0,
    PosFilaRec is PosFila - 1,
    set_fila_board(Cdr, PosFilaRec, Fila, CdrRec).




%OTROS:

/*
Descripcion: Predicado que dada una columna, determian cual es la
posicion mas baja a la que se puede colocar una ficha.
Dominio: Columna(lista) X PosFila(int)
MP:fila_baja/2
MS: member/2.
    fila_baja/2.
*/
fila_baja([_, _, _, _, _, 0], 5).
fila_baja([Car1,Car2|Cdr], 0):-
    Car1 =:= 0,
    Car2 =\= 0,
    member(0, [Car1, Car2, Cdr]).
fila_baja([_|Cdr], PosFila):-
    fila_baja(Cdr, Acc),
    PosFila is Acc + 1.
