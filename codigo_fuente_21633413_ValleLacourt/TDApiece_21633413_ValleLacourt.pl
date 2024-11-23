:- module(tdapiece_21633413_vallelacourt, [get_ficha/2, get_color/2]).

% TDA piece: pieza representada como una lista de el color de la ficha y
% una representacion de esta para ponberla en el tablero.
% piece(Color, Ficha).

%Selectores

/*
Descripcion: Predicado que entrega la representacion de ficha en el
tablero de una Ficha.
Dominio: Pieza(piece) X Ficha(string)
MP: get_ficha/2
MS:
*/
get_ficha([_, Ficha], Ficha).


/*
Descripcion: Predicado que entrega el color de una ficha.
Dominio: Pieza(piece) X Color(string)
MP: get_color/2
MS:
*/
get_color([Color,_], Color).

