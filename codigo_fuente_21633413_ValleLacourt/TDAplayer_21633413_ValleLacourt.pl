:- module(tdaplayer_21633413_vallelacourt, [get_piece_player/2]).

%Selectores

/*
Descripcion: Predicado que entrega la ficha de un jugador.
Dominio: Player(player) X Pieza(piece)
MP: get_piece_player/2
MS:
*/
get_piece_player([_, _, Pieza|_], Pieza).
