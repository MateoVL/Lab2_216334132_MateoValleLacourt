:- module(tdaplayer_21633413_vallelacourt, [get_piece_player/2, actualizarWin/2, actualizarLoss/2, actualizarDraw/2, get_id_player/2, actualizar_Fichas/2, get_remaining_pieces/2]).

%Constructores


%Selectores

/*
Descripcion: Predicado que entrega la ficha de un jugador.
Dominio: Player(player) X Pieza(piece)
MP: get_piece_player/2
MS:
*/
get_piece_player([_, _, Pieza|_], Pieza).

get_id_player([Id|_], Id).

get_remaining_pieces([_, _, _, _, _, _, Piezas], Piezas).

%Modificadores

actualizarWin([Id, Name, Color, Wins, Losses, Draws, Remaining_pieces], NewPlayer):-
    NewWins is Wins + 1,
    player(Id, Name, Color, NewWins, Losses, Draws, Remaining_pieces, NewPlayer).
actualizarLoss([Id, Name, Color, Wins, Losses, Draws, Remaining_pieces], NewPlayer):-
    NewLosses is Losses + 1,
    player(Id, Name, Color, Wins, NewLosses, Draws, Remaining_pieces, NewPlayer).
actualizarDraw([Id, Name, Color, Wins, Losses, Draws, Remaining_pieces], NewPlayer):-
    NewDraws is Draws + 1,
    player(Id, Name, Color, Wins, Losses, NewDraws, Remaining_pieces, NewPlayer).

actualizar_Fichas([Id, Name, Color, Wins, Losses, Draws, Remaining_Pieces], NewPlayer):-
    NewRemaining_Pieces is Remaining_Pieces - 1,
    player(Id, Name, Color, Wins, Losses, Draws, NewRemaining_Pieces, NewPlayer).
