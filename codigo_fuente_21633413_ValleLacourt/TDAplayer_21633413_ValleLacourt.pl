:- module(tdaplayer_21633413_vallelacourt, [player_with_ficha/8, get_piece_player/2, actualizarWin/2, actualizarLoss/2, actualizarDraw/2, get_id_player/2, actualizar_Fichas/2, get_remaining_pieces/2]).


%TDA player: jugador representado como una lista.
% player(Id, Name, Pieza, Wins, Losses, Draws, Remaining_pieces).

%Constructores

/*
Descripcion: Predicado que crea un jugador con la ficha ya asignada y
creada.
Dominio: Id(int) X Name(string) X Pieza(piece) X Wins(int) X Losses(int)
X Draws(int) X Remaining_pieces(int)
MP: player_with_ficha/8
MS:
*/
player_with_ficha(Id, Name, Pieza, Wins, Losses, Draws, Remaining_pieces,
       [Id, Name, Pieza, Wins, Losses, Draws, Remaining_pieces]).



%Selectores

/*
Descripcion: Predicado que entrega la ficha de un jugador.
Dominio: Player(player) X Pieza(piece)
MP: get_piece_player/2
MS:
*/
get_piece_player([_, _, Pieza|_], Pieza).


/*
Descripcion: Predicado que entrega el id de un jugador.
Dominio: Player(player) X Id(int)
MP: get_id_player/2
MS:
*/
get_id_player([Id|_], Id).


/*
Descripcion: Predicado que entrega las fichas restantes de un jugador.
Dominio: Player(player) X Piezas(int)
MP: get_remaining_pieces/2
MS:
*/
get_remaining_pieces([_, _, _, _, _, _, Piezas], Piezas).



%Modificadores

/*
Descripcion: Predicado que aumenta en una unidad las victorias
de un jugador.
Dominio: Player(player) X NewPlayer(player)
MP: actualizarWin/2
MS: player_with_ficha/8
*/
actualizarWin([Id, Name, Color, Wins, Losses, Draws, Remaining_pieces], NewPlayer):-
    NewWins is Wins + 1,
    player_with_ficha(Id, Name, Color, NewWins, Losses, Draws, Remaining_pieces, NewPlayer).


/*
Descripcion: Predicado que aumenta en una unidad las derrotas
de un jugador.
Dominio: Player(player) X NewPlayer(player)
MP: actualizarLoss/2
MS: player_with_ficha/8
*/
actualizarLoss([Id, Name, Color, Wins, Losses, Draws, Remaining_pieces], NewPlayer):-
    NewLosses is Losses + 1,
    player_with_ficha(Id, Name, Color, Wins, NewLosses, Draws, Remaining_pieces, NewPlayer).


/*
Descripcion: Predicado que aumenta en una unidad los empates de un
jugador.
Dominio: Player(player) X NewPlayer(player)
MP: actualizarDraw/2
MS: player_with_ficha/8
*/
actualizarDraw([Id, Name, Color, Wins, Losses, Draws, Remaining_pieces], NewPlayer):-
    NewDraws is Draws + 1,
    player_with_ficha(Id, Name, Color, Wins, Losses, NewDraws, Remaining_pieces, NewPlayer).


/*
Descripcion: Predicado que disminuye en una unidad las fichas de un
jugador.
Dominio: Player(player) X NewPlayer(player)
MP: actualizar_Fichas/2
MS: player_with_ficha/8
*/
actualizar_Fichas([Id, Name, Color, Wins, Losses, Draws, Remaining_Pieces], NewPlayer):-
    NewRemaining_Pieces is Remaining_Pieces - 1,
    player_with_ficha(Id, Name, Color, Wins, Losses, Draws, NewRemaining_Pieces, NewPlayer).
