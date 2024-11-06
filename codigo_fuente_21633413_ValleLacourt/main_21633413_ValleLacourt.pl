


/*
RF02 TDA Player - constructor
Descripcion: Predicado que permite crear un jugador.
Dominio: id (int) X name (string) X color (string) X wins (int) X losses (int) X draws (int) X remaining_pieces (int) X Player
*/
player(Id, Name, Color, Wins, Losses, Draws, Remaining_pieces,
       [Id, Name, Color, Wins, Losses, Draws, Remaining_pieces]).

/*
RF03 TDA Piece - constructor
Descripcion: Predicado que permite crear una ficha de conecta4.
Dominio: color (string)
*/
piece(Color, [Color]).

/*
RF04 TDA Board - constructor
Descripcion: Crear un tablero de conecta4.
Dominio: sin parametros de entrada.
*/
board([Fila0, Fila1, Fila2, Fila3, Fila4, Fila5]):-
    Fila0= [0, 0, 0, 0, 0, 0, 0],
    Fila1= [0, 0, 0, 0, 0, 0, 0],
    Fila2= [0, 0, 0, 0, 0, 0, 0],
    Fila3= [0, 0, 0, 0, 0, 0, 0],
    Fila4= [0, 0, 0, 0, 0, 0, 0],
    Fila5= [0, 0, 0, 0, 0, 0, 0].

