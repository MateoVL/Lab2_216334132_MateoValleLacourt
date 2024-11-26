% 1. Crear jugadores (10 fichas cada uno para un juego corto)
player(1, "PapaFrita", "negro", 0, 0, 0, 21, P1),
player(2, "Hamburgesa", "blanco", 0, 0, 0, 21, P2),
% 2. Crear fichas
piece("negro", PiezaNegra),
piece("blanco", PiezaBlanca),
% 3. Crear tablero inicial vacío
board(TableroVacio),
% 4. Crear nuevo juego
game(P1, P2, TableroVacio, 1, G0),
% 5. Realizando movimientos para crear una victoria diagonal
player_play(G0, P1, 0, G1),
player_play(G1, P2, 2, G2),
player_play(G2, P1, 3, G3),
player_play(G3, P2, 0, G4),
player_play(G4, P1, 4, G5),
player_play(G5, P2, 2, G6),
player_play(G6, P1, 5, G7),
player_play(G7, P2, 6, G8),
player_play(G8, P1, 6, G9),
player_play(G9, P2, 5, G10),
player_play(G10, P1, 1, G11),
player_play(G11, P2, 5, G12),    %P1 trata de jugar en turno incorrecto
player_play(G12, P1, 1, G13),
player_play(G13, P2, 5, G14),
player_play(G14, P1, 5, G15),
player_play(G15, P2, 6, G16),
player_play(G16, P1, 6, G17),    %P2 trata de colocar en columna 0 pero esta llena
player_play(G17, P2, 5, G18),
player_play(G18, P1, 6, G19),
player_play(G19, P2, 3, G20),
player_play(G20, P1, 0, G21),
player_play(G21, P2, 3, G22),
player_play(G22, P1, 1, G23),
player_play(G23, P2, 6, G24),
player_play(G24, P1, 3, G25),
player_play(G25, P2, 4, G26), %Victoria horizontal, se termina el juego y actualiza estadisticas

% 6. Verificaciones del estado del juego
write('¿Se puede jugar en el tablero vacío? '),
can_play(TableroVacio), % Si se puede seguir jugando, el programa continuará
nl,
game_get_board(G12, Board12),
write('¿Se puede jugar después de 12 movimientos? '),
can_play(Board12),
game_get_board(G26, CurrentBoard),
nl,
 write('Jugador actual después de 12 movimientos: '),
   get_current_player(G12, CurrentPlayer),
   write(CurrentPlayer),
   nl,

% 7. Verificaciones de victoria
   write('Verificación de victoria vertical: '),
   check_vertical_win(CurrentBoard, VerticalWinner),
   write(VerticalWinner),
   nl,

   write('Verificación de victoria horizontal: '),
   check_horizontal_win(CurrentBoard, HorizontalWinner),
   write(HorizontalWinner),
   nl,

   write('Verificación de victoria diagonal: '),
   check_diagonal_win(CurrentBoard, DiagonalWinner),
   write(DiagonalWinner),
   nl,

   write('Verificación de ganador: '),
   who_is_winner(CurrentBoard, Winner),
   write(Winner),
   nl,

% 8. Mostrar historial de movimientos
   write('Historial de movimientos: '),
   game_history(G26, History),
   write(History),
   nl,

% 9. Mostrar estado final del tablero
   write('Estado final del tablero: '),
   game_get_board(G26, FinalBoard),
   write(FinalBoard).
