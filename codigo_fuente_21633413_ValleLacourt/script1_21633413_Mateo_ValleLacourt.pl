:- use_module("main_21633413_ValleLacourt").
:- use_module("TDAplayer_21633413_ValleLacourt").
:- use_module("TDAboard_21633413_ValleLacourt").
:- use_module("TDAgame_21633413_ValleLacourt").


% 1. Crear jugadores (10 fichas cada uno para un juego corto)
player(1, "Pedro", "rojo", 0, 0, 0, 21, P1),
player(2, "Maria", "verde", 0, 0, 0, 21, P2),
% 2. Crear fichas
piece("rojo", PiezaRoja),
piece("verde", PiezaVerde),
% 3. Crear tablero inicial vacío
board(TableroVacio),
% 4. Crear nuevo juego
game(P1, P2, TableroVacio, 1, G0),
% 5. Realizando movimientos para crear una victoria diagonal
player_play(G0, P1, 0, G1),
player_play(G1, P2, 1, G2),
player_play(G2, P1, 0, G3),
player_play(G3, P2, 1, G4),
player_play(G4, P1, 0, G5),
player_play(G5, P2, 0, G6),
player_play(G6, P1, 1, G7),
player_play(G7, P2, 3, G8),
player_play(G8, P1, 3, G9),
player_play(G9, P2, 0, G10),
player_play(G10, P1, 3, G11),
player_play(G11, P1, 0, G12),    %P1 trata de jugar en turno incorrecto
player_play(G12, P2, 2, G13),
player_play(G13, P1, 4, G14),
player_play(G14, P2, 4, G15),
player_play(G15, P1, 0, G16),
player_play(G16, P2, 0, G17),    %P2 trata de colocar en columna 0 pero esta llena
player_play(G17, P2, 3, G18),
player_play(G18, P1, 5, G19),
player_play(G19, P2, 6, G20),
player_play(G20, P1, 5, G21),
player_play(G21, P2, 6, G22),
player_play(G22, P1, 5, G23),
player_play(G23, P2, 5, G24),
player_play(G24, P1, 4, G25),
player_play(G25, P2, 6, G26),
player_play(G26, P1, 6, G27),
player_play(G27, P2, 4, G28),
player_play(G28, P1, 2, G29),
player_play(G29, P2, 2, G30),
player_play(G30, P1, 6, G31),
player_play(G31, P2, 3, G32),
player_play(G32, P1, 4, G33),
player_play(G33, P2, 6, G34),
player_play(G34, P1, 1, G35),
player_play(G35, P2, 1, G36),
player_play(G36, P1, 2, G37),
player_play(G37, P2, 5, G38),
player_play(G38, P1, 2, G39),
player_play(G39, P2, 5, G40),
player_play(G40, P1, 1, G41),
player_play(G41, P2, 3, G42),
player_play(G42, P1, 4, G43),
player_play(G43, P2, 2, G44),

% Juan juega en columna 3 (victoria diagonal)
% 6. Verificaciones del estado del juego
write('¿Se puede jugar en el tablero vacío? '),
can_play(TableroVacio), % Si se puede seguir jugando, el programa continuará
nl,
game_get_board(G44, CurrentBoard),
write('¿Se puede jugar después de 44 movimientos? '),
can_play(CurrentBoard),
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

% 8. Verificación de empate
   write('¿Es empate? '),
   is_draw(G44),
   nl,

% 9. Finalizar juego y actualizar estadísticas
   end_game(G44, EndedGame),

% 10. Mostrar historial de movimientos
   write('Historial de movimientos: '),
   game_history(EndedGame, History),
   write(History),
   nl,

% 11. Mostrar estado final del tablero
   write('Estado final del tablero: '),
   game_get_board(EndedGame, FinalBoard),
   write(FinalBoard).
