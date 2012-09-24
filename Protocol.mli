type dir =
  | N | E | W | S

type client_msg =
  | MOVE of pos * dir
  | BOMB of pos

type server_msg =
  | MOVE_OK of player * pos * dir
  | BOMB_OK of player * pos
  | EXPLOSION of pos list * player list * pos * int
