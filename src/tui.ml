open Notty
open Notty.Infix
open Bug

module Term = Notty_unix.Term

let fprintf = Format.fprintf

type game = Game.t

module History : sig
  type t
  type move = { coord : Coord.t ; game : game }
  val show_move : move -> string
  val make : game -> t
  val current_game : t -> game
  val insert : move -> t -> t
  val prev : t -> t
  val next : t -> t
  val fold : (?active:bool -> int -> move -> 'a -> 'a) -> 'a -> t -> 'a
end = struct
  type move = {
    coord : Coord.t;
    game : game;
  }

  type t = game * move list * move list
  (* A zipper. The first value is the initial game. *)

  let make g = g, [], []

  let active = function
    | _, l :: _ls, _ -> Some l
    | _, [], _ -> None

  let current_game (g, _, _ as t) =
    match active t with
    | Some m -> m.game
    | None -> g

  (* Overrides the moves after the active one *)
  let insert x (g, ls, _) = g, x :: ls, []

  let prev = function
    | g, l :: ls, rs -> g, ls, l :: rs
    | t -> t

  let next = function
    | g, ls, r :: rs -> g, r :: ls, rs
    | t -> t

  let show_move { coord; game } =
    let player = Player.opponent @@ Game.current_player game in
    let p = match player with Black -> "b" | White -> "w" in
    let num n = if n >= 0 then "+" ^ string_of_int n else string_of_int n in
    Coord.(Printf.sprintf "%s%s%s%s" p
      (num @@ q coord) (num @@ r coord) (num @@ s coord))

  let fold (f : ?active:bool -> _) init t =
    let fold_move_list (i, acc) list =
      List.fold_left (fun (i, acc) m -> i + 1, f i m acc) (i, acc) list in
    match t with
    | _, a :: ls, rs ->
      let ls = List.rev ls in
      let (i, acc) = fold_move_list (1, init) ls in
      let acc = f ~active:true i a acc and i = i + 1 in
      let (_i, acc) = fold_move_list (i, acc) rs in
      acc
    | _, [], rs ->
      let (_i, acc) = fold_move_list (1, init) rs in
      acc
end

let black_color = A.(fg (gray 14))
let white_color = A.(fg lightwhite)

let attr_black = A.(st bold ++ black_color)
let attr_white = A.(st bold ++ white_color)

let black_stone = I.string ~attr:attr_black "B "
let white_stone = I.string ~attr:attr_white "W "
let allowed = I.string ". "
let disallowed = I.string ~attr:A.(fg lightblack) ", "
let black_stone_allowed = I.string ~attr:A.(st italic ++ black_color) "B "
let white_stone_allowed = I.string ~attr:A.(st italic ++ white_color) "W "

let pp_player ppf (player : Player.t) =
  match player with
  | Black -> I.pp_attr attr_black Format.pp_print_string ppf "Black"
  | White -> I.pp_attr attr_white Format.pp_print_string ppf "White"

let status_line ?(illegal = false) game =
  let p = Game.current_player game in
  let growing ppf =
    if Game.is_growing game then fprintf ppf " %a must grow." pp_player p in
  let illegal_move = if illegal then " [Illegal move.]" else "" in
  I.strf "%a's turn.%t%s" pp_player p growing illegal_move

let winning_line game =
  match Game.final_state game with
  | InProgress -> I.strf ""
  | Won p -> I.strf "%a has won!" pp_player p

let convert_coord center c =
  (* Axial coordinates relative to the left side: *)
  let x = Coord.q c + center and y = Coord.r c + center in
  (* With the shift (the rows above center are already shifted in axial): *)
  let x = if y > center then x - (center - y) else x in
  x, y

type board_cell =
  | Allowed | Disallowed
  | BlackStone | WhiteStone
  | BlackStoneAllowed | WhiteStoneAllowed

let board game =
  let rect_side = Game.board_size game * 2 - 1 in
  let pmoves = Game.possible_moves game in
  let default = match pmoves with
    | AllowedCells _ -> Disallowed
    | DisallowedCells _ -> Allowed in
  let arr = Array.make_matrix rect_side rect_side default in
  let center = Game.board_size game - 1 in
  Game.fold_stones game (fun coord (color : Player.t) () ->
    let v = match color with Black -> BlackStone | White -> WhiteStone in
    let x, y = convert_coord center coord in
    arr.(y).(x) <- v
  ) ();
  begin match pmoves with
  | AllowedCells cells ->
    List.iter (fun c ->
      let x, y = convert_coord center c in
      match arr.(y).(x) with
      | Disallowed -> arr.(y).(x) <- Allowed
      | BlackStone -> arr.(y).(x) <- BlackStoneAllowed
      | WhiteStone -> arr.(y).(x) <- WhiteStoneAllowed
      | _ -> ()
    ) cells
  | DisallowedCells (_, cells) ->
    List.iter (fun c ->
      let x, y = convert_coord center c in
      match arr.(y).(x) with
      | Allowed -> arr.(y).(x) <- Disallowed
      | _ -> ()
    ) cells
  end;
  let vimg = ref I.empty in
  for y = 0 to Array.length arr - 1 do
    let himg = ref I.empty in
    let shift = abs (center - y) in
    for x = shift to Array.length arr.(y) - 1 do
      himg := !himg <|> (match arr.(y).(x) with
        | Allowed -> allowed
        | Disallowed -> disallowed
        | BlackStone -> black_stone
        | WhiteStone -> white_stone
        | BlackStoneAllowed -> black_stone_allowed
        | WhiteStoneAllowed -> white_stone_allowed
      )
    done;
    himg := I.hpad shift shift !himg;
    himg := !himg <|> I.string "|";
    vimg := !vimg <-> !himg
  done;
  !vimg

let game_history height history =
  let move_i n m =
    I.strf " %d.%s" n (History.show_move m) in
  let move_active_i n m =
    I.strf ~attr:A.(st reverse) "[%d.%s]" n (History.show_move m) in
  let f ?(active = false) n move (len, current, imgs) =
    if active then
      len + 1, n      , move_active_i n move :: imgs
    else
      len + 1, current, move_i n move :: imgs
  in
  let len, current, imgs = History.fold f (0, 1, []) history in
  let img = I.vcat @@ List.rev imgs in
  let bot = max (len - current - height + 1) 0 in
  let top = max (len - height - bot) 0 in
  if len >= height then I.vcrop top bot img else img

let response_time sec =
  I.strf ~attr:A.(fg lightblack) "[response time: %fms]" (sec *. 1000.)

let help =
  let line1 = "press j/k to scroll the history, "
            ^ "arrow keys to move the cursor, enter to play" in
  let line2 = "    . can move here    , cannot move here"
            ^ "    B black stone    W white stone" in
  assert String.(length line1 <= 80 && length line2 <= 80);
  let attr = A.(fg lightblack) in
  I.string ~attr line1 <-> I.string ~attr line2

type t = {
  cursor : Coord.t;
  history : History.t;
  illegal : bool;
  time : float;
}

let draw { illegal; history; time; _ } =
  let game = History.current_game history in
  let height = Game.board_size game * 2 - 1 in
  let header = status_line ~illegal game <-> winning_line game in
  let board_with_history = board game <|> game_history height history in
  let footer = response_time time <-> help in
  I.vcat [header; board_with_history; footer]

let () =
  let term = Term.create ~mouse:false () in (* TODO: support mouse as well? *)
  let at_least_two_args = Array.length Sys.argv >= 2 in
  let size = if at_least_two_args then int_of_string Sys.argv.(1) else 4 in
  let board_start_y = 2 in
  let center = size - 1 in
  let t = {
    cursor = Coord.zero;
    history = History.make (Game.make ~size ());
    illegal = false;
    time = 0.;
  } in
  let move t (dir : Coord.direction) =
    let cursor' = Coord.(move t.cursor dir) in
    if Coord.less_than cursor' size then
      { t with cursor = cursor' }
    else
      t
  in
  let play t =
    let time = Unix.gettimeofday () in
    let result = Game.play (History.current_game t.history) t.cursor in
    let time = Unix.gettimeofday () -. time in
    match result with
    | None -> { t with time; illegal = true }
    | Some game ->
      let history = History.(insert { coord = t.cursor; game } t.history) in
      { t with time; history; illegal = false }
  in
  let scroll dir t =
    match dir with
    | `Up -> { t with history = History.prev t.history; illegal = false }
    | `Down -> { t with history = History.next t.history; illegal = false }
  in
  let rec loop t =
    Term.image term @@ draw t;
    let x, y = convert_coord center t.cursor in
    let shift = abs (center - y) in
    Term.cursor term @@ Some (x * 2 - shift, y + board_start_y);
    match Term.event term with
    | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) -> ()
    | `Key (`ASCII 'q', []) -> ()
    | `Resize _ -> loop t
    | `Key (`Arrow `Up, []) -> loop @@ move t NE
    | `Key (`Arrow `Down, []) -> loop @@ move t SW
    | `Key (`Arrow `Left, []) -> loop @@ move t W
    | `Key (`Arrow `Right, []) -> loop @@ move t E
    | `Key (`ASCII 'j', []) -> loop @@ scroll `Down t
    | `Key (`ASCII 'k', []) -> loop @@ scroll `Up t
    | `Key ((`Enter | `ASCII ' '), []) -> loop @@ play t
    | _ -> loop t
  in
  loop t
