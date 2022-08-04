module Coord : sig
  type t = private { q : int; r : int; s : int }
  (** Cube coordinates for a hexhex board.
      Must hold the invariant: [q + r + s = 0].
      The module does not check the invariant. *)

  type direction =
    |  NW | NE
    | W   |   E
    |  SW | SE

  val make : int -> int -> int -> t
  (** [make q r s]. The result is unspecified if [q + r + s <> 0]. *)

  val zero : t

  val compare : t -> t -> int
  val (=) : t -> t -> bool
  val less_than : t -> int -> bool
  val (--) : t -> t -> t
  val (++) : t -> t -> t
  val move : t -> direction -> t
  val rotate_cw : t -> t
  val rotate_ccw : t -> t
  val neighbors : t -> t list

  val symmetries : t -> t list
  (** [symmetries t] is 6 rotations and 6 reflections of [t].
      The order is unspecified, but is deterministic. *)

  val show : t -> string
  val invariants : t -> unit
end = struct
  type t = { q : int; r : int; s : int }
  (* Technically [s] is not necessary to store, but we store it
     anyway for convenience.
     It is possible to store a coordinate in 8 bits (or 16 bits if we drop
     support for 32-bit systems) inside the int to save 24 bytes of space.
     Additionaly, that would also make the type unboxed.
     The record should take 32 bytes of space on 64-bit systems now, not
     counting the pointer. *)

  let[@inline] make q r s = (* assert (q + r + s = 0); *) { q; r; s }

  let invariants t = assert (t.q + t.r + t.s = 0)

  let zero = make 0 0 0

  let (=) x y = x.q = y.q && x.r = y.r && x.s = y.s

  let (--) x y = make (x.q - y.q) (x.r - y.r) (x.s - y.s)

  let (++) x y = make (x.q + y.q) (x.r + y.r) (x.s + y.s)

  let less_than x n = abs x.q < n && abs x.r < n && abs x.s < n

  let permutations ({ q; r; s } as x) =
    [x; make q s r; make r q s; make r s q; make s q r; make s r q]

  let offsets = permutations @@ make ~-1 0 1
  let neighbors x = List.rev_map ((++) x) offsets

  (* (The rotate functions are unused for now) *)
  let rotate_cw x = make (-x.r) (-x.s) (-x.q)
  let rotate_ccw x = make (-x.s) (-x.q) (-x.r)

  let neg x = make (-x.q) (-x.r) (-x.s) (* diagonal reflection *)

  let symmetries x =
    let p = permutations x in
    List.rev_append p (List.rev_map neg p)

  let compare x y =
    match Int.compare x.q y.q with
    | 0 -> (match Int.compare x.r y.r with
      | 0 -> Int.compare x.s y.s
      | c -> c)
    | c -> c

  type direction = NW | NE | W | E | SW | SE

  let move x = function
    | NW -> x ++ make 0 ~-1 1
    | NE -> x ++ make 1 ~-1 0
    | W  -> x ++ make ~-1 0 1
    | E  -> x ++ make 1 0 ~-1
    | SW -> x ++ make ~-1 1 0
    | SE -> x ++ make 0 1 ~-1

  let show x = Printf.sprintf "(%d %d %d)" x.q x.r x.s
end

module Player = struct
  type t = Black | White
  let opponent = function Black -> White | White -> Black
  let starting = Black
  let (=) x y = match x, y with Black, Black | White, White -> true | _ -> false
end

module ABug : sig
  type t
  (** A bug *)

  val monohex : Player.t -> Coord.t -> t
  (** The creation of a bug always begins with a monohex *)

  val grow : t -> Coord.t -> t

  val stones : t -> Coord.t list
  val owner : t -> Player.t
  val size : t -> int

  val are_symmetrical : t -> t -> bool
  (** [are_symmetrical x y] is [true] if [x] and [y] are
      equivalent free polyhexes; [false] otherwise *)

  val neighboring_coords : t -> Coord.t list
end = struct
  module CoordSet = Set.Make(Coord)
  module HexSet = CoordSet

  type t = {
    polyhex : HexSet.t;
    size : int;
    owner : Player.t;
  }

  let monohex player coord = {
    polyhex = HexSet.singleton coord;
    size = 1;
    owner = player;
  }

  let grow t coord = {
    t with
    polyhex = HexSet.add coord t.polyhex;
    size = t.size + 1;
  }

  let stones t = HexSet.elements t.polyhex
  let owner t = t.owner
  let size t = t.size

  let symmetries_init = List.init 12 (fun _ -> HexSet.empty)
  let symmetries polyhex =
    let f hex_coord acc =
      let coord_syms = Coord.symmetries hex_coord in
      List.map2 (fun s b -> HexSet.add b s) acc coord_syms
    in
    HexSet.fold f polyhex symmetries_init

  let centered polyhex =
    let sum c (q, r, s, len) = Coord.(c.q + q, c.r + r, c.s + s, len + 1) in
    let q_s, r_s, s_s, len = HexSet.fold sum polyhex (0, 0, 0, 0) in
    let f x =
      Coord.(make (x.q * len - q_s) (x.r * len - r_s) (x.s * len - s_s)) in
    HexSet.map f polyhex

  (* TODO: A more efficient version of "are_symmetrical"?

     Is it possible to precalculate a list of canonical forms?
     For size=6, there are 814 fixed polyhexes. For size=5, 186. *)

  let are_symmetrical t1 t2 =
    t1.size = t2.size &&
    let x = centered t1.polyhex in
    let y = centered t2.polyhex in
    List.exists (HexSet.equal y) (symmetries x)

  let neighboring_coords t =
    let f hex total_neighbors =
      let hex_neighbors = CoordSet.of_list @@ Coord.neighbors hex in
      CoordSet.union total_neighbors hex_neighbors
    in
    let result = CoordSet.fold f t.polyhex CoordSet.empty in
    (* Then remove the coords inside the polyhex *)
    let result = CoordSet.diff result t.polyhex in
    CoordSet.elements result
end

module BugStore : sig
  type ref
  type t
  val empty : t
  val add : t -> ABug.t -> t * ref
  val get : t -> ref -> ABug.t
  val get_opt : t -> ref -> ABug.t option
  val set : t -> ref -> ABug.t -> t
  val remove : t -> ref -> t
  module Ref : sig
    type t = ref
    val compare : ref -> ref -> int
    val (=) : ref -> ref -> bool
    val (<>) : ref -> ref -> bool
  end
end = struct
  type ref = int
  module M = Map.Make(Int)
  type t = {
    fresh_ref : ref;
    map : ABug.t M.t;
  }

  let empty = { fresh_ref = 0; map = M.empty }

  let add t bug =
    let t' =
      { fresh_ref = t.fresh_ref + 1; map = M.add t.fresh_ref bug t.map } in
    t', t.fresh_ref

  let remove t ref = { t with map = M.remove ref t.map }

  (* This can raise an exception if called after remove *)
  let get t ref = match M.find_opt ref t.map with
    | Some x -> x
    | None -> failwith "BugStore: the ref has not been found"

  let get_opt t ref = M.find_opt ref t.map

  let set t ref new_bug =
    { t with map = M.add ref new_bug t.map }

  module Ref = struct
    type t = ref
    let compare = Int.compare
    let (=) = Int.equal
    let (<>) x y = not (Int.equal x y)
  end
end

module BugMap : sig
  type t
  (** This contains bug1 <-> bug2 connections, indexed by both bug1 and bug2. *)

  val empty : t
  val add : t -> BugStore.ref -> BugStore.ref -> t
  val add_list : t -> BugStore.ref -> BugStore.ref list -> t
  val get_all : t -> BugStore.ref -> BugStore.ref list
  (** [get_all t b]: retrieve all bugs connected to [b] *)

  val remove : t -> BugStore.ref -> t
  val fold : t -> (BugStore.ref -> BugStore.ref list -> 'a -> 'a) -> 'a -> 'a
  (** Fold over all bugs with their connected bugs *)
end = struct
  module RefSet = Set.Make(BugStore.Ref)
  module M = Map.Make(BugStore.Ref)
  type t = RefSet.t M.t (* A bidirectional multimap *)

  (* Invariant: A value of the Set is always present in the Map as a key *)

  let empty = M.empty

  let add_single_link t r1 r2 =
    let f = function
      | Some s -> Some (RefSet.add r2 s)
      | None -> Some (RefSet.singleton r2)
    in
    M.update r1 f t

  let add t r1 r2 =
    let t = add_single_link t r1 r2 in
    add_single_link t r2 r1

  let add_list t r1 r2s =
    let r2s_set = RefSet.of_list r2s in
    let f = function
      | Some s -> Some (RefSet.union s r2s_set)
      | None -> Some r2s_set in
    let with_r1 = M.update r1 f t in
    List.fold_left (fun map r2 -> add_single_link map r2 r1) with_r1 r2s

  let get_all t r =
    match M.find_opt r t with
    | Some x -> RefSet.elements x
    | None -> []

  let remove_single t r1 r2 =
    let f = function
      | Some s ->
        let s' = RefSet.remove r2 s in
        if RefSet.is_empty s' then None else Some s'
      | None -> None in
    M.update r1 f t

  (* Note: remove is quite expensive *)
  let remove t r =
    (* This should take O(k * log n) where k = number of connections *)
    match M.find_opt r t with
    | None -> t
    | Some r2s ->
      (* There is no find and remove operation, so this does a log n operation
         twice (we can also mutate a ref inside Map.update) *)
      let t = M.remove r t in
      RefSet.fold (fun r2 map -> remove_single map r2 r) r2s t

  let fold t f init = M.fold (fun k v acc -> f k (RefSet.elements v) acc) t init
end

module Game : sig
  type t
  type final_state = InProgress | Won of Player.t
  type possible_moves =
    | AllowedCells of Coord.t list
    | DisallowedCells of int * Coord.t list
        (** [DisallowedCells (len, moves)]: [len] is precalculated length of
            the [moves] list. Implicit invariant: [len = List.length moves]. *)

  val make : ?size:int -> unit -> t
  val current_player : t -> Player.t
  val is_growing : t -> bool
  val possible_moves : t -> possible_moves
  val final_state : t -> final_state
  val board_size : t -> int
  val fold_stones : t -> (Coord.t -> Player.t -> 'a -> 'a) -> 'a -> 'a
  val play : t -> Coord.t -> t option
end = struct
  module NeighborSet = Set.Make(BugStore.Ref)

  type cell =
    { stone : BugStore.ref option; neighbor_of : Set.Make(BugStore.Ref).t }

  module Cells : sig
    type t
    val empty : t
    val add_neighbors : t -> BugStore.ref -> Coord.t list -> t
    val add_stone : t -> BugStore.ref -> Coord.t -> t
    val remove_neighbors : t -> BugStore.ref -> Coord.t list -> t
    val remove_stones : t -> Coord.t list -> t
    val find_opt : Coord.t -> t -> cell option
    val fold : (Coord.t -> cell -> 'b -> 'b) -> t -> 'b -> 'b
  end = struct
    module CoordMap = Map.Make(Coord)
    type t = cell CoordMap.t

    (* Invariant: No stone inside a bug should have a neighbor_of pointing
                  to the bug *)

    let add_neighbors cells bref coords =
      let mapper = function
        | Some ({ stone = Some stone_bref; neighbor_of } as cell) ->
          if BugStore.Ref.(stone_bref = bref) then
            Some cell
          else
            Some { cell with neighbor_of = NeighborSet.add bref neighbor_of }
        | Some { stone = None; neighbor_of } ->
          Some { stone = None; neighbor_of = NeighborSet.add bref neighbor_of }
        | None ->
          Some { stone = None; neighbor_of = NeighborSet.singleton bref }
      in
      let f map coord = CoordMap.update coord mapper map in
      List.fold_left f cells coords

    let add_stone cells bref coord =
      let mapper = function
        | None -> Some { stone = Some bref; neighbor_of = NeighborSet.empty }
        | Some { neighbor_of } ->
          let neighbor_of = NeighborSet.remove bref neighbor_of in
          Some { stone = Some bref; neighbor_of }
      in
      CoordMap.update coord mapper cells

    (** It is assumed that the [coords] are inside the board *)
    let remove_neighbors cells bref coords =
      let mapper = function
        | Some { stone; neighbor_of } ->
          let neighbor_of = NeighborSet.remove bref neighbor_of in
          if Option.is_none stone && NeighborSet.is_empty neighbor_of then
            None
          else
            Some { stone; neighbor_of }
        | None -> None
      in
      let f map coord = CoordMap.update coord mapper map in
      List.fold_left f cells coords

    (** It is assumed that the [stones] exist on the board *)
    let remove_stones cells coords =
      let mapper = function
        | Some { neighbor_of } when NeighborSet.is_empty neighbor_of -> None
        | Some { neighbor_of } -> Some { stone = None; neighbor_of }
        | None -> failwith "remove_stones is called with a non-existent stone"
      in
      let f map coord = CoordMap.update coord mapper map in
      List.fold_left f cells coords

    let empty = CoordMap.empty
    let find_opt = CoordMap.find_opt
    let fold = CoordMap.fold
  end

  type final_state =
    | InProgress
    | Won of Player.t

  type possible_moves =
    | AllowedCells of Coord.t list
    | DisallowedCells of int * Coord.t list

  type move_type =
    | PlaceOrGrow
    | EatAndGrow of { eating : (BugStore.ref * BugStore.ref list) list }
        (** [{ eating : (eater * being_eated list) list }]*)

  type t = {
    board_size : int;
    bugs : BugStore.t;
    cells : Cells.t;
    conns : BugMap.t;
    max_bug_size : int;
    known_symmetries : BugMap.t;
    to_move : Player.t;
    move_type : move_type;
    possible_moves : possible_moves;
    final_state : final_state;
  }

  let make ?(size = 4) () =
    if size < 0 then
      invalid_arg "Game.make: size must be greater than 0";
    {
      board_size = size;
      bugs = BugStore.empty;
      cells = Cells.empty;
      conns = BugMap.empty;
      max_bug_size = 1;
      known_symmetries = BugMap.empty;
      to_move = Player.starting;
      move_type = PlaceOrGrow;
      possible_moves = DisallowedCells (0, []);
      final_state = InProgress;
    }

  let current_player t = t.to_move
  let is_growing t = match t.move_type with
    | PlaceOrGrow -> false
    | EatAndGrow _ -> true
  let possible_moves t = t.possible_moves
  let final_state t = t.final_state
  let board_size t = t.board_size

  let fold_stones t f init =
    let g c cell acc =
      match cell with
      | { stone = Some bref } ->
        let bug = BugStore.get t.bugs bref in
        f c (ABug.owner bug) acc
      | _ -> acc
    in
    Cells.fold g t.cells init

  let is_inside_board t coord =
    Coord.less_than coord t.board_size

  let bug_neighbors t bug =
    List.filter (is_inside_board t) @@ ABug.neighboring_coords bug

  let coord_neighbors t coord =
    List.filter (is_inside_board t) @@ Coord.neighbors coord

  let[@inline] split_neighbors t neighbor_of =
    let f bref (ours, theirs) =
      let bug = BugStore.get t.bugs bref in
      if Player.(ABug.owner bug = t.to_move) then
        (bug, bref) :: ours, theirs
      else
        ours, bref :: theirs
    in
    NeighborSet.fold f neighbor_of ([], [])

  (* The code can be seen as divided into three states with the possible
     transitions (final_phase is implicit):
     - place_or_grow -> final_phase, eat_and_grow
     - eat_and_grow -> final_phase, eat_and_grow
     - final_phase -> place_or_grow, eat_and_grow
     The final_phase checks whether any other bug on the board can eat and
     grow, not taking the placed stone into account. No input is allowed during
     the final_phase.

     The code assumes the following holds:
     - No bug can have a neighbor of the same color.
     - A possibility of being eaten by multiple bugs can arise *only* during the
       final_phase. Only one chosen bug can eat and grow during any phase. If
       multiple bugs have a possibility to eat, other bugs are postponed to
       future eat_and_grow.
     - Only bugs that were previously rejected to eat due to lack of space to
       grow, or the bugs that were targets of those bugs, can grow in the
       final_phase. If a bug A has been rejected to eat a set of bugs B, A and B
       can eat and grow during the final_phase. No other bugs can grow during
       the final_phase.
       If one of those bugs has grown, it can no longer eat in the final_phase.
     - ... *)

  let calculate_possible_moves t =
    (* This should take roughly O(number_of_neighbors * log(number_of_bugs)) *)
    let f coord cell (len, pmoves) =
      let disallow = len + 1, coord :: pmoves and allow = len, pmoves in
      match cell with
      | { stone = Some _ } -> disallow
      | { neighbor_of } ->
        begin match fst @@ split_neighbors t neighbor_of with
        | [] -> allow
        | _ :: _ :: _ -> disallow
        | [bug, _bref] when ABug.size bug < t.max_bug_size -> allow
        | [_bug, _bref] -> disallow
        end
    in
    let (len, pmoves) = Cells.fold f t.cells (0, []) in
    DisallowedCells (len, pmoves)

  (** Performs a merge check. Returns a list of possible moves. If the list is
      empty, the bug has no space to grow and cannot eat. *)
  let can_grow t (the_bug, the_bref) eated =
    let is_eaten bref =
      List.exists (fun r -> BugStore.Ref.(r = bref)) eated in
    let is_our bref = ABug.(owner (BugStore.get t.bugs bref) = t.to_move) in
    let can_grow_to coord = match Cells.find_opt coord t.cells with
      | Some { stone = Some bref } when not (is_eaten bref) -> false
      | Some { neighbor_of } ->
        let f bref = is_our bref && BugStore.Ref.(bref <> the_bref) in
        not @@ NeighborSet.exists f neighbor_of
      | None -> true
    in
    let neighbors = bug_neighbors t the_bug in
    List.filter can_grow_to neighbors

  let grow_next t eating =
    { t with move_type = EatAndGrow { eating } }

  let place_next t =
    let f bref eated (pmoves, eating) =
      assert (match eated with [] -> false | _ -> true);
      let bug = BugStore.get t.bugs bref in
      if Player.(ABug.owner bug = t.to_move) then
        match can_grow t (bug, bref) eated with
        | [] -> pmoves, eating
        | _ :: _ as pmoves' ->
          List.rev_append pmoves' pmoves, (bref, eated) :: eating
      else
        pmoves, eating
    in
    match BugMap.fold t.known_symmetries f ([], []) with
    | [], [] -> (* Other bugs on the board still have no space to grow *)
      let t = { t with to_move = Player.opponent t.to_move } in
      let possible_moves = calculate_possible_moves t in
      { t with possible_moves; move_type = PlaceOrGrow }
    | pmoves, eating ->
      let t = { t with possible_moves = AllowedCells pmoves } in
      grow_next t eating

  let placed t (bug, bref) =
    let connected_opponent_bugs = BugMap.get_all t.conns bref in
    let f r = match BugStore.get_opt t.bugs r with
      | Some b -> ABug.are_symmetrical bug b
      | None -> false in
    let eated = List.filter f connected_opponent_bugs in
    match eated with
    | [] -> place_next t
    | _ :: _ ->
      match can_grow t (bug, bref) eated with
      | [] -> (* Merge check has failed *)
        let known_symmetries = BugMap.add_list t.known_symmetries bref eated in
        place_next { t with known_symmetries }
      | _ :: _ as pmoves -> (* Can eat succesfully *)
        let t = { t with possible_moves = AllowedCells pmoves } in
        grow_next t [bref, eated]

  (* Bugs are never removed from t.conns, that's fine *)

  let grow_existing_bug t coord (bug, bref) theirs =
    let bug' = ABug.grow bug coord in
    let bugs = BugStore.set t.bugs bref bug' in
    let cells = Cells.add_neighbors t.cells bref (coord_neighbors t coord) in
    let cells = Cells.add_stone cells bref coord in
    let conns = BugMap.add_list t.conns bref theirs in
    let known_symmetries = BugMap.remove t.known_symmetries bref in
    let t = { t with bugs; cells; conns; known_symmetries } in
    placed t (bug', bref)

  let create_monohex_bug t coord theirs =
    let bug = ABug.monohex t.to_move coord in
    let (bugs, bref) = BugStore.add t.bugs bug in
    let cells = Cells.add_neighbors t.cells bref (coord_neighbors t coord) in
    let cells = Cells.add_stone cells bref coord in
    let conns = BugMap.add_list t.conns bref theirs in
    let t = { t with bugs; cells; conns } in
    placed t (bug, bref)

  let eat t bref =
    let bug = BugStore.get t.bugs bref in
    let cells = Cells.remove_neighbors t.cells bref (bug_neighbors t bug) in
    let cells = Cells.remove_stones cells (ABug.stones bug) in
    let bugs = BugStore.remove t.bugs bref in
    let known_symmetries = BugMap.remove t.known_symmetries bref in
    { t with cells; bugs; known_symmetries }

  let eat_many t beaing_eaten =
    List.fold_left eat t beaing_eaten

  let place_or_grow t coord =
    match Cells.find_opt coord t.cells with
    | Some { stone = Some _ } ->
      None
    | Some { stone = None; neighbor_of } ->
      let ours, theirs = split_neighbors t neighbor_of in
      begin match ours with
      | [] ->
        Some (create_monohex_bug t coord theirs)
      | [bug, bref] when ABug.size bug < t.max_bug_size ->
        Some (grow_existing_bug t coord (bug, bref) theirs)
      (* Growing is disallowed if the bug has already reached the max size *)
      | [_bug] -> None
      (* If the cell is surrounded by 2 or more bugs, merging is not allowed *)
      | _ :: _ :: _  -> None
      end
    | None ->
      Some (create_monohex_bug t coord [])

  let guard b = if b then Some () else None
  let (let*) = Option.bind

  let eat_and_grow t coord eating =
    let* () = guard @@ match t.possible_moves with
      | DisallowedCells _ -> failwith "Game.eat_and_grow expects AllowedCells"
      | AllowedCells coords -> List.exists Coord.((=) coord) coords
    in
    let* { neighbor_of } = Cells.find_opt coord t.cells in
    match split_neighbors t neighbor_of with
    | [bug, bref], theirs ->
      let f (r, list) = if BugStore.Ref.(bref = r) then Some list else None in
      let* being_eaten = List.find_map f eating in
      let t = eat_many t being_eaten in
      let max_bug_size = max t.max_bug_size (ABug.size bug + 1) in
      let t = { t with max_bug_size } in
      Some (grow_existing_bug t coord (bug, bref) theirs)
    | _ -> None

  (* TODO: Are calculate_possible_moves and the checks in place_or_grow / etc.
           always consistent? Is it possible to combine the calculations? *)

  let play t coord =
    let* () = guard @@ is_inside_board t coord in
    let* t = match t.move_type with
      | PlaceOrGrow -> place_or_grow t coord
      | EatAndGrow { eating } -> eat_and_grow t coord eating
    in
    (* To get the total number of the cells on a hexhex board, we can count the
       "rings" of the board, and then add the remaining center cell:
       1 + \sum_{i=1}^{n-1}6i    where n = length of an edge in cells
       After transforming to the closed form (\sum_{i=1}^{n}i = n(n+1)/2),
       this equals to:
       1 + 6n(n-1)/2  =  1 + 3n(n-1)
       This can also be viewed as dividing the board into three parallelograms
       with n and n-1 sides. The only unoccupied cell then is the center. *)
    let total_cells = 3 * t.board_size * (t.board_size - 1) + 1 in
    let final_state = match t.possible_moves with
      | AllowedCells [] -> Won t.to_move
      | AllowedCells (_ :: _)
      | DisallowedCells (_, []) -> InProgress
      | DisallowedCells (len, _ :: _) when len = total_cells -> Won t.to_move
      | DisallowedCells (_, _ :: _) -> InProgress
    in
    Some { t with final_state }

  (* TODO: Functions for importing and exporting?
      let import_board str : t = ...
      let export_board t : string = ... *)
end
