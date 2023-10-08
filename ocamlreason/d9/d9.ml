module Position = struct
  type t =
    { x : int
    ; y : int
    }

  let compare t1 t2 =
    match Stdlib.compare t1.x t2.x with
    | 0 -> Stdlib.compare t1.y t2.y
    | c -> c
  ;;

  let pp t = "{x:" ^ string_of_int t.x ^ " y:" ^ string_of_int t.y ^ "}"
end

module PositionSet = Set.Make (Position)

type head = Head of Position.t
type tail = Tail of Position.t

let as_head node =
  match node with
  | Tail p -> Head p
;;

module State = struct
  type t =
    { head : head
    ; tail_lst : tail list
    }

  let pp t =
    match t.head, t.tail_lst with
    | Head head, tail_lst ->
      Position.pp head
      ^ List.fold_right
          (fun e acc ->
            match e with
            | Tail e -> Position.pp e ^ acc)
          tail_lst
          ""
  ;;

  type instruction =
    | Up of int
    | Right of int
    | Down of int
    | Left of int

  type movement =
    | MoveBy of
        { dis_x : int
        ; dis_y : int
        }

  let get_new n_tail =
    let tail_lst = List.init n_tail (fun _ -> Tail { x = 0; y = 0 }) in
    { head = Head { x = 0; y = 0 }; tail_lst }
  ;;

  let disect instuction =
    match instuction with
    | Up m -> List.init m (fun _ -> MoveBy { dis_x = 0; dis_y = 1 })
    | Right m -> List.init m (fun _ -> MoveBy { dis_x = 1; dis_y = 0 })
    | Down m -> List.init m (fun _ -> MoveBy { dis_x = 0; dis_y = -1 })
    | Left m -> List.init m (fun _ -> MoveBy { dis_x = -1; dis_y = 0 })
  ;;

  let react_to head tail =
    match head, tail with
    | Head h, Tail t ->
      let diff_x = h.x - t.x in
      let diff_y = h.y - t.y in
      let tail =
        match diff_x, diff_y with
        | 0, 0 -> tail
        | 0, 1 -> tail
        | 0, -1 -> tail
        | 1, 0 -> tail
        | -1, 0 -> tail
        | 1, 1 -> tail
        | 1, -1 -> tail
        | -1, -1 -> tail
        | -1, 1 -> tail
        (* On Axes *)
        | 2, 0 -> Tail { x = t.x + 1; y = t.y }
        | -2, 0 -> Tail { x = t.x - 1; y = t.y }
        | 0, 2 -> Tail { x = t.x; y = t.y + 1 }
        | 0, -2 -> Tail { x = t.x; y = t.y - 1 }
        (* Q1 *)
        | 2, 1 -> Tail { x = t.x + 1; y = t.y + 1 }
        | 1, 2 -> Tail { x = t.x + 1; y = t.y + 1 }
        | 2, 2 -> Tail { x = t.x + 1; y = t.y + 1 }
        (* Q2 *)
        | 2, -1 -> Tail { x = t.x + 1; y = t.y - 1 }
        | 1, -2 -> Tail { x = t.x + 1; y = t.y - 1 }
        | 2, -2 -> Tail { x = t.x + 1; y = t.y - 1 }
        (* Q3 *)
        | -2, -1 -> Tail { x = t.x - 1; y = t.y - 1 }
        | -1, -2 -> Tail { x = t.x - 1; y = t.y - 1 }
        | -2, -2 -> Tail { x = t.x - 1; y = t.y - 1 }
        (* Q4 *)
        | -2, 1 -> Tail { x = t.x - 1; y = t.y + 1 }
        | -1, 2 -> Tail { x = t.x - 1; y = t.y + 1 }
        | -2, 2 -> Tail { x = t.x - 1; y = t.y + 1 }
        | _, _ -> invalid_arg ("Invalid case: h" ^ Position.pp h ^ "t" ^ Position.pp t)
      in
      tail
  ;;

  let follow head tail = react_to head tail

  let chain_react_with head tail_lst recordFunc =
    let result =
      List.fold_left
        (fun acc tail ->
          match acc with
          | [] -> [ follow head tail ]
          | last_tail :: _ ->
            let head = as_head last_tail in
            follow head tail :: acc)
        []
        tail_lst
    in
    match result with
    | [] -> invalid_arg "invalid case"
    | last :: _ ->
      recordFunc last;
      List.rev result
  ;;

  let move head i =
    match head, i with
    | Head head, MoveBy { dis_x; dis_y } ->
      Head { x = head.x + dis_x; y = head.y + dis_y }
  ;;

  let mutate_with instruction state recordFunc =
    List.fold_left
      (fun s i ->
        let head = move s.head i in
        let tail_lst = s.tail_lst in
        let tail_lst = chain_react_with head tail_lst recordFunc in
        { head; tail_lst })
      state
      (disect instruction)
  ;;

  let instruction_of str =
    match String.split_on_char ' ' str with
    | [ "R"; multitude ] -> Right (int_of_string multitude)
    | [ "L"; multitude ] -> Left (int_of_string multitude)
    | [ "U"; multitude ] -> Up (int_of_string multitude)
    | [ "D"; multitude ] -> Down (int_of_string multitude)
    | _ -> invalid_arg "unrecognized input"
  ;;
end

let consume_input_text_line_by_line ~filepath ~consumer =
  let rec consume_with ~producer =
    match producer () with
    | None -> consumer None
    | Some line ->
      consumer (Some line);
      consume_with ~producer
  in
  In_channel.with_open_text filepath (fun channel ->
    consume_with ~producer:(fun () -> In_channel.input_line channel))
;;

let state_store = ref (State.get_new 9)
let footprint_store = ref (PositionSet.singleton { x = 0; y = 0 })

let footprint tail =
  match tail with
  | Tail tail -> footprint_store := PositionSet.add tail !footprint_store
;;

let line_count = ref 0

let () =
  consume_input_text_line_by_line ~filepath:"./input" ~consumer:(fun opt ->
    match opt with
    | Some str ->
      print_endline ("ok: " ^ str);
      state_store := State.mutate_with (State.instruction_of str) !state_store footprint;
      line_count := !line_count + 1;
      print_endline (string_of_int !line_count ^ ": " ^ State.pp !state_store)
    | None ->
      print_endline "End of file";
      let set_size = ref 0 in
      PositionSet.iter (fun _ -> set_size := !set_size + 1) !footprint_store;
      print_endline (string_of_int !set_size))
;;
