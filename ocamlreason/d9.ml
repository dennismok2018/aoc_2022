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

  (* todo *)
  let pp t = "x:" ^ string_of_int t.x ^ " y:" ^ string_of_int t.y
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
    ; tail : tail list
    }

  type instruction =
    | Up of int
    | Right of int
    | Down of int
    | Left of int

  type reaction =
    | NoAct
    | MoveBy of
        { dis_x : int
        ; dis_y : int
        }

  let followup reaction tail =
    match reaction with
    | NoAct -> tail
    | MoveBy { dis_x; dis_y } ->
      (match tail with
       | Tail ori -> Tail { x = ori.x + dis_x; y = ori.y + dis_y })
  ;;

  let follow head tail =
    match tail, head with
    | Tail t, Head h ->
      let diff_x = h.x - t.x in
      let diff_y = h.y - t.y in
      let reaction =
        match abs diff_x, abs diff_y, diff_x, diff_y with
        | 0, 0, _, _ -> NoAct
        | 1, 1, _, _ -> NoAct
        | 0, 1, _, _ -> NoAct
        | 1, 0, _, _ -> NoAct
        | 0, _, _, diff_y ->
          MoveBy { dis_x = 0; dis_y = (if diff_y > 0 then diff_y - 1 else diff_y + 1) }
        | _, 0, diff_x, _ ->
          MoveBy { dis_x = (if diff_x > 0 then diff_x - 1 else diff_x + 1); dis_y = 0 }
        | 1, _, diff_x, diff_y ->
          MoveBy
            { dis_x = diff_x; dis_y = (if diff_y > 0 then diff_y - 1 else diff_y + 1) }
        | _, 1, diff_x, diff_y ->
          MoveBy
            { dis_x = (if diff_x > 0 then diff_x - 1 else diff_x + 1); dis_y = diff_y }
        | _, _, _, _ -> invalid_arg "Invalid case"
      in
      followup reaction tail
  ;;

  let get_new n_tail =
    let tail = List.init n_tail (fun _ -> Tail { x = 0; y = 0 }) in
    { head = Head { x = 0; y = 0 }; tail }
  ;;

  let act_on instuction node =
    match node, instuction with
    | Head p, Up m -> Head { x = p.x; y = p.y + m }
    | Head p, Right m -> Head { x = p.x + m; y = p.y }
    | Head p, Down m -> Head { x = p.x; y = p.y - m }
    | Head p, Left m -> Head { x = p.x - m; y = p.y }
  ;;

  let chain_react_with head (tail : tail list) = tail

  let mutate_with instruction state =
    let head = act_on instruction state.head in
    let tail = chain_react_with head state.tail in
    { head; tail }
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

let state_store = ref (State.get_new 1)
let footprint_store = ref PositionSet.empty

let footprint_with (state : State.t) footprint =
  List.fold_left
    (fun set tail ->
      match tail with
      | Tail position -> PositionSet.add position set)
    footprint
    state.tail
;;

let () =
  consume_input_text_line_by_line ~filepath:"./input" ~consumer:(fun opt ->
    match opt with
    | Some str ->
      state_store := State.mutate_with (State.instruction_of str) !state_store;
      footprint_store := footprint_with !state_store !footprint_store
    | None ->
      print_endline "End of file";
      PositionSet.iter (fun pt -> print_endline (Position.pp pt)) !footprint_store)
;;
