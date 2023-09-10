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
  let pp t = "{x:" ^ string_of_int t.x ^ " y:" ^ string_of_int t.y ^ "}"
end

module PositionSet = Set.Make (Position)

type head = Head of Position.t
type tail = Tail of Position.t

let as_head node =
  match node with
  | Tail p -> Head p
;;

let footprint_store = ref (PositionSet.singleton { x = 0; y = 0 })

let footprint_with (tail : tail list) (footprint : PositionSet.t ref) =
  footprint
    := List.fold_left
         (fun set tail ->
           match tail with
           | Tail position -> PositionSet.add position set)
         !footprint
         tail
;;

module State = struct
  type t =
    { head : head
    ; tail : tail list
    }

  let pp t =
    match t.head, t.tail with
    | Head head, tail ->
      Position.pp head
      ^ List.fold_right
          (fun e acc ->
            match e with
            | Tail e -> Position.pp e ^ acc)
          tail
          ""
  ;;

  type instruction =
    | Up of int
    | Right of int
    | Down of int
    | Left of int

  type reaction =
    | NoAct
    | MoveStraight of
        { dis_x : int
        ; dis_y : int
        }
    | MoveDiagonal of
        { dis_x : int
        ; dis_y : int
        }

  let followup reaction tail (set_ref : PositionSet.t ref) =
    match reaction with
    | NoAct -> tail
    | MoveStraight { dis_x; dis_y } ->
      (match abs dis_x, abs dis_y, dis_x, dis_y, dis_x > 0, dis_y > 0, tail with
       | 0, range, _, dis_y, _, positivity, Tail ori ->
         let coeff = if positivity then 1 else -1 in
         let stepped =
           List.init range (fun n -> Tail { x = ori.x; y = ori.y + ((n + 1) * coeff) })
         in
         footprint_with stepped set_ref;
         Tail { x = ori.x; y = ori.y + dis_y }
       | range, 0, dis_x, _, positivity, _, Tail ori ->
         let coeff = if positivity then 1 else -1 in
         let stepped =
           List.init range (fun n -> Tail { x = ori.x + ((n + 1) * coeff); y = ori.y })
         in
         footprint_with stepped set_ref;
         Tail { x = ori.x + dis_x; y = ori.y }
       | _, _, _, _, _, _, _ -> invalid_arg "invalid case")
    | MoveDiagonal { dis_x; dis_y } ->
      (match abs dis_x, abs dis_y, dis_x, dis_y, dis_x > 0, dis_y > 0, tail with
       | 1, range, dis_x, dis_y, _, positivity, Tail ori ->
         let coeff = if positivity then 1 else -1 in
         let stepped =
           List.init range (fun n ->
             Tail { x = ori.x + dis_x; y = ori.y + ((n + 1) * coeff) })
         in
         footprint_with stepped set_ref;
         Tail { x = ori.x + dis_x; y = ori.y + dis_y }
       | range, 1, dis_x, dis_y, positivity, _, Tail ori ->
         let coeff = if positivity then 1 else -1 in
         let stepped =
           List.init range (fun n ->
             Tail { x = ori.x + ((n + 1) * coeff); y = ori.y + dis_y })
         in
         footprint_with stepped set_ref;
         Tail { x = ori.x + dis_x; y = ori.y + dis_y }
       | _, _, _, _, _, _, _ -> invalid_arg "invalid case")
  ;;

  let follow head tail set_ref =
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
          MoveStraight
            { dis_x = 0; dis_y = (if diff_y > 0 then diff_y - 1 else diff_y + 1) }
        | _, 0, diff_x, _ ->
          MoveStraight
            { dis_x = (if diff_x > 0 then diff_x - 1 else diff_x + 1); dis_y = 0 }
        | 1, _, diff_x, diff_y ->
          MoveDiagonal
            { dis_x = diff_x; dis_y = (if diff_y > 0 then diff_y - 1 else diff_y + 1) }
        | _, 1, diff_x, diff_y ->
          MoveDiagonal
            { dis_x = (if diff_x > 0 then diff_x - 1 else diff_x + 1); dis_y = diff_y }
        | _, _, _, _ ->
          invalid_arg ("Invalid case: h" ^ Position.pp h ^ "t" ^ Position.pp t)
      in
      followup reaction tail set_ref
  ;;

  let get_new n_tail =
    let tail = List.init n_tail (fun _ -> Tail { x = 0; y = 0 }) in
    { head = Head { x = 0; y = 0 }; tail }
  ;;

  let act_on instuction node =
    match node, instuction with
    | Head p, Up m -> List.init m (fun n -> Head { x = p.x; y = p.y + (n+1) })
    | Head p, Right m -> List.init m (fun n -> Head { x = p.x + (n+1); y = p.y })
    | Head p, Down m -> List.init m (fun n -> Head { x = p.x; y = p.y - (n+1) })
    | Head p, Left m -> List.init m (fun n -> Head { x = p.x - (n+1); y = p.y })
  ;;

  let chain_react_with head (tail : tail list) set_ref =
    List.rev
      (List.fold_left
         (fun acc tail ->
           match acc with
           | [] -> [ follow head tail set_ref ]
           | last_tail :: _ ->
             let head = as_head last_tail in
             follow head tail set_ref :: acc)
         []
         tail)
  ;;

  let mutate_with instruction state_ref set_ref =
    let head = act_on instruction !state_ref.head in
    List.fold_left (fun _ head -> let tail = chain_react_with head !state_ref.tail set_ref in state_ref := {head; tail}) () head
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

let state_store = ref (State.get_new 2)
let line_count = ref 0

let () =
  consume_input_text_line_by_line ~filepath:"./input" ~consumer:(fun opt ->
    (* consume_input_text_line_by_line ~filepath:"./input" ~consumer:(fun opt -> *)
    match opt with
    | Some str ->
      print_endline ("ok: " ^ str);
      State.mutate_with (State.instruction_of str) state_store footprint_store;
      line_count := !line_count + 1;
      print_endline (string_of_int !line_count ^ ": " ^ State.pp !state_store)
    | None ->
      print_endline "End of file";
      let set_size = ref 0 in
      PositionSet.iter (fun _ -> set_size := !set_size + 1) !footprint_store;
      print_endline (string_of_int !set_size))
;;
