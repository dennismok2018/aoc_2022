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

let replace a ind lst = List.mapi (fun i e -> if i = ind then a else e) lst

(* let left_and_right_consume_input_text_line_by_line
   ~filepath
   ~condition
   ~left
   ~right
   ?first
   ?last
   ()
   =
   let continue = ref true in
   let rec loop ~condition ~left ~right ~first ~last ~channel =
   (match first with
   | None -> ()
   | Some f -> f ());
   (match condition () with
   | true -> continue := left channel
   | false -> right ());
   (match last with
   | None -> ()
   | Some f -> f ());
   if continue.contents then loop ~condition ~left ~right ~first ~last ~channel
   in
   In_channel.with_open_text filepath (fun channel ->
   loop ~condition ~left ~right ~first ~last ~channel)
   ;; *)

(* let () =
   left_and_right_consume_input_text_line_by_line
   ~filepath:"./input"
   ~condition:is_work_queue_empty
   ~left:read_and_push_to_quene
   ~right:resolve_sim
   ~first:incr_cycle_and_sample
   ();
   print_endline (string_of_int (cycle ()));
   print_newline ();
   List.iter (fun e -> e |> string_of_int |> print_endline) !samples_taken;
   print_newline ();
   List.fold_left (fun acc e -> e + acc) 0 !samples_taken |> string_of_int |> print_endline
   ;; *)

(* todo *)
module Monkey = struct
  type t =
    { mutable items : int list
    ; mutable count : int
    ; change_int_on_inspect : int -> int
    ; divisor : int
    ; act_on : bool -> t list -> int -> t list
    }
  [@@deriving show]

  type input_line_t =
    | Start
    | Items of int list
    | Op of (int -> int)
    | Divisor of int
    | True_Action of (t list -> int -> t list)
    | False_Action of (t list -> int -> t list)

  let substrs_of_input_line =
    [ ("Monkey ", fun _ -> Start)
    ; ( "  Starting items: "
      , fun (line, prefix) ->
          let items =
            let extracted =
              let s_len = String.length line in
              let pre_len = String.length prefix in
              String.sub line pre_len (s_len - pre_len)
            in
            (* print_endline extracted; *)
            List.map
              (fun s -> int_of_string (String.trim s))
              (List.filter
                 (fun s -> String.trim s <> "")
                 (String.split_on_char ',' extracted))
          in
          Items items )
    ; ("  Operation: new = old * old", fun _ -> Op (fun i -> i * i))
    ; ( "  Operation: new = old + "
      , fun (line, prefix) ->
          let c =
            let extracted =
              let s_len = String.length line in
              let pre_len = String.length prefix in
              String.sub line pre_len (s_len - pre_len)
            in
            (* print_endline extracted; *)
            int_of_string (String.trim extracted)
          in
          Op (fun i -> i + c) )
    ; ( "  Operation: new = old * "
      , fun (line, prefix) ->
          let c =
            let extracted =
              let s_len = String.length line in
              let pre_len = String.length prefix in
              String.sub line pre_len (s_len - pre_len)
            in
            (* print_endline extracted; *)
            int_of_string (String.trim extracted)
          in
          Op (fun i -> i * c) )
    ; ( "  Test: divisible by "
      , fun (line, prefix) ->
          let i =
            let extracted =
              let s_len = String.length line in
              let pre_len = String.length prefix in
              String.sub line pre_len (s_len - pre_len)
            in
            (* print_endline extracted; *)
            int_of_string (String.trim extracted)
          in
          Divisor i )
    ; ( "    If true: throw to monkey "
      , fun (line, prefix) ->
          let target =
            let extracted =
              let s_len = String.length line in
              let pre_len = String.length prefix in
              String.sub line pre_len (s_len - pre_len)
            in
            (* print_endline extracted; *)
            int_of_string (String.trim extracted)
          in
          True_Action
            (fun old_lst new_value ->
              let old_monkey = List.nth old_lst target in
              let old_items = old_monkey.items in
              let new_items = old_items @ [ new_value ] in
              old_monkey.items <- new_items;
              let new_lst = replace old_monkey target old_lst in
              new_lst) )
    ; ( "    If false: throw to monkey "
      , fun (line, prefix) ->
          let index =
            let extracted =
              let s_len = String.length line in
              let pre_len = String.length prefix in
              String.sub line pre_len (s_len - pre_len)
            in
            (* print_endline extracted; *)
            int_of_string (String.trim extracted)
          in
          False_Action
            (fun old_lst new_value ->
              let old_monkey = List.nth old_lst index in
              let old_items = old_monkey.items in
              let new_items = old_items @ [ new_value ] in
              old_monkey.items <- new_items;
              let new_lst = replace old_monkey index old_lst in
              new_lst) )
    ]
  ;;

  let extract t =
    let items = t.items in
    match items with
    | [] -> t, None
    | hd :: tl ->
      t.items <- tl;
      t.count <- t.count + 1;
      t, Some hd
  ;;
end

open Monkey

(* read input *)
let monkeys = ref []
let items_ref = ref None
let op_ref = ref None
let divisor_ref = ref None
let tr_fn_ref = ref None
let fl_fn_ref = ref None

let init_refs () =
  items_ref := None;
  op_ref := None;
  divisor_ref := None;
  tr_fn_ref := None;
  fl_fn_ref := None
;;

let make_monkey () =
  match !items_ref, !op_ref, !divisor_ref, !tr_fn_ref, !fl_fn_ref with
  | Some items, Some op, Some divsor, Some tf, Some ff ->
    { items
    ; count = 0
    ; change_int_on_inspect = op
    ; divisor = divsor
    ; act_on = (fun b -> if b then tf else ff)
    }
  | _ -> invalid_arg "invalid state"
;;

let _ =
  consume_input_text_line_by_line ~filepath:"./input" ~consumer:(fun str_opt ->
    match str_opt with
    | Some input_line ->
      (try
         let prefix, cons =
           List.find
             (fun (prefix, _) -> String.starts_with ~prefix input_line)
             substrs_of_input_line
         in
         match cons (input_line, prefix) with
         | Start -> init_refs ()
         | Items items -> items_ref := Some items
         | Op op -> op_ref := Some op
         | Divisor pred -> divisor_ref := Some pred
         | True_Action action -> tr_fn_ref := Some action
         | False_Action action ->
           fl_fn_ref := Some action;
           monkeys := make_monkey () :: !monkeys
       with
       | Not_found -> ())
    | None ->
      print_endline "EOF reached;Finished reading input";
      monkeys := List.rev !monkeys)
;;

let modder =
  List.fold_left (fun acc e -> acc * e) 1 (List.map (fun mk -> mk.divisor) !monkeys)
;;

(* runs *)

let round = ref 0

let rec run () =
  round := !round + 1;
  (* List.fold_left (fun _ e -> print_endline (string_of_int e.count)) () !monkeys;
     List.fold_left (fun _ e -> print_endline (show e)) () !monkeys; *)
  let _ =
    List.init (List.length !monkeys) (fun index ->
      let rec f () =
        let mk = List.nth !monkeys index in
        let nmk, item = extract mk in
        let nmonkeys = replace nmk index !monkeys in
        match item with
        | None -> ()
        | Some new_value ->
          let nit = nmk.change_int_on_inspect new_value mod modder in
          let nacc = nmk.act_on (0 = nit mod nmk.divisor) nmonkeys nit in
          monkeys := nacc;
          f ()
      in
      f ())
  in
  match !round with
  | 10000 ->
    List.fold_left (fun _ e -> print_endline (string_of_int e.count)) () !monkeys;
    List.fold_left (fun _ e -> print_endline (show e)) () !monkeys
  | _ -> run ()
;;

let _ = run ()

(* todo^ *)
