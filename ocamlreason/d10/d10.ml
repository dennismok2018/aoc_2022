type instruction =
  | NoOp
  | Add of int

let instruction_of str =
  match String.split_on_char ' ' str with
  | [ "noop" ] -> NoOp
  | [ "addx"; x ] -> Add (int_of_string x)
  | _ -> invalid_arg "unrecognized input"
;;

let pp instruction =
  match instruction with
  | NoOp -> "noop"
  | Add x -> "addx " ^ string_of_int x
;;

(* todo *)

let register_sim = ref 1
let register () = !register_sim
let add x = register_sim := x + !register_sim
let cycle_ref = ref 0
let cycle () = !cycle_ref
let samples_taken = ref []
let sample i = samples_taken := i :: !samples_taken
let sample () = if (cycle () - 20) mod 40 = 0 then sample (register () * cycle ())

let incr_cycle_and_sample () =
  cycle_ref := 1 + !cycle_ref;
  sample ()
;;

let work_quene = ref []
let is_work_queue_empty () = [] = !work_quene
let push x = work_quene := (1, x) :: !work_quene

let read_and_push_to_quene channel =
  match In_channel.input_line channel with
  | None ->
    print_endline "EOF";
    false
  | Some str ->
    (match instruction_of str with
     | NoOp -> ()
     | Add x -> push x);
    true
;;

let resolve_sim () =
  match !work_quene with
  | [ (1, x) ] ->
    add x;
    work_quene := []
  | _ -> invalid_arg "invalid state"
;;

(* todo^ *)

(* let consume_input_text_line_by_line ~filepath ~consumer =
   let rec consume_with ~producer =
   match producer () with
   | None -> consumer None
   | Some line ->
   consumer (Some line);
   consume_with ~producer
   in
   In_channel.with_open_text filepath (fun channel ->
   consume_with ~producer:(fun () -> In_channel.input_line channel))
   ;; *)

let left_and_right_consume_input_text_line_by_line
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
;;

let () =
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
;;
