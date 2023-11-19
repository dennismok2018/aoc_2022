let consume_input_text_line_by_line ~filepath ~consume =
  let rec exhaust channel =
    match In_channel.input_line channel with
    | None -> ()
    | Some line ->
      consume line;
      exhaust channel
  in
  In_channel.with_open_text filepath (fun channel -> exhaust channel)
;;

(* let replace a ind lst = List.mapi (fun i e -> if i = ind then a else e) lst *)

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
   ; ()In_channel.with_open_text filepath (fun channel ->
   loop ~condition ~left ~right ~first ~last ~channel)
   ;; *)

(* let unique lst =
   let rec aux l acc =
   match l with
   | [] -> acc
   | h :: t -> if List.mem h acc then aux t acc else aux t (h :: acc)
   in
   aux lst []
   ;; *)
(* todo *)

(* Define the lexer tokens *)
type token =
  | NUMBER of int
  | COMMA
  | LEFT_BRACKET
  | RIGHT_BRACKET

(* Define the regular expressions for each token *)
let comma = Str.regexp ","
let left_bracket = Str.regexp "\\["
let right_bracket = Str.regexp "\\]"
let number = Str.regexp "[0-9]+"

(* Function to match numbers *)
let tokenize_number str = NUMBER (int_of_string str)

(* Error handling *)
let invalid_character c = Printf.printf "Invalid character: %c\n" c

(* Create the lexer *)
let lex str =
  let len = String.length str in
  let rec tokenize pos =
    if pos >= len
    then []
    else (
      let rest = String.sub str pos (len - pos) in
      if Str.string_match comma rest 0
      then COMMA :: tokenize (pos + 1)
      else if Str.string_match left_bracket rest 0
      then LEFT_BRACKET :: tokenize (pos + 1)
      else if Str.string_match right_bracket rest 0
      then RIGHT_BRACKET :: tokenize (pos + 1)
      else if Str.string_match number rest 0
      then (
        let matched = Str.matched_string rest in
        let token = tokenize_number matched in
        token :: tokenize (pos + String.length matched))
      else (
        let c = String.get rest 0 in
        invalid_character c;
        invalid_arg "SHIT"))
  in
  tokenize 0
;;

type line_t =
  | Blank
  | Line_1
  | Line_2

type node_t =
  | N of int
  | List of node_t list

let print_tokens tokens =
  let _ =
    List.map
      (function
       | LEFT_BRACKET -> print_string "["
       | RIGHT_BRACKET -> print_string "]"
       | COMMA -> print_string ","
       | NUMBER n -> string_of_int n |> print_string)
      tokens
  in
  print_endline ""
;;

let rec print_parsed = function
  | N n -> print_string ("(N:" ^ string_of_int n ^ ")")
  | List nodes ->
    print_string "(List: [";
    let _ = List.map (fun n -> print_parsed n) nodes in
    print_string "])"
;;

let parse tokens =
  let _ = print_tokens tokens in
  let rec aux tokens acc =
    match tokens with
    | [] -> [], List.rev acc
    | NUMBER n :: rest -> aux rest (N n :: acc)
    | LEFT_BRACKET :: rest ->
      (match aux rest [] with
       | right, returned -> aux right (List returned :: acc))
    | RIGHT_BRACKET :: rest -> rest, List.rev acc
    | COMMA :: rest -> aux rest acc
  in
  match aux tokens [] with
  | [], acc -> List.hd acc
  | _ -> invalid_arg "unhandled case"
;;

let populate_with line =
  let tokens = lex line in
  let nodes = parse tokens in
  nodes
;;

let load_input filepath =
  let last_line = ref Blank in
  let line1, line2 = ref (List []), ref (List []) in
  let pairs = ref [] in
  consume_input_text_line_by_line ~filepath ~consume:(fun line ->
    match !last_line with
    | Blank ->
      line1 := populate_with line;
      let _ = print_parsed !line1 in
      print_endline "";
      last_line := Line_1
    | Line_1 ->
      line2 := populate_with line;
      let _ = print_parsed !line2 in
      print_endline "";
      print_endline "";
      last_line := Line_2;
      pairs := (!line1, !line2) :: !pairs
    | Line_2 -> last_line := Blank);
  List.rev !pairs
;;

let compare_2_nodes a_node b_node =
  let rec compare_2_lsts a_lst b_lst =
    match a_lst, b_lst with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | N a :: a_rest, N b :: b_rest ->
      (match compare a b with
       | -1 -> -1
       | 0 -> compare_2_lsts a_rest b_rest
       | 1 -> 1
       | _ -> invalid_arg "not going to happen here")
    | List a :: a_rest, List b :: b_rest ->
      (match compare_2_lsts a b with
       | -1 -> -1
       | 0 -> compare_2_lsts a_rest b_rest
       | 1 -> 1
       | _ -> invalid_arg "not going to happen there")
    | N a :: a_rest, List _b :: _b_rest -> compare_2_lsts (List [ N a ] :: a_rest) b_lst
    | List _a :: _a_rest, N b :: b_rest -> compare_2_lsts a_lst (List [ N b ] :: b_rest)
  in
  match a_node, b_node with
  | List a, List b ->
    (match compare_2_lsts a b with
     | -1 -> true
     | 1 -> false
     | _ -> invalid_arg "no inner way")
  | _, _ -> invalid_arg "no way"
;;

(*  *)
let run () =
  let pairs = load_input "./input" in
  let adds =
    List.mapi
      (fun i (n1, n2) ->
        print_endline ("pair" ^ string_of_int (i + 1));
        print_parsed n1;
        print_endline "\n---";
        print_parsed n2;
        print_endline "";
        if compare_2_nodes n1 n2
        then (
          print_endline "true";
          print_endline "";
          i + 1)
        else (
          print_endline "false";
          print_endline "";
          0))
      pairs
  in
  let sum = List.fold_left ( + ) 0 adds in
  print_int sum;
  print_endline ""
;;

(*  *)

let _ = run ()

(*  *)
(* let run () = *)
(*   let line1 = populate_with "[1,1,3,1,1]" in *)
(*   let line2 = populate_with "[1,1,5,1,1]" in *)
(*   let _ = print_parsed line1 in *)
(*   print_endline ""; *)
(*   let _ = print_parsed line2 in *)
(*   print_endline ""; *)
(*   let adds = *)
(*     List.mapi *)
(*       (fun i (n1, n2) -> *)
(*         match n1, n2 with *)
(*         | List lst1, List lst2 -> if compare_2_nodes lst1 lst2 then i + 1 else 0 *)
(*         | _ -> invalid_arg "unhandled case") *)
(*       [ line1, line2 ] *)
(*   in *)
(*   let sum = List.fold_left ( + ) 0 adds in *)
(*   print_int sum; *)
(*   print_endline "" *)
(* ;; *)
(**)
(* let _ = run () *)
