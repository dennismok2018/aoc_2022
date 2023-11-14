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
        tokenize (pos + 1)))
  in
  tokenize 0
;;

type line_t =
  | Blank
  | Line_1
  | Line_2

type node_t =
  | Singleton of int list
  | Nested of node_t list

let parse tokens =
  let rec aux tokens acc =
    match tokens with
    | [] -> acc
    | NUMBER n :: rest -> aux rest (Singleton [ n ] :: acc)
    | LEFT_BRACKET :: rest -> [ Nested (aux rest []) ]
    | RIGHT_BRACKET :: _ -> aux [] acc
    | COMMA :: rest -> aux rest acc
  in
  match tokens with
  | [] -> []
  | LEFT_BRACKET :: _ -> aux tokens []
  | _ -> raise (invalid_arg "unrecofnized token")
;;

let populate_with line =
  let tokens = lex line in
  let nodes = parse tokens in
  nodes
;;

(* let print_tokens tokens = *)
(*   let _ =  List.map (function *)
(*   | LEFT_BRACKET -> print_string "[" *)
(*   | RIGHT_BRACKET -> print_string "]" *)
(*   | COMMA -> print_string "," *)
(*   | NUMBER n -> string_of_int n |> print_string *)
(*   ) tokens in *)
(*   print_endline "" *)

let rec print_parsed = function
  | Singleton (n :: []) -> print_string ("(Singleton:[" ^ string_of_int n ^ "]), ")
  | Nested nodes ->
    print_string "(Nested: [";
    let _ = List.map (fun n -> print_parsed n) nodes in
    print_string "])"
  | Singleton _ -> raise (invalid_arg "!")
;;

let load_input filepath =
  let last_line = ref Blank in
  let line1, line2 = ref [], ref [] in
  let pairs = ref [] in
  consume_input_text_line_by_line ~filepath ~consume:(fun line ->
    match !last_line with
    | Blank ->
      line1 := populate_with line;
      (* print_tokens !line1; *)
      let _ = List.map print_parsed !line1 in
      print_endline "";
      last_line := Line_1
    | Line_1 ->
      line2 := populate_with line;
      (* print_tokens !line2; *)
      let _ = List.map print_parsed !line2 in
      print_endline "";
      print_endline "";
      last_line := Line_2
    | Line_2 ->
      pairs := (!line1, !line2) :: !pairs;
      last_line := Blank);
  !pairs
;;

(* let rec compare_2_lsts a_lst b_lst =
   let compare_internals a_lst b_lst =
   match a_lst, b_lst with
   | [], [] -> true
   | [], _ :: _ -> true
   | _ :: _, [] -> false
   | head_a :: tail_a, head_b :: tail_b ->
   if head_a > head_b then false else compare_2_lsts tail_a tail_b
   in
   if List.length a_lst > List.length b_lst then false else compare_internals a_lst b_lst
   ;; *)

(*  *)
let run () =
  let _ = load_input "./input" in
  ()
;;

(*  *)

let _ = run ()

(*  *)
