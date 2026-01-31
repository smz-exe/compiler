(* The DFA which accepts "(a|b)*abb" *)

type state =
| S0 (* Start, 14 *)
| S1 (* 1245 *)
| S2 (* 134 *)
| S3 (* 1346 *)
| S4 (* Accept, 1347 *)
| Dead

let next_state current_state char =
  match (current_state, char) with
  | (S0, 'a') -> S1
  | (S0, 'b') -> S2

  | (S1, 'a') -> S1
  | (S1, 'b') -> S3

  | (S2, 'a') -> S1
  | (S2, 'b') -> S2

  | (S3, 'a') -> S1
  | (S3, 'b') -> S4

  | (S4, 'a') -> S1
  | (S4, 'b') -> S2
  | (_, _) -> Dead

let run_automaton input_str =
  let len = String.length input_str in
  let rec loop i st =
    if i = len then st
    else loop(i+1) (next_state st input_str.[i])
  in
  match loop 0 S0 with
  | S4 -> true
  | _ -> false

let () =
  Printf.printf "Test 'ababb': %b\n" (run_automaton "ababb"); (* Should be true*)
  Printf.printf "Test 'abab': %b\n" (run_automaton "abab"); (* Should be false*)
  Printf.printf "Test 'abb': %b\n" (run_automaton "abb");
  Printf.printf "Test 'c': %b\n" (run_automaton "c");
  Printf.printf "Test 'ababababaabb': %b\n" (run_automaton "ababababaabb");

