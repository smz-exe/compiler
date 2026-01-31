(* Helper: get filename without extension *)
let remove_extension filename =
  try
    let dot_pos = String.rindex filename '.' in
    String.sub filename 0 dot_pos
  with Not_found -> filename

(* Helper: get basename (filename without directory) *)
let basename filename =
  try
    let slash_pos = String.rindex filename '/' in
    String.sub filename (slash_pos + 1) (String.length filename - slash_pos - 1)
  with Not_found -> filename

(* Helper: get directory part of path *)
let dirname filename =
  try
    let slash_pos = String.rindex filename '/' in
    String.sub filename 0 slash_pos
  with Not_found -> "."

(* Helper: create directory if it doesn't exist *)
let mkdir_p dir =
  if not (Sys.file_exists dir) then
    let _ = Unix.system ("mkdir -p " ^ dir) in
    ()

(* Parse command-line arguments *)
type config = { input_file : string option; output_base : string }

let parse_args () =
  let args = Array.to_list Sys.argv |> List.tl in
  (* skip program name *)
  let rec parse args input output =
    match args with
    | [] -> (input, output)
    | "-o" :: out :: rest -> parse rest input (Some out)
    | file :: rest -> parse rest (Some file) output
  in
  let input, output = parse args None None in
  let output_base =
    match output with
    | Some out -> out
    | None -> (
        match input with
        | Some inp -> "build/" ^ remove_extension (basename inp)
        | None -> "build/out")
  in
  { input_file = input; output_base }

let main () =
  let config = parse_args () in

  (* Open input file *)
  let cin =
    match config.input_file with
    | Some filename -> open_in filename
    | None -> stdin
  in
  let lexbuf = Lexing.from_channel cin in

  (* Ensure output directory exists *)
  let out_dir = dirname config.output_base in
  mkdir_p out_dir;

  (* Generate assembly filename *)
  let asm_file = config.output_base ^ ".s" in
  let exe_file = config.output_base in

  (* Open assembly output file *)
  let file = open_out asm_file in

  (* Generate code *)
  let code = Emitter.trans_prog (Parser.prog Lexer.lexer lexbuf) in

  (* Write assembly and close *)
  output_string file code;
  close_out file;

  (* Assembling and Linking *)
  let cmd = Printf.sprintf "gcc %s -o %s" asm_file exe_file in
  let _ = Unix.system cmd in

  (* Print output location *)
  Printf.printf "Output: %s\n" exe_file;
  ()

let _ =
  try main () with
  | Parsing.Parse_error -> print_string "syntax error\n"
  | Table.No_such_symbol x -> print_string ("no such symbol: \"" ^ x ^ "\"\n")
  | Semant.TypeErr s -> print_string (s ^ "\n")
  | Semant.Err s -> print_string (s ^ "\n")
  | Table.SymErr s -> print_string (s ^ "\n")
