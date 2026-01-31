open Ast
open Printf
open Types
open Table
open Semant

let label = ref 0

let incLabel () =
  label := !label + 1;
  !label

(* Copy str n times *)
let rec nCopyStr n str = if n > 0 then str ^ nCopyStr (pred n) str else ""

(* Static link to pass to callee *)
let passLink src dst =
  if src >= dst then
    let deltaLevel = src - dst + 1 in
    "\tmovq %rbp, %rax\n"
    ^ nCopyStr deltaLevel "\tmovq 16(%rax), %rax\n"
    ^ "\tpushq %rax\n"
  else "\tpushq %rbp\n"

let output = ref ""

(* Format string for printf and scanf *)
let io = "\t.data\nIO:\n\t.string \"%lld\"\n" ^ "\t.text\n"

(* Main function header *)
let header =
  "\t.globl main\n" ^ "main:\n" ^ "\tpushq %rbp\n" (* Save frame pointer *)
  ^ "\tmovq %rsp, %rbp\n" (* Set frame pointer to stack pointer *)

(* Prologue and epilogue *)
let prologue =
  "\tpushq %rbp\n" (* Save frame pointer *)
  ^ "\tmovq %rsp, %rbp\n" (* Move frame pointer to stack pointer position *)

let epilogue =
  "\tleaveq\n" (* -> movq %ebp, %esp; popl %ebp *)
  ^ "\tretq\n" (* Return to address after call site *)

(* Declaration processing: var decl -> symbol table, func def -> local decls and codegen *)
let rec trans_dec ast nest tenv env =
  match ast with
  (* Function definition *)
  | FuncDec (s, l, _, block) ->
      (* Register parameters in symbol table *)
      let env' = type_param_dec l (nest + 1) tenv env in
      (* Process function body (block) *)
      let code = trans_stmt block (nest + 1) tenv env' in
      (* Compose function code *)
      output :=
        !output ^ s ^ ":\n" (* Function label *) ^ prologue (* Prologue *)
        ^ code (* Body code *) ^ epilogue
  (* Epilogue *)
  (* Variable declaration *)
  | VarDec (t, s) -> ()
  (* Type declaration *)
  | TypeDec (s, t) -> (
      let entry = tenv s in
      match entry with
      | NAME (_, ty_opt) -> ty_opt := Some (create_ty t tenv)
      | _ -> raise (Err s))

(* Statement processing *)
and trans_stmt ast nest tenv env =
  (* temporarily ignore type checking *)
  (* type_stmt ast env; *)
  match ast with
  (* Assignment: get target frame with trans_var *)
  | Assign (v, e) ->
      trans_exp e nest env ^ trans_var v nest env ^ "\tpopq (%rax)\n"
  (* iprint code *)
  | CallProc ("iprint", [ arg ]) ->
      trans_exp arg nest env ^ "\tpopq  %rsi\n" ^ "\tleaq IO(%rip), %rdi\n"
      ^ "\tmovq $0, %rax\n" ^ "\tcall printf\n"
  (* sprint code *)
  | CallProc ("sprint", [ StrExp s ]) ->
      let l = incLabel () in
      "\t.data\n"
      ^ sprintf "L%d:\t.string %s\n" l s
      ^ "\t.text\n"
      ^ sprintf "\tleaq L%d(%%rip), %%rdi\n" l
      ^ "\tmovq $0, %rax\n" ^ "\tcall printf\n"
  (* scan code *)
  | CallProc ("scan", [ VarExp v ]) ->
      trans_var v nest env ^ "\tmovq %rax, %rsi\n" ^ "\tleaq IO(%rip), %rdi\n"
      ^ "\tmovq $0, %rax\n" ^ "\tcall scanf\n"
  (* return code *)
  | CallProc ("return", [ arg ]) -> trans_exp arg nest env ^ "\tpopq %rax\n"
  | CallProc ("new", [ VarExp v ]) ->
      let size = calc_size (type_var v env) in
      sprintf "\tmovq $%d, %%rdi\n" size
      ^ "\tcall malloc\n" ^ "\tpushq %rax\n" ^ trans_var v nest env
      ^ "\tpopq (%rax)\n"
  (* Procedure call code *)
  | CallProc (s, el) -> (
      let entry = env s in
      match entry with
      | FunEntry { formals = _; result = _; level } ->
          (* Actual arguments code *)
          (* Align to 16-byte boundary *)
          (if List.length el mod 2 = 1 then "" else "\tpushq $0\n")
          ^ List.fold_right
              (fun ast code -> code ^ trans_exp ast nest env)
              el ""
          (* Code to pass static link *)
          ^ passLink nest level (* Function call code *)
          ^ "\tcallq " ^ s ^ "\n" (* Pop pushed arguments + static link *)
          ^ sprintf "\taddq $%d, %%rsp\n" ((List.length el + 1 + 1) / 2 * 2 * 8)
      | _ -> raise (No_such_symbol s))
  (* Block code: statement blocks ignore function definitions *)
  | Block (dl, sl) ->
      (* Process declarations in block *)
      let tenv', env', addr' = type_decs dl nest tenv env in
      List.iter (fun d -> trans_dec d nest tenv' env') dl;
      (* Extend frame *)
      let ex_frame = sprintf "\tsubq $%d, %%rsp\n" ((-addr' + 16) / 16 * 16) in
      (* Generate code for body (statement list) *)
      let code =
        List.fold_left
          (fun code ast -> code ^ trans_stmt ast nest tenv' env')
          "" sl
        (* Add frame extension for local variables *)
      in
      ex_frame ^ code
  (* If statement without else *)
  | If (e, s, None) ->
      let condCode, l = trans_cond e nest env in
      condCode ^ trans_stmt s nest tenv env ^ sprintf "L%d:\n" l
  (* If statement with else *)
  | If (e, s1, Some s2) ->
      let condCode, l1 = trans_cond e nest env in
      let l2 = incLabel () in
      condCode
      ^ trans_stmt s1 nest tenv env
      ^ sprintf "\tjmp L%d\n" l2 ^ sprintf "L%d:\n" l1
      ^ trans_stmt s2 nest tenv env
      ^ sprintf "L%d:\n" l2
  (* While statement *)
  | While (e, s) ->
      let condCode, l1 = trans_cond e nest env in
      let l2 = incLabel () in
      sprintf "L%d:\n" l2 ^ condCode ^ trans_stmt s nest tenv env
      ^ sprintf "\tjmp L%d\n" l2 ^ sprintf "L%d:\n" l1
  (* Empty statement *)
  | NilStmt -> ""

(* Variable address processing *)
and trans_var ast nest env =
  match ast with
  | Var s -> (
      let entry = env s in
      match entry with
      | VarEntry { offset; level; ty = _ } ->
          "\tmovq %rbp, %rax\n"
          ^ nCopyStr (nest - level) "\tmovq 16(%rax), %rax\n"
          ^ sprintf "\tleaq %d(%%rax), %%rax\n" offset
      | _ -> raise (No_such_symbol s))
  | IndexedVar (v, size) ->
      trans_exp (CallFunc ("*", [ IntExp 8; size ])) nest env
      ^ trans_var v nest env ^ "\tmovq (%rax), %rax\n" ^ "\tpopq %rbx\n"
      ^ "\tleaq (%rax,%rbx), %rax\n"

(* Expression processing *)
and trans_exp ast nest env =
  match ast with
  (* Integer constant *)
  | IntExp i -> sprintf "\tpushq $%d\n" i
  (* Variable reference: get reference frame with trans_var *)
  | VarExp v ->
      trans_var v nest env ^ "\tmovq (%rax), %rax\n" ^ "\tpushq %rax\n"
  (* Addition *)
  | CallFunc ("+", [ left; right ]) ->
      trans_exp left nest env ^ trans_exp right nest env ^ "\tpopq %rax\n"
      ^ "\taddq %rax, (%rsp)\n"
  (* Subtraction *)
  | CallFunc ("-", [ left; right ]) ->
      trans_exp left nest env ^ trans_exp right nest env ^ "\tpopq %rax\n"
      ^ "\tsubq %rax, (%rsp)\n"
  (* Multiplication *)
  | CallFunc ("*", [ left; right ]) ->
      trans_exp left nest env ^ trans_exp right nest env ^ "\tpopq %rax\n"
      ^ "\timulq (%rsp), %rax\n" ^ "\tmovq %rax, (%rsp)\n"
  (* Division *)
  | CallFunc ("/", [ left; right ]) ->
      trans_exp left nest env ^ trans_exp right nest env ^ "\tpopq %rbx\n"
      ^ "\tpopq %rax\n" ^ "\tcqto\n" ^ "\tidivq %rbx\n" ^ "\tpushq %rax\n"
  (* Negation *)
  | CallFunc ("!", arg :: _) -> trans_exp arg nest env ^ "\tnegq (%rsp)\n"
  (* Function call: return value is in %rax *)
  | CallFunc (s, el) ->
      trans_stmt (CallProc (s, el)) nest initTable env ^ "\tpushq %rax\n"
  | _ -> raise (Err "internal error")

(* Relational operation processing *)
and trans_cond ast nest env =
  match ast with
  | CallFunc (op, left :: right :: _) -> (
      let code =
        (* Operand code *)
        trans_exp left nest env ^ trans_exp right nest env
        (* Load operand values into %rax, %rbx *)
        ^ "\tpopq %rax\n"
        ^ "\tpopq %rbx\n"
        (* cmp instruction *)
        ^ "\tcmpq %rax, %rbx\n"
      in
      let l = incLabel () in
      match op with
      (* Condition and branch relation is inverted *)
      | "==" -> (code ^ sprintf "\tjne L%d\n" l, l)
      | "!=" -> (code ^ sprintf "\tje L%d\n" l, l)
      | ">" -> (code ^ sprintf "\tjle L%d\n" l, l)
      | "<" -> (code ^ sprintf "\tjge L%d\n" l, l)
      | ">=" -> (code ^ sprintf "\tjl L%d\n" l, l)
      | "<=" -> (code ^ sprintf "\tjg L%d\n" l, l)
      | _ -> ("", 0))
  | _ -> raise (Err "internal error")

(* Generate entire program *)
let trans_prog ast =
  let code = trans_stmt ast 0 initTable initTable in
  io ^ header ^ code ^ "\txorq %rax, %rax\n" ^ epilogue ^ !output
