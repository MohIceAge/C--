
(* Ecrit par Mohamed Hadjoudj *)


open Cparse
open Genlab

(* trouve le plus petit index de l'élément e dans l *)
let rec find_i e =
  let rec aux i = function 
    | [] -> None
    | a::q -> if a=e then Some(i) else aux (i+1) q
  in aux 0;;

let args_reg = [|"%rdi";"%rsi";"%rdx";"%rcx";"%r8";"%r9"|];;
let join = String.concat "";;
let compile out = 
  let global_vars = ref ["NULL"] and local_vars = ref [] and funs = ref [] in
  let strings = ref [] in
  let uniqid_ = ref 0 in
  let uniqid () = uniqid_ := !uniqid_+1; string_of_int !uniqid_ in
  let var_reg s = (match (find_i s !local_vars) with
        None -> (match (find_i s !global_vars) with 
          None -> (Printf.sprintf "%s(%%rip)" s)
          | _ -> (Printf.sprintf ".%s(%%rip)" s))
      | Some(i) -> Printf.sprintf "-%d(%%rbp)" (8*((List.length !local_vars)-i)))
  in let rec declare_args decl =
    local_vars := [];
    if decl = [] then "" else
    let arg = function i when i < 6 -> args_reg.(i)
      | i -> let s = (string_of_int ((i-4)*8))^"(%rbp)" in Printf.sprintf "%s, %%r13\n\tmovq %%r13" s in
    let rec aux i = function
      | CDECL(_, name)::q  -> local_vars := name::!local_vars;
        (Printf.sprintf "\tmovq %s, -%d(%%rbp) # var %s\n" (arg i) (8*(i+1)) name)^(aux (i+1) q);
      | _-> "" in (Printf.sprintf "\tsubq $%d, %%rsp\n" (8*(List.length decl)))^(aux 0 decl)
  and compile_code  = function
      CBLOCK(decl, l) -> List.iter (fun a -> match a with CDECL(_, name) -> local_vars := name::!local_vars | _ -> ()) decl;
        let s = (if decl != [] then (Printf.sprintf "\tsubq $%d, %%rsp#création des variables local (entrée de boucle)\n\n" (8*(List.length decl))) else "") ^
        (join (List.map (fun (_, code) -> compile_code code) l)) ^
        (if decl != [] then (Printf.sprintf "\taddq $%d, %%rsp #supression des variables local (sortie de boucle)\n" (8*(List.length decl))) else "") in
        List.iter (fun a -> match !local_vars with _::lcl -> local_vars := lcl | _ -> ()) decl;
        s
    | CEXPR((_, expr)) -> compile_expr expr
    | CIF((_, expr), (_, code1), (_, code2)) -> let id = uniqid () in
        Printf.sprintf "%s\n\tcmpq $0, %%rax\n\tje .if%s_eb\n%s\n\tjmp .if%s_ee\n.if%s_eb:\n%s\n.if%s_ee:\n" (compile_expr expr) id (compile_code code1) id id (compile_code code2) id
    | CWHILE((_, expr), (_, code)) -> let id = uniqid () in
        Printf.sprintf ".while%s_b:\n%s\n\tcmpq $0, %%rax\n\tje .while%s_e\n%s\n\tjmp .while%s_b\n.while%s_e:\n" id (compile_expr expr) id (compile_code code) id id
    | CRETURN(None) -> "\tmovq %rbp,%rsp\n\tpopq %rbp\n\tret\n"
    | CRETURN(Some((_, expr))) -> (compile_expr expr)^"\tmovq %rbp,%rsp\n\tpopq %rbp\n\tret\n"
  and compile_expr = function
      CST(i) -> Printf.sprintf "\tmovq $%d, %%rax\n" i
    | VAR(s) -> Printf.sprintf "\tmovq %s, %%rax \n" (var_reg s)    | STRING(s) -> strings := s::!strings; Printf.sprintf "\tleaq .ST%d(%%rip), %%rax\n" (List.length !strings)
    | CALL(s, l) -> let s' = (join (List.map (fun (_, expr) -> (compile_expr expr)^"\tpushq %rax\n") (List.rev l))) in
      let i = min 6 (List.length l) in let rec aux = function i when i > 0 -> (aux (i-1))^"\tpopq "^args_reg.(i-1)^"\n" | _ -> s'
      in (aux i)^"\tmovq $0, %rax\n\t.align 16\n\tcallq "^s^"\n"
    | SET_VAR(s, (_, expr)) -> (compile_expr expr)^(Printf.sprintf "\tmovq %%rax, %s\n" (var_reg s))
    | SET_ARRAY(s, (_, expr1), (_, expr2)) -> (compile_expr expr1)^"\tpushq %r9\n\tpushq %r10\n\tmovq %rax, %r9\n"^(compile_expr expr2)^"\tmovq "^(var_reg s)^", %r10\n\tmovq %rax, (%r10, %r9, 8)\n\tpopq %r10\n\tpopq %r9\n"
    | OP1(M_MINUS, (_, expr)) -> (compile_expr expr)^"\tnegq %rax \n"
    | OP1(M_NOT, (_, expr)) -> (compile_expr expr)^"\tnotq %rax \n"
    | OP1(mop, (_, VAR(s))) -> "\tmovq "^(var_reg s)^", %rax\n"^(match mop with
      | M_POST_INC -> "\tpushq %rax\n\tinc %rax\n\tmovq %rax, "^(var_reg s)^"\n\tpopq %rax\n"
      | M_POST_DEC -> "\tpushq %rax\n\tdec %rax\n\tmovq %rax, "^(var_reg s)^"\n\tpopq %rax\n"
      | M_PRE_INC -> "\tinc %rax\n\tmovq %rax, "^(var_reg s)^"\n"
      | M_PRE_DEC -> "\tdec %rax\n\tmovq %rax, "^(var_reg s)^"\n")
    | OP1(mop, (_, OP2(S_INDEX, (_, expr1), (_, expr2)))) -> "\tpushq %r9\n\tpushq %r10\n"^(compile_expr expr1)^"\tmovq %rax, %r9\n"^(compile_expr expr2)^"\tmovq %rax, %r10\n\tmovq (%r9, %r10, 8), %rax\n"^(match mop with
      | M_POST_INC -> "\tpushq %rax\n\tinc %rax\n\tmovq %rax, (%r9, %r10, 8)\n\tpopq %rax\n"
      | M_POST_DEC -> "\tpushq %rax\n\tdec %rax\n\tmovq %rax, (%r9, %r10, 8)\n\tpopq %rax\n"
      | M_PRE_INC ->  "\tinc %rax\n\tmovq %rax, (%r9, %r10, 8)\n"
      | M_PRE_DEC ->  "\tdec %rax\n\tmovq %rax, (%r9, %r10, 8)\n")^"\tpopq %r10\n\tpopq %r9\n"
    | OP2(bop, (_, expr1), (_, expr2)) -> "\tpushq %r9\n"^(match bop with
        S_MUL -> (compile_expr expr1)^"\tpushq %rax\n"^(compile_expr expr2)^"\tpopq %r9\n\timulq %r9,%rax\n"
      | S_DIV -> (compile_expr expr2)^"\tpushq %rax\n"^(compile_expr expr1)^"\tpopq %r9\n\tcqto\n\tpushq %rdx\n\tidivq %r9\n\tpopq %rdx\n"
      | S_MOD -> (compile_expr expr2)^"\tpushq %rax\n"^(compile_expr expr1)^"\tpopq %r9\n\tcqto\n\tpushq %rdx\n\tidivq %r9\n\tmovq %rdx, %rax\n\tpopq %rdx\n"
      | S_ADD -> (compile_expr expr1)^"\tpushq %rax\n"^(compile_expr expr2)^"\tpopq %r9\n\taddq %r9,%rax\n"
      | S_SUB -> (compile_expr expr2)^"\tpushq %rax\n"^(compile_expr expr1)^"\tpopq %r9\n\tsubq %r9,%rax\n"
      | S_INDEX -> (compile_expr expr1)^"\tpushq %rax\n"^(compile_expr expr2)^"\tpopq %r9\n\tpushq %r10\n\tmovq %rax,%r10\n\tmovq (%r9, %r10, 8),%rax \n\tpopq %r10\n"
      )^"\tpopq %r9\n"
    | CMP(cop, (erl, expr1), (_, expr2)) -> let id = uniqid () and cmp = (match cop with C_LT -> "jg" | C_LE -> "jge" | C_EQ -> "je") in
      "\tpushq %r10\n"^(compile_expr expr1)^"\tmovq %rax, %r10\n"^(compile_expr expr2)^"\tcmpq %r10, %rax\n\t"^cmp^" .cmp"^id^"b\n\tmovq $0, %rax\n\tjmp .cmp"^id^"e\n.cmp"^id^"b:\n\tmovq $1, %rax\n.cmp"^id^"e:\n"
    | EIF((_, expr0), (_, expr1), (_, expr2)) -> let id = uniqid () in 
        (compile_expr expr0)^"\tcmpq $0, %rax\n\tje .eif"^id^"b\n"^(compile_expr expr1)^"\tjmp .eif"^id^"e\n.eif"^id^"b:\n"^(compile_expr expr2)^".eif"^id^"e:\n"
    | ESEQ(l) -> ""
    | _ -> ""
  in let rec declare_int name = global_vars := name::!global_vars
  and compile_fun name decl code = 
    let s1 = declare_args decl in
    let s2 = compile_code code in
    Printf.sprintf "%s:\n\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n%s\n%s\n" name s1 s2
  in let output () = 
    Printf.fprintf out ".data \n";
    List.iter (fun n -> Printf.fprintf out "\t.%s: .space 8 \n" n;) (!global_vars);
    Printf.fprintf out ".text \n.globl main \n";
    List.iter (fun s -> Printf.fprintf out "%s" s) (List.rev !funs);
    Printf.fprintf out ".section .rodata \n\t.align 8\n";
    List.iteri (fun i s -> Printf.fprintf out ".ST%d:\n\t.string \"%s\"\n" (i+1) s) (List.map (String.escaped) (List.rev !strings));
  in let rec aux = function
    | [] -> output ()
    | CDECL(_, name)::q -> declare_int name; aux q
    | CFUN(_, name, decl, (_, code))::q -> funs := (compile_fun name decl code)::!funs; aux q
  in aux
