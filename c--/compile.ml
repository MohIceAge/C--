open Cparse
open Genlab

let args_reg = [|"%rdi";"%rsi";"%rdx";"%rcx";"%r8";"%r9"|];;
let join = String.concat "";;
let compile out = 
  let global_vars = ref ["NULL"] and funs = ref [] in
  let code = ref "" in
  let strings = ref [] in
  let nbcif = ref 0 in
  let nbeif = ref 0 in
  let nbwhile = ref 0 in
  let rec declare_args decl =
    if decl = [] then "" else
    let arg = function i when i < 6 -> args_reg.(i)
      | i -> let s = (string_of_int ((i-4)*8))^"(%rbp)" in Printf.sprintf "%s, %%r13\n\tmovq %%r13" s in
    let rec aux i = function
      | CDECL(_, name)::q  -> (Printf.sprintf "\tmovq %s, -%d(%%rbp) # int %s\n" (arg i) (8*(i+1)) name)^(aux (i+1) q);
      | _-> "" in (Printf.sprintf "\tsubq $%d, %%rsp\n" (8*(List.length decl)))^(aux 0 decl)
  and compile_code  = function
      CBLOCK(decl, l) -> join (List.map (fun (_, code) -> compile_code code) l)
    | CEXPR((_, expr)) -> compile_expr expr
    | CIF((_, expr), (_, code1), (_, code2)) -> nbcif:=!nbcif+1;
      Printf.sprintf "%s\n\tcmpq $0, %%rax\n\tje .if%d_eb\n%s\n\tjmp .if%d_ee\n.if%d_eb:\n%s\n.if%d_ee:\n" (compile_expr expr) !nbcif (compile_code code1) !nbcif !nbcif (compile_code code2) !nbcif
    | CWHILE((_, expr), (_, code)) -> nbwhile:=!nbwhile+1;
      Printf.sprintf ".while%d_b:\n%s\n\tcmpq $0, %%rax\n\tje .while%d_e\n%s\n.while%d_e:\n" !nbwhile (compile_expr expr) !nbwhile (compile_code code) !nbwhile
    | CRETURN(None) -> ""
    | CRETURN(Some((_, expr))) -> compile_expr expr
  and compile_expr = function
      CST(i) -> Printf.sprintf "\tmovq $%d, %%rax\n" i
    | STRING(s) -> strings := s::!strings; Printf.sprintf "\tleaq ST%d(%%rip), %%rax\n" (List.length !strings)
    | CALL(s, l) -> let s' = (join (List.map (fun (_, expr) -> (compile_expr expr)^"\tpushq %rax\n") l)) in
      let i = min 6 (List.length l) in let rec aux = function i when i > 0 -> (aux (i-1))^"\tpopq "^args_reg.(i-1)^"\n" | _ -> s'
      in (aux i)^"\tmovq $0, %rax\n\t.align 16\n\tcallq "^s^"\n"
    | _ -> ""
  in let rec declare_int name = global_vars := name::!global_vars
  and compile_fun name decl code = 
    let pile = ref (List.map (fun v -> match v with CDECL(_, name) -> name | _ -> failwith "declare_args") decl) in 
    let s1 = declare_args decl in
    let s2 = compile_code code in
    Printf.sprintf "%s:\n\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n%s\n%s\n\tmovq %%rbp,%%rsp\n\tpopq %%rbp\n\tret\n" name s1 s2
  in let output () = 
    Printf.fprintf out ".data \n";
    List.iter (fun n -> Printf.fprintf out "\t.%s: .space 8 \n" n;) (!global_vars);
    Printf.fprintf out ".text \n.globl main \n";
    List.iter (fun s -> Printf.fprintf out "%s" s) (List.rev !funs);
    Printf.fprintf out ".section .data \n";
    List.iteri (fun i s -> Printf.fprintf out "ST%d:\n\t.asciz \"%s\"\n" (i+1) s) (List.map (String.escaped) (List.rev !strings));
  in let rec aux = function
    | [] -> output ()
    | CDECL(_, name)::q -> declare_int name; aux q
    | CFUN(_, name, decl, (_, code))::q -> funs := (compile_fun name decl code)::!funs; aux q
  in aux
