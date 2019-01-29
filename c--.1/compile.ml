
(* Ecrit par Mohamed Hadjoudj *)


open Cparse
open Genlab

type instruction = 
    Comment of string
  | Ncomment of string
  | Mov of arg * arg
  | Push of arg
  | Pop of arg
  | Inc of arg
  | Dec of arg
  | Neg of arg
  | Not of arg
  | Lea of arg * arg
  | Add of arg * arg
  | Sub of arg * arg
  | Mul of arg * arg
  | Div of arg
  | Cmp of arg * arg
  | Label of string 
  | SubLabel of int 
  | Jmp of int 
  | Jle of int
  | Jl of int
  | Je of int
  | Jne of int
  | Call of string
  | CallExt of string
  | Ret
and arg = 
  | Reg of register
  | Str of string
  | Cst of int
  | GlobalVar of int
  | InternalVar of string
  | ExtVar of string
  | Offset of int * register
  | ArrayEl of register * register
and register = Rax | Rbx | Rcx | Rdx | Rsi | Rdi | Rbp | Rsp | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 | Rip
and program = instruction list

let rec getIndex e = function
    [] -> None
  | t::q -> if e = t then Some 0 
            else (match (getIndex e q) with None -> None | Some i -> Some(i+1))

(* Générateur d'identifiants pour les labels. *)
let uniqI_ = ref 0
let uniqI () = uniqI_ := !uniqI_ + 1; !uniqI_


(* Renvoi la liste des variables utilisées dans un code. *)
let rec var_used = function
    CBLOCK (_, loccodelist) ->  List.flatten (List.map (fun (_, a) -> var_used a) loccodelist)
  | CIF((_,expr), (_,code1), (_,code2)) -> (var_used_expr expr) @ (var_used code1) @ (var_used code2)
  | CWHILE((_,expr), (_,code)) -> (var_used_expr expr) @ (var_used code)
  | CEXPR(_, expr) -> var_used_expr expr
  | CRETURN(Some (_,expr)) -> var_used_expr expr
  | CTHROW (_, (_,expr)) -> var_used_expr expr
  | CTRY((_,code), l, finally) -> 
      let l' = match finally with
          None -> code::(List.map (fun (_,_,(_, a)) -> a) l)
        | Some(_,code2) ->code2::code::(List.map (fun (_,_,(_, a)) -> a) l) in
      List.flatten (List.map var_used l')
  | _ -> [] 

(* Renvoi la liste des variables utilisées dans une expression. *)
and var_used_expr = function
  | VAR s -> [s]
  | CST _ | STRING _ -> []
  | SET_VAR(s, (_,expr)) -> s :: (var_used_expr expr)
  | SET_ARRAY(s, (_,expr1), (_,expr2)) -> s :: (var_used_expr expr1) @ (var_used_expr expr2)
  | CALL (s, l) -> List.flatten (List.map (fun (_, a) -> var_used_expr a) l)
  | OP1 (_, (_, expr)) -> var_used_expr expr
  | OP2 (_, (_, expr1), (_, expr2)) -> (var_used_expr expr1) @ (var_used_expr expr2)
  | CMP (_, (_, expr1), (_, expr2)) -> (var_used_expr expr1) @ (var_used_expr expr2)
  | EIF (e1, e2, e3) -> List.flatten (List.map (fun (_, a) -> var_used_expr a) [e1; e2; e3]) 
  | ESEQ l -> List.flatten (List.map (fun (_, a) -> var_used_expr a) l)

(* Vérifie si la variable est utilisée dans le code. *)
let rec var_is_used name code = (getIndex name (var_used code)) != None

(* Convertie un programme (liste d'instructions) en assembleur. *)
let asm_of_program l = 
  let strings = ref [] in
  let global_vars = ref [] in
  let internal_vars = ref [] in
  let rec aux = function
      Comment(s)::Comment(s')::q -> (aux (Comment (if s <> s' then s^"; "^s' else s)::q))
    | Comment(s)::q -> " # "^s^(aux q)
    | Ncomment(s)::q -> " \n# "^s^(aux q)
    | Mov(x1, x2)::q -> " \n\tmovq "^(string_of_arg x1)^", "^(string_of_arg x2)^(aux q)
    | Push(x)::q -> " \n\tpushq "^(string_of_arg x)^(aux q)
    | Pop(x)::q -> " \n\tpopq "^(string_of_arg x)^(aux q)
    | Inc(x)::q -> " \n\tinc "^(string_of_arg x)^(aux q)
    | Dec(x)::q -> " \n\tdec "^(string_of_arg x)^(aux q)
    | Neg(x)::q -> " \n\tnegq "^(string_of_arg x)^(aux q)
    | Not(x)::q -> " \n\tnotq "^(string_of_arg x)^(aux q)
    | Lea(x1, x2)::q -> " \n\tleaq "^(string_of_arg x1)^", "^(string_of_arg x2)^(aux q)
    | Add(x1, x2)::q -> " \n\taddq "^(string_of_arg x1)^", "^(string_of_arg x2)^(aux q)
    | Sub(x1, x2)::q -> " \n\tsubq "^(string_of_arg x1)^", "^(string_of_arg x2)^(aux q)
    | Mul(x1, x2)::q -> " \n\timulq "^(string_of_arg x1)^", "^(string_of_arg x2)^(aux q)
    | Div(x)::q -> " \n\tcqto\n\tidivq "^(string_of_arg x)^(aux q)
    | Cmp(x1, x2)::q -> " \n\tcmpq "^(string_of_arg x1)^", "^(string_of_arg x2)^(aux q)
    | Label(s)::q -> " \n"^s^": "^(aux q)
    | SubLabel(i)::q -> " \n.L"^(string_of_int i)^": "^(aux q)
    | Jmp(i)::q -> " \n\tjmp "^".L"^(string_of_int i)^(aux q)
    | Jle(i)::q -> " \n\tjle "^".L"^(string_of_int i)^(aux q)
    | Jl(i)::q -> " \n\tjl "^".L"^(string_of_int i)^(aux q)
    | Je(i)::q -> " \n\tje "^".L"^(string_of_int i)^(aux q)
    | Jne(i)::q -> " \n\tjne "^".L"^(string_of_int i)^(aux q)
    | Call(s)::q -> " \n\tmovq $0, %rax\n\t.align 16 \n\tcallq "^s^(aux q)
    | CallExt(s)::q -> 
        "\n\tmovq $0, %rax\n\t.align 16 \n\tcallq "^s
        ^(match s with "malloc" | "calloc" | "realloc" -> "" | _ ->"\n\tmovslq %eax, %rax")
        ^(aux (Comment("fonction externe")::q))
    | Ret::q -> " \n\tret"^(aux q)
    | [] -> " \n \n# MCC Mohamed HADJOUDJ 2019 \n"
  and string_of_arg = function
      Reg(r) -> string_of_reg r
    | Str(s) -> let i = match getIndex s (List.rev !strings) with None -> strings:=s::!strings; List.length !strings | Some i -> i+1 in
      ".S"^(string_of_int i)^(string_of_arg (Offset(0, Rip)))
    | Cst(i) -> "$"^(string_of_int i)
    | GlobalVar(i) -> if (getIndex i !global_vars) = None then global_vars:=i::!global_vars;
      ".V"^(string_of_int i)^(string_of_arg (Offset(0, Rip)))
    | InternalVar(s) -> if (getIndex s !internal_vars) = None then internal_vars:=s::!internal_vars;
      ".MCC_"^s^(string_of_arg (Offset(0, Rip)))
    | ExtVar(s) -> s^(string_of_arg (Offset(0, Rip)))
    | Offset(i, r) -> (if i != 0 then string_of_int i else "")^"("^(string_of_reg r)^")"
    | ArrayEl(r1, r2) -> "("^(string_of_reg r1)^", "^(string_of_reg r2)^", 8)"
  and string_of_reg = function
      Rax -> "%rax"
    | Rbx  -> "%rbx"
    | Rcx  -> "%rcx"
    | Rdx  -> "%rdx"
    | Rsi  -> "%rsi"
    | Rdi  -> "%rdi"
    | Rbp  -> "%rbp"
    | Rsp  -> "%rsp"
    | R8  -> "%r8"
    | R9  -> "%r9"
    | R10  -> "%r10"
    | R11  -> "%r11"
    | R12  -> "%r12"
    | R13  -> "%r13"
    | R14  -> "%r14"
    | R15 -> "%r15"
    | Rip -> "%rip"
  in let s = aux l in
  
      ".data\n"^(String.concat "\n" (List.rev_map (fun i -> "\t.V"^(string_of_int i)^": .space 8") !global_vars))^ 
      (String.concat "\n" (List.rev_map (fun s -> "\t.MCC_"^s^": .zero 8") !internal_vars))^ 
      "\n.text \n.globl main \n"^s^(if !strings != [] then "\n.section .rodata\n\t.align 8\n"^(
      String.concat "\n" (List.mapi (fun i s -> ".S"^(string_of_int (i+1))^":\n\t.string \""^(String.escaped  s)^"\"") (List.rev !strings))
    ) else "")



(* Simplifie le code ne faisant intervenir aucune variable. *)
let rec optimise_code (loc0, code0) = match code0 with
    CBLOCK (vars, l) ->  loc0, CBLOCK (vars, List.map optimise_code l)
  | CIF(e, c1, c2) -> begin match (optimise_expr e) with (_, CST x) -> optimise_code (if x=0 then c2 else c1) 
    | e' -> loc0, CIF(e', c1, c2) end
  | CWHILE(e, c) -> begin match (optimise_expr e) with (loc, CST x) -> (if x=0 then loc0, CBLOCK([],[]) else loc0, CWHILE((loc, CST 1), c)) 
    | e' -> loc0, CWHILE(e', c) end
  | CEXPR(e) -> loc0, CEXPR(optimise_expr e)
  | CRETURN(Some e) -> loc0, CRETURN(Some(optimise_expr e))
  | CRETURN(None) -> loc0, CRETURN(None)
  | CTHROW (s, e) -> loc0, CTHROW(s, optimise_expr e)
  | CTRY(code, l, Some finally) -> loc0, CTRY(optimise_code code, List.map (fun (a,b,c) -> (a,b,optimise_code c)) l, Some (optimise_code finally))
  | CTRY(code, l, None) -> loc0, CTRY(optimise_code code, List.map (fun (a,b,c) -> (a,b,optimise_code c)) l, None)

(* Calcul les expressions ne faisant intervenir aucune variable. *)
and optimise_expr (loc, expr) = match expr with
  | OP1 (op, e) -> (match (optimise_expr e), op with (_, CST x),M_MINUS -> (loc, CST (-x)) | e',_ -> loc, OP1 (op, e'))
  | OP2 (op, e1, e2) -> let e1', e2' = (optimise_expr e1),(optimise_expr e2) in 
      begin match e1', e2' with 
        (_, CST x1), (_, CST x2) -> begin match op with
            S_MUL -> loc, CST (x1*x2)
          | S_DIV -> loc, CST (x1/x2)
          | S_MOD -> loc, CST (x1 mod x2)
          | S_ADD -> loc, CST (x1+x2)
          | S_SUB -> loc, CST (x1-x2)
          | _ -> loc, OP2 (op, e1', e2')
        end 
      | (_, STRING s), (_, CST x) -> if op = S_INDEX then loc, STRING (String.sub s x 1) else loc, OP2 (op, e1', e2')
      | _ -> loc, OP2 (op, e1', e2')
    end
  | CMP (op, e1, e2) -> begin match (optimise_expr e1),(optimise_expr e2) with
        (_, CST x1), (_, CST x2) -> if (C_LT = op && x1 < x2) || (C_LE = op && x1 <= x2) || (C_EQ = op && x1 == x2) then loc, (CST 1) else loc, (CST 0)
      | e1', e2' -> loc, CMP (op, e1', e2')
    end
  | EIF (e1, e2, e3) -> (match (optimise_expr e1) with (_, CST x) -> if x!=0 then optimise_expr e2 else optimise_expr e3 
    | e1' -> let e2', e3' = optimise_expr e2, optimise_expr e3 in loc, EIF (e1', e2', e3'))
  | SET_VAR(s,e) -> loc, SET_VAR(s, (optimise_expr e))
  | SET_ARRAY(s, e1, e2) -> loc, SET_ARRAY(s, (optimise_expr e1), (optimise_expr e2))
  | CALL(s, l) -> loc, CALL(s, List.map optimise_expr l)
  | ESEQ (l) -> loc, ESEQ (List.map optimise_expr l)
  | _ -> (loc, expr)


let compile out declarations = 
  (* Registres pour les six premiers arguments d'une fonction. *)
  let args_reg = [|Rdi; Rsi; Rdx; Rcx; R8; R9|] in
  (* Liste des variables locales. *)
  let local_vars = ref [] in
  (* Nom de la fonction en train d'être compilée. *)
  let current_function = ref "" in
  (* Liste des noms d'exceptions. *)
  let exceptions = ref [] in
  (* Liste du label de chaque exception. *)
  let exceptions_labels = ref [] in
  (* Pile des try emboitées. (try = (id, id de chaque exception)) *)
  let tryStack = ref [] in
  (* Liste des variables globales. On vérifie que chaque variable est utilisée au moins une fois. *)
  let global_vars = List.fold_left (fun a b -> match b with 
      CDECL (loc,s) -> 
        if not (List.exists (fun dec -> match dec with CDECL _ -> false | CFUN(_,_,_,(_,code)) -> var_is_used s code) declarations) 
        then Error.warning (Some loc) ("Variable inutilisée: \""^s^"\".");
        s::a
    | _ -> a) ["NULL"] declarations in
  (* Liste des fonctions. On vérife qu'aucune fonction n'a été définie plusieurs fois. *)
  let funs = List.fold_left (fun a b -> match b with 
      CFUN (loc,s,vars,_) -> if (List.exists (fun (a,b) -> a=s) a) then Error.fatal (Some loc) ("Redéclaration de la fonction \""^s^"\".");
        (s, List.length vars)::a 
    | _ -> a) [] declarations in
  

  (* Compile du code en un programme. *)
  let rec compile_code (loc0, code0) = match code0 with  
      CBLOCK (vars, l) -> let t1 = (declare_vars vars) in 
        let t2 = List.flatten (List.map 
          (fun a -> let (_,l1,c1,l2,c2),_= a in (compile_code a)@[Comment (Printf.sprintf "%d:%d -> %d:%d" l1 c1 l2 c2)])
         l) in
        t1 @ t2 @ (destroy_vars (List.length vars)) 
    | CIF(e, c1, c2) -> let i = uniqI () in let j = uniqI () in
      let t1 = compile_expr e in let t2 = compile_code c1 in let t3 = compile_code c2 in
      t1@[
        Cmp (Cst 0, Reg Rax);
        Je i;
      ]@t2@[
        Jmp j;
        SubLabel i;
      ]@t3@[
        SubLabel j;
      ]
    | CWHILE(e, c) -> let i = uniqI () in let j = uniqI () in 
      let t1 = compile_expr e in let t2 = compile_code c in
      [
        SubLabel i;
      ]@t1@[
        Cmp (Cst 0, Reg Rax);
        Je j;
      ]@t2@[
        Jmp i;
        SubLabel j;
      ]
    | CEXPR(e) -> compile_expr e;
    | CRETURN(Some e) -> let t = compile_expr e in t @ (compile_code (loc0,CRETURN(None)))
    | CRETURN(None) -> (if !current_function = "main" then [] else [
      Mov (Reg Rax, Reg Rbx);
      Mov (Cst 0, Reg Rax);
    ]) @ (match !tryStack with [] -> [] | (tryId, _)::q -> [
      Mov (Cst 1, InternalVar "RET_FINALLY");
      Mov (Reg Rax, InternalVar "RET_FINALLY_RAX");
      Mov (Reg Rbx, InternalVar "RET_FINALLY_RBX");
      Mov (Reg Rcx, InternalVar "RET_FINALLY_RCX");
      Jmp tryId;
    ]) @ [
      Mov (Reg Rbp, Reg Rsp);
      Pop (Reg Rbp);
      Ret;
    ]
    | CTHROW (s, e) -> begin match getIndex s (List.rev !exceptions) with
          None -> fail_excep ()
        | Some exId -> let q = if List.mem exId (List.flatten (List.map (fun (_, exList) -> exList) !tryStack))
          then [Jmp (List.nth (List.rev !exceptions_labels) exId);]
          else compile_code (loc0, CRETURN None) in let ret = (compile_expr e)@[
            Push (Reg Rax);
            Mov (Cst 1, Reg Rax); Comment "On lève une exception.";
            Mov (Cst exId, Reg Rbx); Comment "On enregistre l'identifiant de l'exception.";
            Pop (Reg Rcx); Comment "Valeur de la variable renvoyée."
          ]@q in 
          local_vars := []; ret
      end
    | CTRY(code, l, None) -> compile_code (loc0, CTRY(code, l, Some (loc0, CBLOCK ([], [])))) 
    | CTRY(code, l, Some finally) -> 
        let i  = uniqI () in
        tryStack := (i, List.map (fun (excep, _, _) -> match getIndex excep (List.rev !exceptions) with
            Some i -> i
          | None -> let i = List.length !exceptions in
          exceptions:=excep::!exceptions; exceptions_labels:= (uniqI ())::!exceptions_labels; i
        ) l)::!tryStack;
        let t = List.flatten (List.map (
        fun (excep, var, code) -> let exId, exLab = match getIndex excep (List.rev !exceptions) with Some i -> i, (List.nth (List.rev !exceptions_labels) i)
            | None -> (* let exId, exLab = (List.length !exceptions), uniqI () in 
              exceptions:=excep::!exceptions; exceptions_labels:= exLab::!exceptions_labels;
              exId, exLab *) failwith "CTRY";
        in
        let (loc, _) = code in local_vars := var::!local_vars; let p,v,q = addr_of_expr (loc, VAR var) in
          [SubLabel exLab; Comment ("catch "^excep)]@
          (declare_vars [CDECL (loc,var)])@p@[Mov (Reg Rcx, v)]@q@
          (compile_code code)@[Jmp i]
      ) l) in
      let p = [Ncomment "Début de try"]@(compile_code code)@[Jmp i; Comment "Pas d'exception"]@t@[SubLabel i; Comment "Fin de try"] in
      let id = uniqI () in
      tryStack := List.tl !tryStack;
      p @ (compile_code finally) @ [
        Cmp (Cst 0, InternalVar "RET_FINALLY");
        Je id;
        Mov (InternalVar "RET_FINALLY_RAX", Reg Rax);
        Mov (InternalVar "RET_FINALLY_RBX", Reg Rbx);
        Mov (InternalVar "RET_FINALLY_RCX", Reg Rcx);
      ]@(match !tryStack with [] -> [] | (tryId, _)::q -> [
        Mov (Cst 1, InternalVar "RET_FINALLY");
        Mov (Reg Rax, InternalVar "RET_FINALLY_RAX");
        Mov (Reg Rbx, InternalVar "RET_FINALLY_RBX");
        Mov (Reg Rcx, InternalVar "RET_FINALLY_RCX");
        Jmp tryId;
      ]) @ [
        Mov (Reg Rbp, Reg Rsp);
        Pop (Reg Rbp);
        Ret;
      ]@[
        Ret;
        SubLabel id;
      ]
  and compile_expr (loc0, expr0) = match expr0 with
    | VAR(s) -> let p, v, q = addr_of_expr (loc0, expr0) in p@[Mov (v, Reg Rax)]@q
    | CST(i) -> [Mov (Cst i, Reg Rax)]
    | STRING(s) -> [Lea (Str s, Reg Rax)]
    | SET_VAR(s,e) -> let p, v, q = addr_of_expr (loc0, VAR s) in p@(compile_expr e)@[Mov(Reg Rax, v)]@q
    | SET_ARRAY(s,e1,e2) -> let p, v, q = addr_of_expr (loc0, OP2(S_INDEX, (loc0, VAR s), e1)) in p@(compile_expr e2)@[Mov(Reg Rax, v)]@q
    | CALL(s, el) -> let is_ext = (match List.find_all (fun (a,b) -> a=s) funs with
        (i,j)::_-> if List.length el != j then Error.fatal (Some(loc0)) "Mauvais nombre d'arguments."; false
      | [] -> true) in
      let i = min 6 (List.length el) in
      (List.flatten (List.map (fun e -> (compile_expr e)@[Push (Reg Rax)]) (List.rev el))) @
      (List.mapi (fun i _ -> Pop (Reg args_reg.(i))) (Array.to_list (Array.make i 0)))
      @(if is_ext then [CallExt s] else let i,j = uniqI(),uniqI() in [
        Call s;
        Cmp (Cst 0, Reg Rax); Comment ("On vérifie si une exception a été lancée. ");
        Jne i;
        Mov (Reg Rbx, Reg Rax); Comment ("Si ce n'est pas le cas on place le résultat (en %rbx) dans %rax. ");
        Jmp j;
        SubLabel i;
        Ncomment ("Sinon on cherche l'exception correspondante. ");
      ] @ (search_exception ()) @[
          SubLabel j;
        ])
    | OP1(op, e) -> begin match op with
          M_MINUS -> (compile_expr e)@[Neg (Reg Rax)]
        | M_NOT -> (compile_expr e)@[Not (Reg Rax)]
        | M_POST_INC -> let p, v, q = addr_of_expr e in p@[Push v; Mov (v, Reg Rax); Inc (Reg Rax); Mov (Reg Rax, v); Pop (Reg Rax)]@q
        | M_POST_DEC -> let p, v, q = addr_of_expr e in p@[Push v; Mov (v, Reg Rax); Dec (Reg Rax); Mov (Reg Rax, v); Pop (Reg Rax)]@q
        | M_PRE_INC -> let p, v, q = addr_of_expr e in p@[Mov(v, Reg Rax); Inc(Reg Rax); Mov(Reg Rax, v)]@q
        | M_PRE_DEC -> let p, v, q = addr_of_expr e in p@[Mov(v, Reg Rax); Dec(Reg Rax); Mov(Reg Rax, v)]@q
      end
    | OP2(op, e1, e2) -> begin match op with 
        S_MUL -> let t1 = compile_expr e1 in let t2 = compile_expr e2 in 
        t1@[
          Push (Reg R12);
          Mov (Reg Rax, Reg R12)
        ]@t2@ [
          Mul (Reg R12, Reg Rax);
          Pop (Reg R12);
        ]
      | S_DIV -> let t1 = compile_expr e1 in let t2 = compile_expr e2 in
        t2@[
          Push (Reg R12);
          Mov (Reg Rax, Reg R12)
        ]@t1@ [
          Push (Reg Rdx);
          Div (Reg R12);
          Pop (Reg Rdx);
          Pop (Reg R12);
        ]
      | S_MOD -> let t1 = compile_expr e1 in let t2 = compile_expr e2 in
        t2@[
          Push (Reg R12);
          Mov (Reg Rax, Reg R12)
        ]@t1@ [
          Push (Reg Rdx);
          Div (Reg R12);
          Mov (Reg Rdx, Reg Rax);
          Pop (Reg Rdx);
          Pop (Reg R12);
        ]
      | S_ADD -> let t1 = compile_expr e1 in let t2 = compile_expr e2 in
        t1@[
          Push (Reg R12);
          Mov (Reg Rax, Reg R12);
        ]@t2@ [
          Add (Reg R12, Reg Rax);
          Pop (Reg R12);
        ]
      | S_SUB -> let t1 = compile_expr e2 in let t2 = compile_expr e1 in
        t1@[
          Push (Reg R12);
          Mov (Reg Rax, Reg R12);
        ]@t2@ [
          Sub (Reg R12, Reg Rax);
          Pop (Reg R12);
        ]
      | S_INDEX -> let p,v,q = addr_of_expr (loc0, expr0) in p@ [
          Mov (v, Reg Rax);
        ]@q
      end
    | CMP(op, e1, e2) -> let t1 = compile_expr e1 in let t2 = compile_expr e2 in let i = uniqI () in let j = uniqI () in 
      t1@[
        Push (Reg R12);
        Mov (Reg Rax, Reg R12);
      ]@t2@[
        Cmp (Reg Rax, Reg R12);
        (match op with C_LT -> Jl i | C_LE -> Jle i| C_EQ -> Je i);
        Mov (Cst 0, Reg Rax);
        Jmp j;
        SubLabel i;
        Mov (Cst 1, Reg Rax);
        SubLabel j;
        Pop (Reg R12);
      ]
    | EIF (e1, e2, e3) -> compile_code (loc0, CIF(e1, (loc0, CEXPR e2), (loc0, CEXPR e3)))
    | ESEQ(el) -> List.flatten (List.map compile_expr el)
  and declare_vars vars = let n = List.length vars in if n < 1 then [] else
    [
      Sub (Cst (8*n), Reg Rsp);
      Comment ("var "^(String.concat ", " (List.map 
      (fun a -> match a with CDECL(loc,s) -> 
        if s = "NULL" then Error.fatal (Some loc) ("NULL est un mot clé réservé.");
        local_vars:=s::!local_vars; s | _ -> "") 
      vars
      )))
    ]
  and destroy_vars n = for i = 1 to (min n (List.length !local_vars)) do local_vars := List.tl !local_vars; done; if n<1 then [] else [
        Add(Cst (n*8), Reg Rsp)
      ]
  and declare_args args = 
    local_vars := [];
    (declare_vars args)@(List.flatten (List.mapi (fun i a -> match a with
        CDECL(loc,s) -> if i < 6 then [Mov (Reg (args_reg.(i)), Offset(-8*(i+1), Rbp))]
      else [
        Mov (Offset(8*(i-4), Rbp), Reg Rax);
        Mov (Reg Rax, (Offset(-8*(i+1), Rbp)));
      ]
      | _ -> []
    ) args ))
  and fail_excep () = [
      Lea (Str "Erreur: Exception non rattrapée !\n\n", Reg Rdi);
      CallExt "printf";
    ]@(compile_code (("",0,0,0,0), CRETURN None))
  and search_exception () = (List.flatten (List.map (fun (tryid, exl) -> 
    (List.flatten (List.map (fun exId -> [
      Cmp (Cst exId, Reg Rbx); Comment ("Est-ce l'exception \""^(List.nth (List.rev !exceptions) exId)^"\" ?");
      Je (List.nth (List.rev !exceptions_labels) exId);
    ]) exl))) !tryStack))@(if !current_function = "main" then fail_excep ()
    else [
      Ncomment "Exception non rattrapée, on la transmet à l'appelant."
    ]@(compile_code (("",0,0,0,0), CRETURN None)))
  and addr_of_expr (loc0, expr0) = match expr0 with VAR s when s!="NULL"-> 
      begin match getIndex s !local_vars with
          Some i -> [],Offset(-(8*((List.length !local_vars)-i)), Rbp),[]
        | None -> (match getIndex s global_vars with
            Some i -> [],GlobalVar i,[]
          | None -> [],ExtVar s,[])
      end
      | OP2(S_INDEX, e1, e2) -> let t1 = compile_expr e1 in let t2 = (compile_expr e2) in t1@[

          Push (Reg R12);
          Mov (Reg Rax, Reg R12)
        ]@t2@[
            Push (Reg R13);
            Mov (Reg Rax, Reg R13)
          ], ArrayEl (R12, R13), [
            Pop (Reg R13);
            Pop (Reg R12);
          ]
      | _ -> Error.fatal (Some(loc0)) "Impossible d'acceder à l'addresse de l'expression.";
  in
  Printf.fprintf out "%s\n" (asm_of_program (
    List.fold_left (fun a b -> match b with 
      CFUN(loc,s,args,code) -> local_vars:=[];current_function:=s; let code' = optimise_code code in let t1 = (declare_args args) in
      let t2 = (compile_code code') in
        [
          Label s;
          Push (Reg Rbp);
          Mov (Reg Rsp, Reg Rbp)
        ]@t1@t2@
        (* On rajoute un return au cas où il n'y en avait pas. *)
        (compile_code (loc,CRETURN(None)))
        @a
    | _ -> a
    ) [] declarations 
      
  ))



  
