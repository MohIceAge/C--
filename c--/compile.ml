open Cparse
open Genlab


let compile out decl_list =
  let todo s = Printf.fprintf out "#TODO %s\n" s
  in
  todo "test";
 let rec compile_decl_list = function 
|[] -> () 
|(h::t) ->  compile_decl h; compile_decl_list t
and compile_decl = function 
 |CDECL(l,s) -> todo ("cdecl " ^ s)
 |CFUN (l,s, vdl , lc) -> begin 
 todo ("cfun "^s);
 List.iter compile_decl vdl;
 compile_code lc
 end
and compile_code (_,c) = match c with 
|CBLOCK(vdl,lcl) -> begin 
 todo ("cblock ");
List.iter compile_decl vdl;
List.iter compile_code lcl
end
|_ -> todo ("not cblock")
in 
compile_decl_list decl_list

  