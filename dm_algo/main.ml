
open Printf

let print_help () = 
    printf "Usage %s filename source sink\n" Sys.argv.(0);
    exit 0


let () = match Sys.argv with [|_; filename; source; sink|] -> begin
        (* On vérifie que s et t sont bien da *) 
        let ic = open_in file in
        try 
            let nodes = ref [] and edges = [] in
            if input_line ic <> "Sommets:" then print_help () 
            let line = ref (input_line ic) in
            while !line <> "Arcs:" do 
                nodes := !nodes@(String.split_on_char ',' !line);
                line := input_line ic;
            done;
            line := input_line ic;
            while !line <> "" do 
                let i = ref 0 in
                while !line.[!i+1] <> ':' do i:=!i+1 done;
                let node = String.sub s 0 !i in
                let neighbours = String.sub s (!i+4) ((String.length s) - (!i+5) )
                line := input_line ic;
            done;
        with _ ->  print_help () 
    end
    | _ -> print_help ()
