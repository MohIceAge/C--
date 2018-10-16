
let rec solve g =
  let moves = Rules.moves g in
  if Rules.won g then Some([]) else if moves = [] then None else begin
    let games = List.map (fun mov -> (Rules.apply_move g mov, mov)) moves in
    try 
      let reclist = List.map (fun gm -> let (g,m) = gm in (find g, m)) games in
      let valid_moves = List.find (fun x -> let (a,b) = x in a!=None) reclist in
      match valid_moves with Some(next), mov -> Some(mov::next)
      | _ -> failwith "solve"
    with _ -> None
  end
