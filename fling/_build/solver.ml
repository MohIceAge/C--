
let rec is_valid g move = let g' = Rules.apply_move g move in
  if Rules.won g' then true else let moves = Rules.moves g in
  if moves = [] then false else List.exists (is_valid g') moves

let solve game = let l = List.filter (is_valid game) (Rules.moves game) in
  if l = [] then None else Some(l)