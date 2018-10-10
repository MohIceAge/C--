type direction = Up | Right | Down | Left

type ball = Position.t * int

type move = ball * direction

type game = ball list

let i = ref 0

let make_ball p = i := !i + 1; (p, !i-1)

let new_game ps = ps

let eq_ball b b' = match b,b' with (_, i),(_, i') -> i = i'

let make_move p d = (p, d)


let moves g = []

let get_balls g = g

let is_ball g p =
  let rec aux = function
      [] -> false
    | (p', i)::b -> if p=p' then true else aux b
  in
  aux g

let ball_of_position game p =
  let rec aux = function
      [] -> failwith "ball_of_position"
    | (p', i)::b -> if p=p' then (p', i) else aux b
  in
  aux game

let position_of_ball b = match b with (a, _) -> a

let apply_move a b =
  let  g,((p, i), d) = Obj.magic (a,b) in
  let next_pos (x',y') = match d with Up -> (x',y'+1) | Right -> (x'+1, y') | Down -> (x', y'-1) | Left -> (x'-1, y') in
  let pos_in_range (x', y') = 0 <= x' && 0<= y' && x' < 15 && y' < 15 in
  let p' = next_pos p in
  if is_ball g p' then g
  else let rec aux game i' p' =
         let p'' = next_pos p' in
         if not (pos_in_range p'') then if i=i' then g else List.filter (fun (_, k) -> k!=i) game
         else if is_ball game p'' then let (q, j) = ball_of_position game p'' in
           aux (List.map (fun (a,b) -> if b=i' then (p', i') else (a,b)) game) j (next_pos q)
         else aux game i' p'' in Obj.magic (aux g i p')

