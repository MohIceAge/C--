type direction = Up | Right | Down | Left

type ball = Position.t * int

type move = ball * direction

type game = ball list

let i = ref 0

let make_ball p = i := !i + 1; (p, !i-1)

let new_game ps = ps

let eq_ball b b' = match b,b' with (_, i),(_, i') -> i = i'

let make_move p d = (p, d)

let apply_move g move = failwith "TODO apply_move"

let moves g = failwith "TODO moves"

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
