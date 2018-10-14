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
    | (p', i)::b -> if Position.eq p p' then true else aux b
  in
  aux (get_balls g)

let ball_of_position game p =
  let rec aux = function
      [] -> failwith "ball_of_position"
    | (p', i)::b -> if Position.eq p p' then (p', i) else aux b
  in
  aux game

let position_of_ball b = match b with (a, _) -> a

let apply_move g move =
  (*Raccourcis*)
  let px = Position.proj_x and py = Position.proj_y in
  let pos = Position.from_int in

  let (b, dir) = move in
  let (p, id) = b in
  let x,y = px p, py p in

  let balls = get_balls g in

  (*Creation de la liste des balles se trouvant dans la direction dir dans l'ordre*)
  let selector b' = let (p', _) = b' in match dir with Up -> x = (px p') && y < (py p')
    | Right -> y = (py p') && x < (px p')
    | Down -> x = (px p') && y > (py p')
    | Left -> y = (py p') && x > (px p') in
  let ordre b b' = let (p, _), (p', _) = b, b' in match dir with Up -> (py p') - (py p)
    | Right -> (px p') - (px p)
    | Down -> (py p) - (py p')
    | Left -> (px p) - (px p') in
  let balls_dir = List.sort ordre (List.filter selector balls) in

  let prev_pos p = let x = px p and y = py p in
  match dir with Down -> pos x (y+1)
    | Left -> pos (x+1) y
    | Up -> pos x (y-1)
    | Right -> pos (x-1) y in

  (*Met à jour les positions des balles*)
  let update_game g balls =
      let rec update_ball l b = match l with [] -> b
        | b'::q -> if eq_ball b' b then b' else update_ball q b
      in List.map (update_ball balls) g
  in

  let remove_ball balls b =
    List.filter (fun b' -> not (eq_ball b b')) balls in

  (*Renvoi la liste des balles dont la position doit changer*)
  let rec aux acc lst = function [] -> if eq_ball lst b then g else remove_ball (update_game g acc) lst
    | (p', i')::q -> let (p, i) = lst in if i=id  && Position.eq p (prev_pos p') then g
      else let acc' = ((prev_pos p'), i)::acc in
        aux acc' (p', i') q in

  aux [] b balls_dir

