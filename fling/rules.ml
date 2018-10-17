type direction = Up | Right | Down | Left

type ball = Position.t * int

type move = ball * direction

type game = ball list

let i = ref 0

(*Raccourcis*)
let px, py = Position.proj_x , Position.proj_y
let pos = Position.from_int

(*Renvoi la position dans la direction opposé de dir*)
let prev_pos dir p = let x = px p and y = py p in
  match dir with Down -> pos x (y+1)
    | Left -> pos (x+1) y
    | Up -> pos x (y-1)
    | Right -> pos (x-1) y

(*Renvoi la position dans la direction de dir*)
let next_pos dir p = let x = px p and y = py p in
  match dir with Up -> pos x (y+1)
    | Right -> pos (x+1) y
    | Down -> pos x (y-1)
    | Left -> pos (x-1) y

let make_ball p = i := !i + 1; (p, !i-1)

let new_game ps = ps

let eq_ball b b' = match b,b' with (_, i),(_, i') -> i = i'


let make_move b d = (b, d)

let get_balls g = g

let won game = (List.length (get_balls game)) <2


let is_ball g p =
  let rec aux = function
      [] -> false
    | (p', i)::b -> if Position.eq p p' then true else aux b
  in
  aux (get_balls g)


(*Renvoi la liste des balles de balls se trouvant dans la direction dir dans l'ordre *)
let balls_dir b dir balls = let (p, id) = b in
  let x,y = px p, py p in
  let selector b' = let (p', _) = b' in match dir with Up -> x = (px p') && y < (py p')
    | Right -> y = (py p') && x < (px p')
    | Down -> x = (px p') && y > (py p')
    | Left -> y = (py p') && x > (px p') in
  let ordre b b' = let (p, _), (p', _) = b, b' in match dir with Up -> (py p') - (py p)
    | Right -> (px p') - (px p)
    | Down -> (py p) - (py p')
    | Left -> (px p) - (px p') in
  List.sort ordre (List.filter selector balls)

let moves g = let balls = get_balls g in
  let ball_moves b = let (p, id) = b in
    let l = List.filter (fun dir -> not (is_ball g (next_pos dir p)) && (balls_dir b dir balls) != [] ) [Up; Right; Down; Left] in
    List.map (fun dir -> make_move b dir) l
  in
  List.concat (List.map ball_moves balls)




let ball_of_position game p =
  let rec aux = function
      [] -> failwith "ball_of_position"
    | (p', i)::b -> if Position.eq p p' then (p', i) else aux b
  in
  aux game

let position_of_ball b = match b with (a, _) -> a

let apply_move g move =
  let (b, dir) = move in
  let (p, id) = b in

  let balls = get_balls g in

  (*Met à jour les positions des balles*)
  let update_game g balls =
      let rec update_ball l b = match l with [] -> b
        | b'::q -> if eq_ball b' b then b' else update_ball q b
      in List.map (update_ball balls) g
  in

  let remove_ball balls b =
    List.filter (fun b' -> not (eq_ball b b')) balls in


  let rec aux acc lst = function [] -> if eq_ball lst b then g (*Pas de balle dans la direction choisie*)
                                        else remove_ball (update_game g acc) lst (* On supprime la balle qui sort du quadrillage*)
    | (p', i')::q -> let (p, i) = lst in if i=id  && Position.eq p (prev_pos dir p') then g (* Si la premiere balle est collé on ne fait rien*)
      else let acc' = ((prev_pos dir p'), i)::acc in (* sinon on empile la modification à faire *)
        aux acc' (p', i') q in
  if is_ball g (next_pos dir p) then g else
  aux [] b (balls_dir b dir balls)

