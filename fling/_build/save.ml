open Printf

let s = string_of_int
let i = int_of_string

let ball_of_string s' = let l = List.map i (String.split_on_char ',' s') in
  match l with [x;y] -> Rules.make_ball (Position.from_int x y)
  | _ -> failwith "ball_of_string"

let save game = let balls = Rules.get_balls game in
  let l = List.map (fun ball ->
      let p = Rules.position_of_ball ball in
      let x,y = Position.proj_x p, Position.proj_y p in (s x)^","^(s y)) balls in
  let s = String.concat ";" l in
  let oc = open_out "game.fling" in
  fprintf oc "%s\n" s;
  close_out oc

let op () =
  let ic = open_in "game.fling" in
  try
    let s' = input_line ic in
    close_in ic;
    let l = String.split_on_char ';' s' in
    let balls = List.map ball_of_string l in
    Rules.new_game balls
  with e ->
    close_in_noerr ic;
    raise e


