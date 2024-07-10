(*-------------------------------------------------------------------*)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                         Unir et Trouver                           *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*-------------------------------------------------------------------*)

(*--------------------------< Structure >----------------------------*)
type 'a unir_trouver =
  { parent : int array
  ; rang : int array
  ; mutable classes : int
  }

(*---------------------------< Creation >----------------------------*)
let creation n = {
  parent = Array.init (n) (fun i -> i);
  rang = Array.make n 0;
  classes = n
}

(*---------------< Trouver avec compression de chemin>---------------*)
let rec trouver u_t x =
  let y = u_t.parent.(x) in
  if y = x then x
  else (
    let y_repr = trouver u_t y in
    (* le pere de x devient le representant de sa classe *)
    u_t.parent.(x) <- y_repr;
    y_repr)
;;

(*------------------------< Union par rang >-------------------------*)
let unir u_t x y =
  let x_repr = trouver u_t x in
  let y_repr = trouver u_t y in
  if x_repr <> y_repr then (
    let x_rang = u_t.rang.(x) in
    let y_rang = u_t.rang.(y) in
    if x_rang > y_rang then u_t.parent.(y_repr) <- x_repr
    else u_t.parent.(x_repr) <- y_repr;
    if x_rang = y_rang then u_t.rang.(y_repr) <- y_rang + 1;
    u_t.classes <- u_t.classes - 1)
;;
