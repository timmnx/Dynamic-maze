(*-------------------------------------------------------------------*)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                               A*                                  *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*-------------------------------------------------------------------*)
open General
open Labyrinthe

(*-----------------------------< A* Dynamique >-----------------------------*)
type etat =
  { num_laby : int
  ; ombre_laby : labyrinthe
  ; position : point
  }

(* let laby_resultant (laby_dyn : labyrinthe_dynamique) : labyrinthe =
  let periode = cote laby_dyn in
  let n = cote laby_dyn.(0) in
  let laby_res = labyrinthe_vide n n in
  for i = 1 to n - 2 do
    for j = 1 to n - 2 do
      let mur = ref (-1) in
      for k = 0 to periode - 1 do
        mur := max !mur laby_dyn.(k).(i).(j)
      done;
      laby_res.(i).(j) <- !mur
    done
  done;
  laby_res
;; *)


(*-----------------------------< Aleatoire >-----------------------------*)
let marche_aleatoire (laby_dyn : labyrinthe_dynamique) : int =
  let p = Array.length laby_dyn in
  let n = cote laby_dyn.(0) in
  let l = ref 0 in
  let pos = ref (1,1) in
  while !pos <> (n-2, n-2) do
    let v = !pos::voisins !pos laby_dyn.(!l % p) in
    pos := element_aleatoire v;
    incr l
  done;
  !l
;;

let esperance_marche_aleatoire (laby_dyn : labyrinthe_dynamique) n_test =
  let s = ref 0 in
  for i = 0 to n_test - 1 do
    s := !s + marche_aleatoire laby_dyn;
  done;
  (float_of_int !s) /. (float_of_int n_test)
