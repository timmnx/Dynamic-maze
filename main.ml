(*-------------------------------------------------------------------*)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                              MAIN                                 *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*-------------------------------------------------------------------*)
open General
open Unir_trouver
open Labyrinthe
open A_star
(* open Parcours *)

let mi = max_int

let lab_test =
  { g =
      [| [| [| -1; -1; -1; -1; -1 |]
          ; [| -1; mi; -1; mi; -1 |]
          ; [| -1; mi; -1; mi; -1 |]
          ; [| -1; mi; mi; mi; -1 |]
          ; [| -1; -1; -1; -1; -1 |]
         |]
       ; [| [| -1; -1; -1; -1; -1 |]
          ; [| -1; mi; mi; mi; -1 |]
          ; [| -1; mi; -1; mi; -1 |]
          ; [| -1; mi; -1; mi; -1 |]
          ; [| -1; -1; -1; -1; -1 |]
         |]
      |]
  ; t = 2
  ; n = 5
  ; src = Point.creer (1, 1)
  ; dst = Point.creer (3, 3)
  }
;;

let main () =
  let laby = creer_labyrinthe 10 (Point.creer (1, 1)) (Point.creer (19, 19)) in
  A_star.classique laby (fun v -> A_star.dist_euclidienne v laby.dst);
  (* let laby_dyn = lab_test in *)
  let laby_dyn =
    creer_labyrinthe_dynamique_complexe 5 3 (Point.creer (1, 1)) (Point.creer (9, 9)) 10
  in
  Labyrinthe.print_dynamique_couleur laby_dyn;
  List.iter
    Point.print
    (Labyrinthe.voisins_accessibles_dyn laby_dyn (Point.creer (1, 1)) 0);
  print_newline ();
  A_star.dynamique laby_dyn (fun v -> A_star.dist_euclidienne v laby.dst);
  ()
;;

let () = main ()
