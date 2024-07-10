(*-------------------------------------------------------------------*)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                A*                                 *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*-------------------------------------------------------------------*)
open General
open Fileprio
open Point
open Labyrinthe

let dist_euclidienne p d = abs (p.i - d.i) + abs (p.j - d.j)

let classique (laby : Labyrinthe.labyrinthe) h =
  let dist = Array.make_matrix laby.n laby.n max_int in
  let fp = Fileprio.creer () in
  let parent = Array.make_matrix laby.n laby.n { i = -1; j = -1 } in
  dist.(laby.src.i).(laby.src.j) <- 0;
  Fileprio.ajouter fp laby.src (h laby.src);
  let relax v w =
    let d = dist.(v.i).(v.j) + 1 in
    if d < dist.(w.i).(w.j) then (
      dist.(w.i).(w.j) <- d;
      laby.g.(w.i).(w.j) <- d;
      parent.(w.i).(w.j) <- v;
      Fileprio.reprioriser fp w (d + h w))
  in
  let rec boucle () =
    if fp.data = [] then failwith "A_star : Pas trouve";
    let _, v = Fileprio.retirer fp in
    if not (Point.egaux v laby.dst) then (
      List.iter (relax v) (Labyrinthe.voisins_accessibles laby v);
      boucle ())
  in
  boucle ();
  let rec colorer v =
    laby.g.(v.i).(v.j) <- -2;
    if not (Point.egaux v laby.src) then colorer parent.(v.i).(v.j)
  in
  colorer laby.dst;
  Labyrinthe.print_couleur laby
;;

let dynamique (laby : Labyrinthe.labyrinthe_dynamique) h =
  let dist = Array.init laby.t (fun _ -> Array.make_matrix laby.n laby.n max_int) in
  let fp = Fileprio.creer () in
  let parent = Array.init laby.t (fun _ -> Array.make_matrix laby.n laby.n ({ i = -1; j = -1 }, -1)) in
  dist.(0).(laby.src.i).(laby.src.j) <- 0;
  Fileprio.ajouter fp (laby.src, 0) (h laby.src);
  let relax t v w =
    let d = dist.(t).(v.i).(v.j) + 1 in
    let t2 = (t + 1) % laby.t in
    if d < dist.(t2).(w.i).(w.j) then (
      dist.(t2).(w.i).(w.j) <- d;
      laby.g.(t).((v.i + w.i) / 2).((v.j + w.j) / 2) <- d;
      (* laby.g.((t + 1) % laby.t).((v.i + w.i) / 2).((v.j + w.j) / 2) <- d; *)
      laby.g.(t2).(w.i).(w.j) <- d;
      parent.(t2).(w.i).(w.j) <- v, t;
      Fileprio.reprioriser fp (w, t2) (d + h w))
  in
  let rec boucle () =
    if fp.data = [] then failwith "A_star : Pas trouve";
    let _, (v, t) = Fileprio.retirer fp in
    Point.print v; print_newline ();
    if not (Point.egaux v laby.dst) then (
      List.iter (relax t v) (Labyrinthe.voisins_accessibles_dyn laby v t);
      boucle ())
    else t
  in
  let t = boucle () in
  let rec colorer (v, t) =
    (* Point.print v; print_int t; print_newline (); *)
    let w, t2 = parent.(t).(v.i).(v.j) in
    laby.g.(t).(v.i).(v.j) <- -2;
    (* laby.g.(t).((v.i + w.i) / 2).((v.j + w.j) / 2) <- -2; *)
    (* laby.g.(t2).((v.i + w.i) / 2).((v.j + w.j) / 2) <- -2; *)
    if not (Point.egaux v laby.src) then colorer (w, t2)
  in
  colorer (laby.dst, t);
  Labyrinthe.print_dynamique_couleur laby
;;
