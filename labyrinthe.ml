(*-------------------------------------------------------------------*)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                           Labyrinthe                              *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*-------------------------------------------------------------------*)
open General

type labyrinthe =
  { g : int array array
  ; n : int
  ; src : Point.t
  ; dst : Point.t
  }

type labyrinthe_dynamique =
  { g : int array array array
  ; n : int
  ; t : int
  ; src : Point.t
  ; dst : Point.t
  }

(*-----------------------< Fonctions simples >-----------------------*)

let labyrinthe_vide n s d =
  let laby = { g = Array.make_matrix n n 0; n; src = s; dst = d } in
  for i = 0 to n - 1 do
    if i % 2 = 0 then
      for j = 0 to n - 1 do
        if j % 2 = 0 || i = 0 || i = n - 1 then laby.g.(i).(j) <- -1
      done
    else (
      laby.g.(i).(0) <- -1;
      laby.g.(i).(n - 1) <- -1)
  done;
  laby
;;

let dupliquer_labyrinthe (laby : labyrinthe) =
  { g = Array.init laby.n (fun i -> Array.init laby.n (fun j -> laby.g.(i).(j)))
  ; n = laby.n
  ; src = laby.src
  ; dst = laby.dst
  }
;;

(** Met les cases vides du labyrinthe à [max_int]*)
let normaliser (laby : labyrinthe) =
  for i = 0 to laby.n-1 do
    for j = 0 to laby.n-1 do
      laby.g.(i).(j) <- laby.g.(i).(j) > -1 <?> (max_int, -1)
    done
  done
;;

let voisins (laby : labyrinthe) (p : Point.t) =
  let n = laby.n - 1 in
  let res = ref [] in
  if p.i > 0 then res := Point.nord p :: !res;
  if p.j > 0 then res := Point.ouest p :: !res;
  if p.i < n then res := Point.sud p :: !res;
  if p.j < n then res := Point.est p :: !res;
  !res
;;

(** Donne les cases voisines d'une case dans le labyrinthe selon les directions cardinales*)
let voisins_2 (laby : labyrinthe) (p : Point.t) =
  let n = laby.n - 2 in
  let res = ref [] in
  if p.i > 1 then res := Point.nord (Point.nord p) :: !res;
  if p.j > 1 then res := Point.ouest (Point.ouest p) :: !res;
  if p.i < n then res := Point.sud (Point.sud p) :: !res;
  if p.j < n then res := Point.est (Point.est p) :: !res;
  !res
;;

let voisins_accessibles (laby : labyrinthe) (p : Point.t) =
  List.filter (fun (q : Point.t) -> laby.g.(q.i).(q.j) <> -1) (voisins laby p)
;;

let voisins_accessibles_dyn (laby : labyrinthe_dynamique) (p : Point.t) t =
  let n = laby.n - 2 in
  let res = ref [p] in
  if p.i > 1 then res := Point.nord (Point.nord p) :: !res;
  if p.j > 1 then res := Point.ouest (Point.ouest p) :: !res;
  if p.i < n then res := Point.sud (Point.sud p) :: !res;
  if p.j < n then res := Point.est (Point.est p) :: !res;
  List.filter (fun (q : Point.t) -> laby.g.(t).((p.i + q.i)/2).((p.j + q.j)/2) <> -1) !res
;;

(** [voisins l (x, y)] renvoie une liste de couples des coordonnees des murs et cases voisins*)
let voisins_2dif (laby : labyrinthe) (p : Point.t) ut : Point.t list =
  let valeur = Unir_trouver.trouver ut laby.g.(p.i).(p.j) in
  let v = ref [] in
  if p.i > 1 then
    if valeur <> Unir_trouver.trouver ut laby.g.(p.i - 2).(p.j) then
      v := Point.nord (Point.nord p) :: !v;
  if p.j > 1 then
    if valeur <> Unir_trouver.trouver ut laby.g.(p.i).(p.j - 2) then
      v := Point.ouest (Point.ouest p) :: !v;
  if p.i < laby.n - 2 then
    if valeur <> Unir_trouver.trouver ut laby.g.(p.i + 2).(p.j) then
      v := Point.sud (Point.sud p) :: !v;
  if p.j < laby.n - 2 then
    if valeur <> Unir_trouver.trouver ut laby.g.(p.i).(p.j + 2) then
      v := Point.est (Point.est p) :: !v;
  !v
;;

(*---------------------------< Afficher >----------------------------*)
let print_grille g n src dst =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let p = Point.creer (i, j) in
      if Point.egaux p src then print_string "<>"
      else if Point.egaux p dst then print_string "><"
      else if g.(i).(j) = -1 then print_string "[]"
      else if g.(i).(j) = -2 then print_string "::"
      else if g.(i).(j) = max_int then print_string "  "
      else (
        print_string " ";
        print_int (g.(i).(j) mod 10))
    done;
    print_newline ()
  done
;;

let print (laby : labyrinthe) = print_grille laby.g laby.n laby.src laby.dst

let print_ut (l : labyrinthe) ut =
  for i = 0 to l.n - 1 do
    for j = 0 to l.n - 1 do
      print_string
        (if l.g.(i).(j) = -2 then "\x1B[102m  \x1B[m"
         else if l.g.(i).(j) = -1 then noir "  "
         else if i % 2 = 1 && j % 2 = 1 then
           couleurs.(Unir_trouver.trouver ut l.g.(i).(j) % Array.length couleurs) "  "
         else "  ")
      (* else couleurs.(l.(i).(j) % Array.length couleurs) "  ") *)
    done;
    print_newline ()
  done;
  print_newline ()
;;

let print_grille_couleur g n =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      print_string
        (if g.(i).(j) = -2 then "\x1B[102m  \x1B[m"
         else if g.(i).(j) = -1 then "\x1B[40m  \x1B[m"
         else if g.(i).(j) = max_int then "  "
         else "\x1B[101m  \x1B[m")
    done;
    print_newline ()
  done;
  print_newline ()
;;
let print_couleur (l : labyrinthe) =
  print_grille_couleur l.g l.n
;;

(*--------------------------< Labyrinthe >--------------------------*)
let creer_labyrinthe n (s : Point.t) (d : Point.t) : labyrinthe =
  Random.self_init ();
  let laby =
    { g = Array.make_matrix ((2 * n) + 1) ((2 * n) + 1) (-1)
    ; n = (2 * n) + 1
    ; src = s
    ; dst = d
    }
  in
  let reste = ref [] in
  let dep, arr = ref 0, ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      laby.g.((2 * i) + 1).((2 * j) + 1) <- (i * n) + j;
      reste := ((2 * i) + 1, (2 * j) + 1) :: !reste;
      arr := (i * n) + j
    done
  done;
  let reste_array = Array.of_list (melanger_liste !reste) in
  let u_t = Unir_trouver.creation (n * n) in
  (* print_ut laby u_t; *)
  let aux differents =
    for i = 0 to Array.length reste_array - 1 do
      let p = Point.creer reste_array.(i) in
      match voisins_2dif laby p u_t with
      | [] when not differents ->
        (match voisins_2 laby p with
         | [] -> ()
         | l_voisins ->
           if Random.int ((n ^^ 2) / 2) = 0 then (
             let q = l_voisins <@ Random.int 4 in
             laby.g.((p.i + q.i) / 2).((p.j + q.j) / 2) <- laby.g.(p.i).(p.j)))
      | [] -> ()
      | l_voisins ->
        let q = l_voisins <@ Random.int 4 in
        laby.g.((p.i + q.i) / 2).((p.j + q.j) / 2) <- laby.g.(p.i).(p.j);
        Unir_trouver.unir u_t laby.g.(p.i).(p.j) laby.g.(q.i).(q.j)
    done
  in
  aux true;
  aux false;
  (* print_ut laby u_t; *)
  laby.g.(s.i).(s.j) <- 0;
  laby.g.(d.i).(d.j) <- 0;
  (* print_newline (); *)
  normaliser laby;
  laby
;;

(*--------------------------< Labyrinthe dynamique >--------------------------*)
let creer_labyrinthe_dynamique n t s d : labyrinthe_dynamique =
  { g = Array.init t (fun _ -> (creer_labyrinthe n s d).g)
  ; n = (2 * n) + 1
  ; t
  ; src = s
  ; dst = d
  }
;;

let creer_labyrinthe_dynamique_complexe n t s d p : labyrinthe_dynamique =
  let laby_dyn = { g = Array.init t (fun _ -> (creer_labyrinthe n s d).g)
  ; n = (2 * n) + 1
  ; t
  ; src = s
  ; dst = d
  } in
  for i = 1 to 2*n-1 do
    for j = 1 to 2*n-1 do
      let k = Random.int (t-1) in
      if k <> 0 then (
      let l = (k + 1) % t in
      if laby_dyn.g.(l).(i).(j) = -1 && Random.int 100 < p then
        laby_dyn.g.(k).(i).(j) <- -1
      )
    done
  done;
  laby_dyn
;;

let print_dynamique (laby_dyn : labyrinthe_dynamique) =
  for k = 0 to laby_dyn.t - 1 do
    print_grille laby_dyn.g.(k) laby_dyn.n laby_dyn.src laby_dyn.dst
  done
;;

let print_dynamique_couleur (laby_dyn : labyrinthe_dynamique) =
  for k = 0 to laby_dyn.t - 1 do
    print_grille_couleur laby_dyn.g.(k) laby_dyn.n
  done
;;
