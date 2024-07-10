(*-------------------------------------------------------------------*)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                             General                               *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*-------------------------------------------------------------------*)

let normal = "\x1B[0m"
let rouge x = "\x1B[41m" ^ x ^ normal
let vert x = "\x1B[42m" ^ x ^ normal
let jaune x = "\x1B[43m" ^ x ^ normal
let bleu x = "\x1B[44m" ^ x ^ normal
let violet x = "\x1B[45m" ^ x ^ normal
let orange x = "\x1B[46m" ^ x ^ normal
let noir x = "\x1B[40m" ^ x ^ normal
let blanc x = "\x1B[47m" ^ x ^ normal
let couleurs = [| rouge; vert; jaune; bleu; violet; orange; blanc |]

(** [expr <?> (t, f)] renvoie [t] si [expr] est vrai, [f] sinon.*)
let ( <?> ) expr (t, f) = if expr then t else f

(** [a % b] renvoie le modulo positif de [a] par [b].*)
let ( % ) a b =
  let c = a mod b in
  c >= 0 <?> (c, c + b)
;;

(** [a^^b] renvoie a ([int]) puissance b([int])*)
let rec ( ^^ ) a b =
  if b = 0 then 1 else if b % 2 = 1 then a * (a ^^ (b - 1)) else (a * a) ^^ (b / 2)
;;

type vecteur =
  { x : int
  ; y : int
  }

let liste_alea n =
  Random.self_init ();
  let mem = Array.init n (fun i -> i) in
  for i = 0 to n - 2 do
    let k = i + 1 + Random.int (n - i - 1) in
    mem.(i) <- mem.(i) + mem.(k);
    mem.(k) <- mem.(i) - mem.(k);
    mem.(i) <- mem.(i) - mem.(k)
  done;
  let res = ref [] in
  for i = 0 to n - 1 do
    res := mem.(i) :: !res
  done;
  !res
;;

let ( <@ ) l n =
  let rec aux n = function
    | [] -> aux n l
    | h :: t when n = 0 -> h
    | h :: t -> aux (n - 1) t
  in
  aux n l
;;

let element_aleatoire l =
  match l with
  | [] -> failwith "Liste vide"
  | hd :: tl ->
    let rec aux p r = function
      | [] -> r
      | h :: t -> aux (p + 1) (Random.int p = 0 <?> (h, r)) t
    in
    aux 1 hd tl
;;

let melanger_liste liste =
  Random.self_init ();
  let mem = Array.of_list liste in
  let n = Array.length mem in
  for i = 0 to n - 2 do
    let k = i + 1 + Random.int (n - i - 1) in
    let temp = mem.(i) in
    mem.(i) <- mem.(k);
    mem.(k) <- temp
  done;
  let res = ref [] in
  for i = 0 to n - 1 do
    res := mem.(i) :: !res
  done;
  !res
;;

let print_tab a =
  let n = Array.length a in
  for i = 0 to n - 1 do
    let m = Array.length a.(i) in
    for j = 0 to m - 1 do
      if a.(i).(j) = max_int then print_char 'm'
      else if a.(i).(j) <> -1 then print_int (a.(i).(j) / 2)
      else print_char ' ';
      print_char ' '
    done;
    print_newline ()
  done;
  print_newline ()
;;
