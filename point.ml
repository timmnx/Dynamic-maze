(*-------------------------------------------------------------------*)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                               POINT                               *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*-------------------------------------------------------------------*)

type t =
  { mutable i : int
  ; mutable j : int
  }

let creer (i, j) = { i; j }
let egaux p q = p.i = q.i && p.j = q.j
let print p =
  print_string "(";
  print_int p.i;
  print_string ", ";
  print_int p.j;
  print_string ")"
;;
let nord p = creer (p.i - 1, p.j)
let ouest p = creer (p.i, p.j - 1)
let sud p = creer (p.i + 1, p.j)
let est p = creer (p.i, p.j + 1)