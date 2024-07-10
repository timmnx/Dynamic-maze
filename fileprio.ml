(*-------------------------------------------------------------------*)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                             FILEPRIO                              *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*-------------------------------------------------------------------*)

type 'a t =
  { mutable data : (int * 'a) list
  ; mutable len : int
  }

let creer () = { data = []; len = 0 }

let print fp print =
  let rec iter = function
    | [] -> print_endline "[]"
    | (p,x) :: t ->
      print_string ("<" ^ (string_of_int p) ^ "|");
      print x;
      print_string "> :: ";
      iter t
  in
  iter fp.data
;;

let ajouter fp x p =
  let rec insere l =
    match l with
    | [] -> (p, x) :: []
    | (hp, hx) :: t when p > hp -> (hp, hx) :: insere t
    | _ -> (p, x) :: l
  in
  fp.data <- insere fp.data;
  fp.len <- fp.len + 1
;;

let reprioriser fp x p =
  let action = ref false in
  let rec aux = function
    | [] ->
      action := true;
      []
    | (hp, hx) :: t when x = hx -> (p, x) :: t
    | h :: t -> h :: aux t
  in
  fp.data <- aux fp.data;
  if !action then ajouter fp x p
;;

let retirer fp =
  match fp.data with
  | [] -> failwith "Retirer sur fileprio vide"
  | h :: t ->
    fp.len <- fp.len - 1;
    fp.data <- t;
    h
;;