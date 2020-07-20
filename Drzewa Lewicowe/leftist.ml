(* Drzewa Lewicowe *)
(* Autor: Juliusz Korab-Karpowicz *)
(* Recenzent: Grzegorz B. Zaleski *)

(* Typ złączalnej kolejki priorytetowej *)
type 'a queue =
    | Node of ('a * 'a queue * 'a queue * int)
    | Null

(* Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)
let is_empty q =
    q = Null

(* Pusta kolejka priorytetowa *)
let empty = Null

(* Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty

(* Uporządkowuje dwie kolejki według:
 * I. Nie byciem pustą kolejką
 * II. Priorytem elementu zawartego w korzeniu
 * Zwracana jest para, w której druga kolejka jest albo pusta, albo ma element
 * o mniejszym priorytecie w korzeniu, niż kolejka pierwsza*)
let order q1 q2 = 
    match q1, q2 with
    | _, Null ->
        q1, q2
    | Node (x1, _, _, _), Node (x2, _, _, _) when x1 < x2 ->
        q1, q2
    | _, _ ->
        q2, q1

(* [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)
let rec join q1 q2 =
    let q1, q2 = order q1 q2 in
    match q1, q2 with
    | _, Null ->
        q1
    | Null, _ ->
        assert false
    | Node (x1, l1, r1, npl1), _ ->
        let new_r = join r1 q2 in
        match new_r with
        | Node (_, _, _, npl2) ->
            if npl1 > npl2 then
                Node (x1, l1, new_r, min npl1 npl2 + 1)
            else
                Node (x1, new_r, l1, min npl1 npl2 + 1)
        | Null ->
            assert false

(* Zwraca kolejkę powstałą z dołączenia elementu [e] do kolejki [q] *)
let add e q =
    join q (Node (e, Null, Null, 0))

(* Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e, q')] gdzie [e]
 * jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
 * Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min q =
    match q with
    | Node (x, l, r, _) ->
        (x, join l r)
    | Null ->
        raise Empty

(* Testy *)

(* let test a b num msg =*)
(*   if a = b then print_endline "ok"*)
(*   else (print_int num; print_endline msg);;*) 

(* let rec zwin l q num msg =*)
(*   try*)
(*     match l with*)
(*     | [] -> test q empty num msg*)
(*     | h::t -> let (mn, r) = delete_min q in test mn h num msg; zwin t r (num+1) msg*)
(*   with Empty -> (print_int num; print_string "Empty"; print_endline msg);;*)

(* let a = add 0. empty;;        (* 0.*)*)
(* let b = add 1. empty;;        (* 1. *)*)
(* let c = add (-0.1) empty;;    (* -0.1 *)*)
(* let d = add 7. a;;            (* 0., 7. *)*)
(* let e = add (-3.) d;;         (* -3., 0., 7. *)*)
(* let f = add (-0.5) c;;        (* -0.5, -0.1 *)*)
(* let g = join b c;;            (* -0.1, 1.*)*)
(* let h = join d e;;            (* -3., 0., 0., 7., 7. *)*)
(* let i = join f e;;            (* -3., -0.5, -0.1, 0., 7. *)*)
(* let j = join h i;;            (* -3., -3., -0.5, -0.1, 0., 0., 0., 7., 7., 7. *)*)

(* let la = [0.];;*)
(* let lb = [1.];;*)
(* let lc = [-0.1];;*)
(* let ld = la @ [7.];;*)
(* let le = -3.::ld;;*)
(* let lf = -0.5::lc;;*)
(* let lg = lc @ lb;;*)
(* let lh = [-3.; 0.; 0.; 7.; 7.];;*)
(* let li = [-3.; -0.5; -0.1; 0.; 7.];;*)
(* let lj = [-3.; -3.; -0.5; -0.1; 0.; 0.; 0.; 7.; 7.; 7.];;*)

(* test (join empty empty) empty (-1) ": empty + empty";;*)
(* zwin la a 0 ": a";;*)
(* zwin lb b 0 ": b";;*)
(* zwin lc c 0 ": c";;*)
(* zwin ld d 0 ": d";;*)
(* zwin le e 0 ": e";;*)
(* zwin lf f 0 ": f";;*)
(* zwin lg g 0 ": g";;*)
(* zwin lh h 0 ": h";;*)
(* zwin li i 0 ": i";;*)
(* zwin lj j 0 ": j";;*)
