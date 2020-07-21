(* Modyfikacja drzew *)
(* Autor: Juliusz Korab-Karpowicz *)
(* Reviewer: Szymon Frąckowiak (gr. 1) *)

(*
 * ISet - Interval sets
 * Copyright (C) 1996-2019 Xavier Leroy, Nicolas Cannasse, Markus Mottl,
 * Jacek Chrzaszcz, Juliusz Korab-Karpowicz
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Typ drzewa - Pusty, albo wierzchołek przechowujący lewego i prawego syna,
 * wartość, wysokość w drzewie oraz sumę zakresu liczb w poddrzewie *)
type t =
  | Empty
  | Node of t * (int * int) * t * int * int

(* Zwraca wysokość *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(* Zwraca sumę zakresu liczb w poddrzewie *)
let desc = function
  | Node (_, _, _, _, d) -> d
  | Empty -> 0

(* Tworzy drzewo *)
let make l k r =
  Node (l, k, r, max (height l) (height r) + 1, desc l + desc r + snd k - fst k + 1)

(* Balansuje drzewo, aby spełniało warunki AVLa *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

(* Minimalny element *)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(* Usuwa minimalny element *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

(* Łączy dwa drzewa *)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

(* To jest zwracane przez lower/upper bound gdy nie ma wyniku *)
let none = max_int, min_int

(* Najmniejszy element, że fst k >= x *)
let rec lower_bound x acc = function
  | Node (l, k, r, _, _) ->
      if fst k < x then
        lower_bound x acc r
      else 
        lower_bound x (if acc = none then k else min acc k) l
  | Empty -> acc

(* Największy element, że fst k <= x *)
let rec upper_bound x acc = function
  | Node (l, k, r, _, _) ->
      if fst k > x then
        upper_bound x acc l
      else 
        upper_bound x (if acc = none then k else max acc k) r
  | Empty -> acc

(* Puste drzewo *)
let empty = 
  Empty

(* Sprawdza czy drzewo jest puste*)
let is_empty x = 
  x = Empty

(* Dodanie zakresu liczb do drzewa, z gwarancją nieistnienia kolidujących
 * zakresów *)
let rec add_one x = function
  | Node (l, k, r, h, d) ->
      let c = compare x k in
      if c = 0 then Node (l, x, r, h, d)
      else if c < 0 then
        let nl = add_one x l in
        bal nl k r
      else
        let nr = add_one x r in
        bal l k nr
  | Empty -> make Empty x Empty

(* Usunięcie zakresu liczb z drzewa, z gwarancją istnienia dokładnie takiego 
 * zakresu w drzewie *)
let remove_one x set =
  let rec loop = function
    | Node (l, k, r, _, _) ->
        let c = compare x k in
        if c = 0 then merge l r else
        if c < 0 then bal (loop l) k r else bal l k (loop r)
    | Empty -> Empty in
  loop set

(* Usunięcie zakresu liczb z drzewa *)
let rec remove x set =
  let ret = lower_bound (fst x) none set in
  if ret != none && fst ret <= snd x then
    if snd ret <= snd x then
      remove x (remove_one ret set)
    else
      remove x (add_one (snd x + 1, snd ret) (remove_one ret set))
  else
    let ret = upper_bound (fst x) none set in
    if ret = none || snd ret < fst x then
      set
    else if snd ret > snd x then
      if fst ret >= fst x then
        remove x (add_one (snd x + 1, snd ret) (remove_one ret set))
      else
        remove x (add_one (fst ret, fst x - 1) (add_one (snd x + 1, snd ret) (remove_one ret set)))
    else if fst ret >= fst x then
      remove x (remove_one ret set)
    else
      remove x (add_one (fst ret, fst x - 1) (remove_one ret set))

(* Dodanie zakresu liczb do drzewa *)
let add x set =
  let set = remove x set in
  let ret = lower_bound (fst x) none set in
  if ret != none && (snd x = max_int || fst ret = snd x + 1) then
    let set, x = remove_one ret set, (fst x, snd ret) in
    let ret = upper_bound (fst x) none set in
    if ret != none && (fst x = min_int || snd ret = fst x - 1) then
      let set, x = remove_one ret set, (fst ret, snd x) in
      add_one x set
    else
      add_one x set
  else
    let ret = upper_bound (fst x) none set in
    if ret != none && (fst x = min_int || snd ret = fst x - 1) then
      let set, x = remove_one ret set, (fst ret, snd x) in
      add_one x set
    else
      add_one x set

(* Czy liczba x należy do drzewa set *)
let mem x set =
  let rec loop = function
    | Node (l, k, r, _, _) ->
      if fst k <= x && x <= snd k then
        true
      else
        loop (if fst k > x then l else r)
    | Empty -> false in
  loop set

(* Wykonuje f, i aplikuje na wszystkich przedziałach należących do drzewa, w rosnącej
 * kolejności *)
let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop set

(* Wykonuje f na wszystkich przedziałach należących do drzewa, w rosnącej
 * kolejności wyliczając wartość końcową funkcji z początkowym argumentem acc *)
let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc set

(* Lista wszystkich przedziałów należących do drzewa, w kolejności rosnącej *)
let elements set = 
  let rec loop acc = function
    | Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] set

(* Funkcja pomocnicza do split - połącza poddrzewa z pewną wartością *)
let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(* Zwraca drzewo zawierające liczby mniejsze od x, wartość boolową oznaczającą
 * czy x było częścią drzewa, i drzewo zawierające liczby większe od x *)
let split x set =
  let pres = mem x set in
  let set = remove (x, x) set in
  let rec loop x = function
    | Empty ->
        (Empty, Empty)
    | Node (l, v, r, _, _) ->
        let c = compare (x, x) v in
        if c < 0 then
          let (ll, rl) = loop x l in (ll, join rl v r)
        else
          let (lr, rr) = loop x r in (join l v lr, rr)
  in
  let setl, setr = loop x set in
  setl, pres, setr

(* Dodawanie bez overflowu *)
let (+.) x y = 
  if x + y < 0 then max_int
  else x + y

(* Odejmowanie bez overflowu *)
let (-.) x y = 
  if x - y < 0 then max_int
  else x - y

(* Zwraca ilość liczb należących do drzewa, które są mniejsze bądź równe x *)
let below x set =
  let rec loop acc = function
    | Empty -> acc
    | Node(l, k, r, _, d) ->
      if fst k <= x && x <= snd k then
        acc +. (x -. fst k +. 1 +. desc l )
      else if fst k < x then
        loop (acc +. desc l +. (snd k -. fst k) +. 1) r
      else
        loop acc l
  in loop 0 set

(* Testy *)

(* let a = empty *)
(* let a = add (-20, 5) a *)
(* let a = add (6, 18) a *)
(* let a = add (4, 10) a *)
(* let a = add (14, 16) a *)
(* let a = remove (-18, 14) a *)
(* let a = remove (5, 17) a;; *)
(* assert(mem 14 a = false);; *)
(* let a = add (-4, 9) a;; *)
(* assert(mem 16 a = false);; *)
(* assert(mem (-14) a = false);; *)
(* assert(mem 10 a = false);; *)
(* let a = remove (-9, 10) a;; *)
(* let a = add (-6, 7) a;; *)
(* let a = add (-2, 7) a;; *)
(* let a = add (-12, 17) a;; *)
(* let a = add (-13, 8) a;; *)
(* let a = add (-13, -2) a;; *)
(* assert(mem 11 a = true);; *)
(* assert(elements a = [(-20, -19); (-13, 18)]);; *)
(* let a = add (0, 5) empty;; *)
(* let a = add (7, 8) a;; *)
(* let a = add (-3, -3) a;; *)
(* let a = add (10, 13) a;; *)
(* assert(elements a = [(-3, -3); (0, 5); (7, 8); (10, 13)]);; *)
(* assert(below 8 a = 9);; *)
(* let b = add (6, 6) a;; *)
(* let b = remove (6, 6) b;; *)
(* let b = add (-100, -5) b;; *)
(* let b = add (-4, 6) b;; *)
(* assert(elements b = [(-100, 8); (10, 13)]);; *)
(* assert(below 10 b = 110);; *)
(* let c = remove (2, 10) a;; *)
(* assert(elements c = [(-3, -3); (0, 1); (11, 13)]);; *)
(* assert(below 12 c = 5);; *)
