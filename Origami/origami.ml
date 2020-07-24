(* Origami *)
(* Autor: Juliusz Korab-Karpowicz *)
(* Code Review: Michał Traczyk*)

(* Punkt albo wektor na płaszczyżnie *)
type point = float * float

(* Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(* Iloczyn wektorowy *)
let il_wek (x1, y1) (x2, y2) = x1 *. y2 -. x2 *. y1

(* Odejmowanie wektorów *)
let (-*) (x1, y1) (x2, y2) = 
  (x1 -. x2, y1 -. y2)

(* Dzielenie wektoru przez skalar *)
let (/*) (x1, y1) x = 
  (x1 /. x, y1 /. x)

(* Funkcja zamieniająca bool w int *)
let int x = 
  if x then 1 else 0

(* [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
 * o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
 * a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
 * od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
 * (lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz, 
 * w pozostałych przypadkach 0 razy *)
let prostokat (x1, y1) (x2, y2) = 
  fun (x, y) -> int (x >= x1 && x <= x2 && y >= y1 && y <= y2)

(* Liczba podniesiona do kwadratu *)
let sq x =
  x *. x

(* Wyjątek gdy podany jest ujemny promień koła *)
exception Niepoprawne_dane

(* [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku w
 * punkcie [p] i promieniu [r] *)
let kolko (xk, yk) r =
  if r < 0. then raise Niepoprawne_dane
  (* else fun (x, y) -> int (sq(xk -. x) +. sq(yk -. y) <= sq r) *)
  else fun (x, y) -> int (sq(xk -. x) +. sq(yk -. y) <= sq r)

(* [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
 * punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
 * w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])
 * jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
 * przebicie po prawej stronie prostej powinno więc zwrócić 0.
 * Przebicie dokładnie na prostej powinno zwrócić tyle samo,
 * co przebicie kartki przed złożeniem. Po stronie lewej -
 * tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
 * który nałożył się na punkt przebicia. *)
let zloz p1 p2 k = 
  fun p -> match compare (il_wek (p -* p1) (p2 -* p1)) 0. with
  | 0 -> k p
  | x when x > 0 -> 0
  | _ -> 
      let odb = 
        let a, b = p1 -* p2 in
        let c = il_wek p2 p1 in
        let asq = sq a in
        let bsq = sq b in
        let x, y = p in
        ((asq -. bsq) *. x +. 2. *. b *. (c +. a *. y), 
        (bsq -. asq) *. y -. 2. *. a *. (c -. b *. x)) /* (asq +. bsq)
      in k p + k odb

(* Wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
 * z listy *) 
let skladaj l k =
  let pom acc (p1, p2) = 
    zloz p1 p2 acc
  in List.fold_left pom k l

(* Testy *)

(* let test a b msg = if a<>b then (print_int a; print_string "<>"; print_int b; print_string " test: "; print_endline msg);; *)

(* let p1 = prostokat (0., 0.) (10., 10.) *)
(* let k1 = kolko (5., 5.) 5. *)
(* let l1 = [((0., 0.), (10., 10.)); *)
(* 	  ((5., 0.), (10., 5.)); *)
(* 	  ((10., 0.), (0., 10.)); *)
(* 	  ((2.5, 0.), (2.5, 10.))];; *)
(* let l2 = [((8., 0.), (10., 2.)); *)
(* 	  ((6., 0.), (10., 4.)); *)
(* 	  ((4., 0.), (10., 6.)); *)
(* 	  ((2., 0.), (10., 8.)); *)
(* 	  ((0., 0.), (10., 10.)); *)
(* 	  ((0., 2.), (8., 10.)); *)
(* 	  ((0., 4.), (6., 10.)); *)
(* 	  ((0., 6.), (4., 10.)); *)
(* 	  ((0., 8.), (2., 10.))];; *)

(* let p2 = skladaj l1 p1 *)
(* let p3 = skladaj l2 p1 *)
(* let k2 = skladaj l1 k1;; *)

(* test (p2 (7., 3.)) 0 "0.1: p2";; *)
(* test (p2 (5., 8.)) 0 "0.2: p2";; *)
(* test (p2 (3., 5.)) 0 "0.3: p2";; *)
(* test (p2 (5., 5.)) 0 "0.4: p2";; *)
(* test (p2 (0., 0.)) 2 "1: p2";; *)
(* test (p2 (0., 10.)) 2  "2: p2";; *)
(* test (p2 (2.5, 2.5)) 2 "3: p2";; *)
(* test (p2 (2.5, 7.5)) 2 "4: p2";; *)
(* test (p2 (2.5, 5.)) 4 "5: p2";; *)
(* test (p2 (0., 5.)) 5 "6: p2";; *)
(* test (p2 (1., 2.)) 4 "7: p2";; *)
(* test (p2 (1., 5.)) 8 "8: p2";; *)
(* test (p2 (1., 8.)) 4 "9: p2";; *)

(* test (k2 (7., 3.)) 0 "0.1: k2";; *)
(* test (k2 (5., 8.)) 0 "0.2: k2";; *)
(* test (k2 (3., 5.)) 0 "0.3: k2";; *)
(* test (k2 (5., 5.)) 0 "0.4: k2";; *)
(* test (k2 (2.5, 2.5)) 2 "1: k2";; *)
(* test (k2 (2.5, 7.5)) 2 "2: k2";; *)
(* test (k2 (2.5, 5.)) 4 "3: k2";; *)
(* test (k2 (0., 5.)) 5 "4: k2";; *)
(* test (k2 (1., 3.)) 4 "5: k2";; *)
(* test (k2 (1., 5.)) 8 "6: k2";; *)
(* test (k2 (1., 7.)) 4 "7: k2";; *)

(* test (p3 ((-4.), 6.)) 2 "1: p3";; *)
(* test (p3 ((-3.), 5.)) 1 "2: p3";; *)
(* test (p3 ((-3.), 7.)) 2 "3: p3";; *)
(* test (p3 ((-2.), 6.)) 3 "4: p3";; *)
(* test (p3 ((-2.5), 6.5)) 4 "5: p3";; *)
(* test (p3 ((-2.), 8.)) 4 "6: p3";; *)
(* test (p3 ((-1.), 7.)) 3 "7: p3";; *)
(* test (p3 ((-1.5), 7.5)) 6 "8: p3";; *)
(* test (p3 (0., 8.)) 5 "9: p3";; *)
(* test (p3 ((-1.), 9.)) 4 "10: p3";; *)
(* test (p3 ((-0.5), 8.5)) 8 "11: p3";; *)
(* test (p3 (0., 10.)) 6 "12: p3";; *)
(* test (p3 (1., 9.)) 5 "13: p3";; *)
(* test (p3 (0.5, 9.5)) 10 "14: p3";; *)

(* let kolo = kolko (0.,0.) 10. in *)
(* assert (kolo (1000., 0.) = 0); *)
(* let poziomo = zloz (0.,0.) (1.,0.) kolo in *)
(* assert (poziomo (0.,0.) = 1); *)
(* assert (poziomo (0.,1.) = 2); *)
(* assert (poziomo (0.,-1.) = 0); *)
(* let pionowo = zloz (0.,0.) (0.,1.) kolo in *)
(* assert (pionowo (0.,0.) = 1); *)
(* assert (pionowo (-1.,0.) = 2); *)
(* assert (pionowo (1.,0.) = 0); *)
(* let cwiartka = zloz (0.,0.) (0.,1.) poziomo in *)
(* assert (cwiartka (0.,0.) = 1); *)
(* assert (cwiartka (-1.,1.) = 4); *)
(* assert (cwiartka (-1.,0.) = 2); *)

