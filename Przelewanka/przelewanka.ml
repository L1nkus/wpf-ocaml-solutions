(* Przelewanka *)
(* Autor: Juliusz Korab-Karpowicz *)
(* Reviewer: Juliusz Korab-Karpowicz *)

(* Zwraca tablicę z pominięciem naczyń o zerowej pojemności*)
let usun_zerowe_pojemnosci l =
  Array.of_list (
    Array.fold_left (fun acc (x, y) -> if x = 0 then acc else (x,y) :: acc) [] l
    )

(* Sprawdza, czy docelowo istnieje co najmniej jedna szklanka pełna, bądź pusta *)
(* Gdy tablica nie spełnia warunku, zwraca true *)
let warunek_pelen_lub_pusty l =
  Array.fold_left (fun acc (x, y) -> acc && y <> 0 && y <> x) true l

(* Największy wspólny dzielnik liczb dodatnich *)
let rec nwd a b =
  if b = 0 then a else nwd b (a mod b)

(* Sprawdza, czy docelowa zawartość każdej szklanki jest wielokrotnością nwd *)
(* Gdy tablica nie spełnia warunku, zwraca true *)
let warunek_nwd l =
  let l_nwd = Array.fold_left (fun acc (x, _) -> nwd x acc) 0 l in
  Array.fold_left (fun acc (_, y) -> acc || y mod l_nwd <> 0) false l

(* Przelewanka - wyznacza minimalną ilość ruchów, aby uzyskać odpowiednie
 * ilości wody w każdej ze szklanek o podanych pojemnościach. Dozwolone
 * czynności, to nalanie do pełna wody z kranu, wylanie całej wody do zlewu, i
 * przelanie wody z jednej szklanki do drugiej (na tyle ile się zmieści)
 * Funkcja przyjmuje tablicę par liczb [|(x1,y1);...;(xn,yn)|], gdzie x oznacza
 * pojemność danej szklanki, a y jej docelową ilość wody.
 * Gdy rozwiązanie nie istnieje, zwraca -1 *)
let przelewanka l =
  let l = usun_zerowe_pojemnosci l in
  let n = Array.length l in
  if n = 0 then
    0
  else if warunek_pelen_lub_pusty l || warunek_nwd l then
    -1
  else
    let set = Hashtbl.create 1000007 in
    let wyjscie = Array.map snd l in
    let limit = Array.map fst l in
    let kol = Queue.create () in
    let pusty = Array.make n 0 in
    Queue.add (pusty, 0) kol;
    Hashtbl.add set (Array.copy pusty) 0;

    let prob_dodac (arr, dist) =
      if not (Hashtbl.mem set arr) then
        begin
          let n_arr = Array.copy arr in
          Hashtbl.add set n_arr (dist + 1);
          Queue.add (n_arr, dist + 1) kol;
        end;
    in

    let wylej arr dist =
      Array.iteri (fun i elem ->
        if arr.(i) <> 0 then
          arr.(i) <- 0;
          prob_dodac (arr, dist);
          arr.(i) <- elem;
      ) arr
    in

    let napelnij arr dist =
      Array.iteri (fun i elem ->
        if arr.(i) <> limit.(i) then
          arr.(i) <- limit.(i);
          prob_dodac (arr, dist);
          arr.(i) <- elem;
      ) arr
    in

    let przelej arr dist =
      Array.iteri (fun i elemi ->
        Array.iteri (fun x elemx ->
          if i <> x  && elemx <> limit.(x) then
            let dif = min elemi (limit.(x) - elemx) in
            arr.(x) <- elemx + dif;
            arr.(i) <- elemi - dif;
            prob_dodac (arr, dist);
            arr.(i) <- elemi;
            arr.(x) <- elemx;
        ) arr
      ) arr
    in

    let ret = ref (-1) in
    while not (Queue.is_empty kol || !ret <> -1) do
      let arr, dist = Queue.take kol in
      let arr = Array.copy arr in
      if arr = wyjscie then
        ret := dist
      else
        begin
          przelej arr dist;
          napelnij arr dist;
          wylej arr dist;
        end
    done;
    !ret

(* Testy *)
(* let c = [|(10,2);(20,20);(10,0);(1000,1000)|];; *)
(* assert ( przelewanka c = -1 );; *)
(* let c = [|(3,2);(5,4);(5,2);(6,1)|];; *)
(* assert (przelewanka c = -1);; *)
(* let c = [|(40,1);(10,4);(23,2);(40,1)|];; *)
(* assert (przelewanka c = -1);; *)
(* let c = [|(12,2);(6,3);(4,4);(10,2)|];; *)
(* assert (przelewanka c = -1);; *)
(* let c = [|(14,3);(3,1)|];; *)
(* assert (przelewanka c = -1);; *)

(* let c = [|(3,2);(3,3);(1,0);(12,1)|];; *)
(* assert ( przelewanka c = 4 );; *)
(* let c = [|(1,1);(100,99)|];; *)
(* assert ( przelewanka c = 2 );; *)
(* let c = [|(3,3);(5,4);(5,2);(6,1)|];; *)
(* assert (przelewanka c = 6);; *)
(* let c = [|(100,3);(2,1);(1,0);(6,1)|];; *)
(* assert (przelewanka c = 7);; *)
(* let c = [|(3,3);(5,5);(5,5);(6,6)|];; *)
(* assert (przelewanka c = 4);; *)
(* let c = [|(40,20);(20,10);(10,5);(5,0)|];; *)
(* przelewanka c ;; *)
(* let c = [|(19,3);(1,1);(2,2)|];; *)
(* assert (przelewanka c = 6);; *)
(* let c = [|(14,3);(3,1);(3,0)|];; *)
(* assert (przelewanka c = 13);; *)
(* let c = [|(3,3);(4,0);(1,1);(6,6)|];; *)
(* assert (przelewanka c = 3);; *)
(* let c = [|(46,20);(23,10);(13,5);(5,0)|];; *)
(* assert (przelewanka c = 10);; *)
(* let c = [|(18,3);(3,1);(2,2)|];; *)
(* assert (przelewanka c = 4);; *)
(* let c = [|(14,3);(5,1)|];; *)
(* assert (przelewanka c = -1);; *)
(* let c = [|(14,3);(5,1);(5,0)|];; *)
(* assert (przelewanka c = 16);; *)

(* let c = [|(10000,5000);(1,0)|];; *)
(* assert (przelewanka c = 10000);; *)
(* let c = [|(50000,450);(3,1);(3,0)|];; *)
(* assert (przelewanka c = 33635);; *)
(* let c = [|(100000,25252);(2,2)|];; *)
(* assert (przelewanka c = 25253);; *)
