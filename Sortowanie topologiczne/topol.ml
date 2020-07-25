(* Sortowanie topologiczne *)
(* Autor: Juliusz Korab-Karpowicz *)
(* Reviewer: Karol Zagródka*)

open PMap

(* Wyjątek gdy podany graf nie jest acykliczny, co uniemożliwia zdefiniowanie
 * sortowania topologicznego *)
exception Cykliczne

(* Sortowanie topologiczne DAG-u:
 * Wejście - Lista krawędzi grafu w postaci par (żródło,lista sąsiadów)
 * Wyjście - Lista wierzchołków grafu w porządku topologicznym *)
let topol l =
  let l = List.sort compare l in
  (* Lista sąsiedztw *)
  let adj = List.fold_left (fun adj (node, ls) -> 
    if not (mem node adj) then
      add node ls adj
    else
      add node (List.rev_append (find node adj) ls) adj
  ) empty l in
  (* Mapa przechowująca wejściowy stopień wierzchołków *)
  let in_deg = List.fold_left (fun in_deg (_, ls) -> 
    List.fold_left (fun in_deg node ->
      if not (mem node in_deg) then
        add node 1 in_deg
      else
        add node (find node in_deg + 1) in_deg
    ) in_deg ls
  ) empty l in
  (* Lista (stos) wierzchołków z zerowym wejściowym stopniem *)
  let zer = List.fold_left (fun zer (node, _) ->
    if not (mem node in_deg) && (zer = [] || List.hd zer <> node) then
      node::zer
    else
      zer
  ) [] l in
  (* Algorytm Kahn'a *)
  let rec kahn zer in_deg ret =
    match zer with
    | [] ->
      if not (is_empty in_deg) then
        raise Cykliczne
      else
        ret
    | cur::zer ->
      let (zer, in_deg) =
        if mem cur adj then
          List.fold_left (fun (zer, in_deg) node ->
          if find node in_deg = 1 then
            node::zer, (remove node in_deg)
          else
            zer, (add node (find node in_deg - 1) in_deg)
        ) (zer, in_deg) (find cur adj)
        else
          zer, in_deg
      in kahn zer in_deg (cur::ret)
  in
  List.rev (kahn zer in_deg [])

(* Testy *)

(* let czy_cykliczne l = *)
(*    match (try (topol l) with *)
(*       Cykliczne -> []) with *)
(*          | [] -> true *)
(*          | _ -> false *)
(* let test input output = *)
(*    let rec loop a b f = function *)
(*       | [] -> false *)
(*       | h::t -> *) 
(*          if f then *) 
(*             if h = b then true *) 
(*             else loop a b f t *)
(*          else if h = a then loop a b true t *) 
(*             else loop a b f t *)
(*    and pom i a = function *)
(*       | [] -> (match i with *)
(*          | [] -> true *)
(*          | g::o -> pom o (fst g) (snd g)) *)
(*       | h::t -> match (loop a h false output) with *)
(*          | true -> pom i a t *)
(*          | false -> false in *)
(*    pom (List.tl input) (fst (List.hd input)) (snd (List.hd input)) *)
(* let a = [(1, [2]); (2, [3]); (3, [4]); (4, [1])] *)
(* let b = [(1, [2]); (2, [3]); (3, [4])] *)
(* let c = [('A', ['B'; 'C'; 'E']); ('D', ['F'; 'E'; 'G']); ('B', ['C'; 'D']); *)
(*    ('C', ['D'; 'F']); ('F', ['G'; 'H'])] *)
(* let d = [("zolty", ["niebieski"; "bialy"; "czarny"]); ("bialy", ["czarny"]); *) 
(*    ("czarny", []); ("czerwony", ["zielony"; "zolty"; "niebieski"; "czarny"])] *)
(* let e = [(1, [2; 5; 8; 3]); (5, [8; 6; 4; 7]); (7, [6; 9; 2]); (8, [6; 9; 3])] *)
(* let _ = assert(czy_cykliczne a); *)
(*         assert(not (czy_cykliczne b)); *)
(*         assert(test b (topol b)); *)
(*         assert(test c (topol c)); *)
(*         assert(test (List.tl c) (topol (List.tl c))); *)
(*         assert(test d (topol d)); *)
(*         assert(test e (topol e)); *)
(*         assert(test (List.tl e) (topol (List.tl e))); *)
(*         assert(test (b @ e) (topol (b @ e))); *)
(*         assert(test (List.tl b @ e) (topol (List.tl b @ e))) *)
