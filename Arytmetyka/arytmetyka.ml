(************************************************)
(* Zadanie o arytmetyce niedokładnych wartości. *)
(************************************************)

(* Autor: Juliusz Korab-Karpowicz *)
(* Recenzent: Michał Wiśniewski*)

(* Chcemy, aby iloczyn zera z (minus)nieskończonością nie był NaNem, ale zerem,
 * ponieważ to upraszcza działanie funkcji skrajne_mozliwosci
 * Dlatego, redefiniujemy iloczyn z uwzględnieniem tej zmiany *)
let ( *. ) a b =
    if (a = 0.0 && (b = infinity || b = neg_infinity))
    || (b = 0.0 && (a = infinity || a = neg_infinity)) then
        0.0
    else
        ( *. ) a b

(* Również, zastępujemy sumę plus i minus nieskończoności, zerem *)
let ( +. ) a b =
    if min a b = neg_infinity && max a b = infinity then
        0.0
    else
        ( +. ) a b

(* Przedział liczb rzeczywistych
 * fst (skrót od first - ang. pierwszy, bo pierwszy element rekordu ) jest dolną granicą przedziału
 * snd (skrót od second - ang. drugi, bo drugi element rekordu) ) jest górną granicą przedziału
 * Jeżeli przedział jest NaNem, fst i snd są oba równe NaN
 * Inaczej, fst jest zawsze <= snd *)
type przedzial =
    { fst   : float;
      snd   : float;
    }

(* Przedział reprezentujący NaN *)
let nan_przedzial = {fst = nan; snd = nan}

(* Funkcja zwracająca minimum i maksimum spośród wyników operacji na krańcowych
 * elementach dwóch przedziałów *)
let skrajne_mozliwosci x y operacja =
    let l = [operacja x.fst y.fst; operacja x.fst y.snd; operacja x.snd y.fst; operacja x.snd y.snd]
    in List.fold_left min infinity l, List.fold_left max neg_infinity l

(* Wartość reprezentujemy jako sumę maksymalnie dwóch przedziałów. Elementy rekordu to:
 * fst (skrót od first - ang. pierwszy, bo pierwszy element rekordu) jest pierwszym przedziałem
 * snd (skrót od second - ang. drugi, bo drugi element rekordu) jest drugim przedziałem
 * Jeśli wartość możemy zapisać tylko jednym przedziałem, to snd przepisujemy nan_przedzial
 * Inaczej, każdy element w fst jest < od każdego elementu w snd
 * Gdy wartość składa się z dwóch predziałów, to dolna granica fst
 * jest równa neg_infinity, a górna granica snd jest równa infinity
 * Gdy wartość jst NaNem, oba przedziały są nan_przedział-ami *)
type wartosc =
    { fst   : przedzial;
      snd   : przedzial;
    }

(* Funkcja sprawdzająca, czy wartość jest złożona z dwóch przedziałów *)
let hassnd a =
    classify_float a.snd.fst != FP_nan

(* Funkcja sprawdzająca, czy wartość zawiera jedynie zero*)
let iszero a =
    a.fst.fst = 0.0 && a.fst.snd = 0.0

(* Funkcja zwracającą parę liczb, w której pierwsza liczba jest nie większa od drugiej *)
let min_max a b =
    if a < b then
        a, b
    else
        b, a

(* Funkcja, która jeżeli 'b' jest sumą dwóch przedziałów, a 'a' jest jedynym przedziałem,
 * zamienia ich kolejność. Zawsze 'a' i 'b' będą się składać z różnej ilości przedziałów *)
let rozdz a b =
    if hassnd a then a, b
    else b, a

(* Jeżeli wartość jest zapisana jako suma dwóch przedziałów, ale te
 * przedziały mają już element wspólny, łączymy je w jeden *)
let merge a =
    if hassnd a && a.fst.snd >= a.snd.fst then
        {fst = {fst = neg_infinity; snd = infinity}; snd = nan_przedzial}
    else
        a

(* Zwaraca minimalną wartość przedziału *)
let min_wartosc x =
    x.fst.fst

(* Zwaraca maksymalną wartość przedziału *)
let max_wartosc x =
    if hassnd x then
        x.snd.snd
    else
        x.fst.snd

(* Tworzy wartość od x do y *)
let wartosc_od_do x y =
    {fst = {fst = x; snd = y}; snd = nan_przedzial }

(* Tworzy wartość zawierającą liczby w zakresie x +/- p% *)
let wartosc_dokladnosc x p =
    let p = p *. 0.01 in
    if x < 0.0 then
        {fst = {fst = x *. (1.0 +. p); snd = x *. (1.0 -. p)}; snd = nan_przedzial}
    else
        {fst = {fst = x *. (1.0 -. p); snd = x *. (1.0 +. p)}; snd = nan_przedzial}

(* Tworzy wartość zawierającą jedynie x *)
let wartosc_dokladna x =
    {fst = {fst = x; snd = x}; snd = nan_przedzial}

(* Czy liczba x jest zawarta w wartości w *)
let in_wartosc w x =
    if hassnd w then
        w.fst.snd >= x || w.snd.fst <= x
    else
        w.fst.fst <= x && w.fst.snd >= x

(* Środkowa wartość przedziału od min_wartosc do max_wartosc,
 * lub NaN jeśli min i max_wartosc nie są określone *)
let sr_wartosc a =
    if hassnd a || (a.fst.fst = neg_infinity && a.fst.snd = infinity) then 
        nan
    else
        (a.fst.fst +. a.fst.snd) /. 2.0

(* Zwraca wartość zawierającą wszystkie możliwe wyniki a + b *)
let plus a b =
    if classify_float a.fst.fst = FP_nan || classify_float b.fst.fst = FP_nan then
        {fst = nan_przedzial; snd = nan_przedzial}
    else if hassnd a && hassnd b then
        {fst = {fst = neg_infinity; snd = infinity}; snd = nan_przedzial}
    else if not (hassnd a || hassnd b) then
        let (mn, mx) = skrajne_mozliwosci a.fst b.fst ( +. ) in
        {fst = {fst = mn; snd = mx}; snd = nan_przedzial}
    else
        let (a, b) = rozdz a b in
        merge
        {fst = {fst = neg_infinity; snd = a.fst.snd +. b.fst.snd};
        snd = {fst = a.snd.fst +. b.fst.fst; snd = infinity}}

(* Zwraca wartość zawierającą wszystkie możliwe wyniki a - b *)
(* Odwołujemy się do funkcji plus, robiąc z b wartość przeciwną*)
let minus a b =
    let b =
        if hassnd b then
            {fst = {fst = neg_infinity; snd = -.b.snd.fst};
            snd = {fst = -.b.fst.snd; snd = infinity}}
        else
            {fst = {fst = -.b.fst.snd; snd = -.b.fst.fst};
            snd = nan_przedzial}
    in plus a b

(* Zwraca wartość zawierającą wszystkie możliwe wyniki a * b *)
(* W zależności od ilości przedziałów dla wartości a i b, rozważamy różne przypadki *)
let razy a b =
    if classify_float a.fst.fst = FP_nan || classify_float b.fst.fst = FP_nan then
        {fst = nan_przedzial; snd = nan_przedzial}
    else if not (hassnd a || hassnd b) then
        let (mn, mx) = skrajne_mozliwosci a.fst b.fst ( *. ) in
        {fst = {fst = mn; snd = mx}; snd = nan_przedzial}
    else if hassnd a && hassnd b then
        if iszero a || iszero b then
            {fst = {fst = 0.0; snd = 0.0}; snd = nan_przedzial}
        else if in_wartosc a 0.0 || in_wartosc b 0.0 then
            {fst = {fst = neg_infinity; snd = infinity}; snd = nan_przedzial}
        else
            merge
            {fst = {fst = neg_infinity; snd = max (a.fst.snd *. b.snd.fst) (b.fst.snd *. a.snd.fst)};
            snd = {fst = min (a.snd.fst *. b.snd.fst) (a.fst.snd *. b.fst.snd); snd = infinity}}
    else
        let (a, b) = rozdz a b in
        if iszero b then
            {fst = {fst = 0.0; snd = 0.0}; snd = nan_przedzial}
        else if in_wartosc b 0.0 then
            {fst = {fst = neg_infinity; snd = infinity}; snd = nan_przedzial}
        else
            merge
            (
            if b.fst.fst < 0.0 then
                {fst = {fst = neg_infinity; snd = max (a.snd.fst *. b.fst.fst) (a.snd.fst *. b.fst.snd)};
                snd = {fst = min (a.fst.snd *. b.fst.fst) (a.fst.snd *. b.fst.snd); snd = infinity}}
            else
                {fst = {fst = neg_infinity; snd = max (a.fst.snd *. b.fst.fst) (a.fst.snd *. b.fst.snd)};
                snd = {fst = min (a.snd.fst *. b.fst.fst) (a.snd.fst *. b.fst.snd); snd = infinity}}
            )

(* Zwraca wartość zawierającą wszystkie możliwe wyniki a / b *)
(* Sprawdzamy warunki brzegowe i wykonujemy mnożenie a przez wartość odwrotną do b *)
let podzielic a b =
    if classify_float a.fst.fst = FP_nan || classify_float b.fst.fst = FP_nan then
        {fst = nan_przedzial; snd = nan_przedzial}
    else
        if iszero b then
            {fst = nan_przedzial; snd = nan_przedzial}
        else if iszero a then
            {fst = {fst = 0.0; snd = 0.0}; snd = nan_przedzial}
        else
            let b = 
                if hassnd b then
                    let (mn, mx) = min_max (1.0 /. b.fst.snd) (1.0 /. b.snd.fst) in
                    if in_wartosc b 0.0 then
                        {fst = {fst = neg_infinity; snd = mn};
                        snd = {fst = mx; snd = infinity}}
                    else
                        {fst = {fst = mn; snd = mx};
                        snd = nan_przedzial}
                else
                    let (mn, mx) = min_max (1.0 /. b.fst.fst) (1.0 /. b.fst.snd) in
                    if b.fst.fst != neg_infinity && b.fst.snd != infinity then
                        if in_wartosc b 0.0 then
                            if b.fst.snd > 0.0 then
                                if b.fst.fst < 0.0 then
                                    {fst = {fst = neg_infinity; snd = mn};
                                    snd = {fst = mx; snd = infinity}}
                                else
                                    {fst = {fst = 1.0 /. b.fst.snd; snd = infinity};
                                    snd = nan_przedzial}
                            else
                                {fst = {fst = neg_infinity; snd = 1.0 /. b.fst.fst};
                                snd = nan_przedzial}
                        else
                            {fst = {fst = mn; snd = mx};
                            snd = nan_przedzial}
                    else if b.fst.fst = neg_infinity then
                        if b.fst.snd = infinity then
                            {fst = {fst = neg_infinity; snd = infinity};
                            snd = nan_przedzial}
                        else
                            {fst = {fst = 1.0 /. b.fst.snd; snd = 0.0};
                            snd = nan_przedzial}
                    else
                        {fst = {fst = 0.0; snd = 1.0 /. b.fst.fst};
                        snd = nan_przedzial}
            in razy a b

(*********)
(* Testy *)
(*********)

(* let ( =. ) (x : float) (y : float) = *)
(* let e = 1e-6 and d = x -. y in ~-.e < d && d < e;; *)
(* let a = min_wartosc ( podzielic ( wartosc_dokladnosc (4.000000) (7.000000) ) ( wartosc_od_do (-7.000000) (-2.000000) ) ) ;; *)
(* assert (a =. -2.14);; *)
(* let a = max_wartosc ( podzielic ( minus ( wartosc_dokladna (7.000000) ) ( wartosc_dokladna (-1.000000) ) ) ( wartosc_dokladna (9.000000) ) ) ;; *)
(* assert (a =. 0.88888888888888884);; *)
(* let a = min_wartosc ( plus ( plus ( wartosc_dokladnosc (2.000000) (0.000000) ) ( wartosc_dokladna (6.000000) ) ) ( wartosc_od_do (-4.000000) (5.000000) ) ) ;; *)
(* assert (a =. 4.);; *)
(* let a = max_wartosc ( podzielic ( podzielic ( wartosc_dokladna (5.000000) ) ( wartosc_od_do (-2.000000) (-1.000000) ) ) ( wartosc_dokladna (8.000000) ) ) ;; *)
(* assert (a =. -0.3125);; *)
(* let a = min_wartosc ( podzielic ( wartosc_dokladnosc (-1.000000) (3.000000) ) ( razy ( wartosc_dokladna (-3.000000) ) ( plus ( podzielic ( plus ( minus ( wartosc_dokladnosc (9.000000) (1.000000) ) ( wartosc_dokladna (-1.000000) ) ) ( plus ( wartosc_od_do (-3.000000) (0.000000) ) ( wartosc_od_do (-4.000000) (0.000000) ) ) ) ( podzielic ( podzielic ( wartosc_dokladna (-4.000000) ) ( wartosc_dokladnosc (-10.000000) (4.000000) ) ) ( wartosc_dokladna (5.000000) ) ) ) ( wartosc_dokladna (3.000000) ) ) ) ) ;; *)
(* assert (a =. 0.00240987801545303236);; *)
(* let a = min_wartosc ( razy ( wartosc_dokladnosc (4.000000) (9.000000) ) ( podzielic ( wartosc_dokladna (-9.000000) ) ( plus ( razy ( minus ( wartosc_dokladnosc (-9.000000) (6.000000) ) ( wartosc_dokladna (-6.000000) ) ) ( wartosc_od_do (-9.000000) (5.000000) ) ) ( wartosc_dokladnosc (5.000000) (2.000000) ) ) ) ) ;; *)
(* assert (a = neg_infinity);; *)
(* let a = in_wartosc ( minus ( wartosc_od_do (-7.000000) (-4.000000) ) ( wartosc_dokladna (0.000000) ) ) (-1.000000);; *)
(* assert (a = false);; *)
(* let a = sr_wartosc ( plus ( wartosc_dokladnosc (-4.000000) (5.000000) ) ( wartosc_dokladna (-6.000000) ) ) ;; *)
(* assert (a =. -10.);; *)
(* let a = min_wartosc ( podzielic ( wartosc_dokladnosc (-8.000000) (8.000000) ) ( wartosc_od_do (-3.000000) (0.000000) ) ) ;; *)
(* assert (a =. 2.45333333333333314);; *)
(* let a = min_wartosc ( razy ( wartosc_od_do (6.000000) (7.000000) ) ( wartosc_od_do (-2.000000) (0.000000) ) ) ;; *)
(* assert (a =. -14.);; *)
(* let a = sr_wartosc ( minus ( minus ( wartosc_od_do (-10.000000) (-7.000000) ) ( plus ( wartosc_dokladna (0.000000) ) ( wartosc_od_do (4.000000) (7.000000) ) ) ) ( wartosc_dokladna (6.000000) ) ) ;; *)
(* assert (a =. -20.);; *)
(* let a = max_wartosc ( minus ( wartosc_dokladna (2.000000) ) ( wartosc_dokladna (-6.000000) ) ) ;; *)
(* assert (a =. 8.);; *)
(* let a = min_wartosc ( minus ( wartosc_dokladna (3.000000) ) ( wartosc_dokladnosc (7.000000) (9.000000) ) ) ;; *)
(* assert (a =. -4.63);; *)
(* let a = sr_wartosc ( podzielic ( wartosc_od_do (-8.000000) (8.000000) ) ( wartosc_od_do (-8.000000) (0.000000) ) ) ;; *)
(* assert ((classify_float a) == FP_nan);; *)
(* let a = in_wartosc ( plus ( wartosc_dokladna (2.000000) ) ( podzielic ( wartosc_dokladnosc (0.000000) (9.000000) ) ( wartosc_dokladnosc (1.000000) (8.000000) ) ) ) (4.000000);; *)
(* assert (a = false);; *)
(* let a = in_wartosc ( minus ( wartosc_dokladna (0.000000) ) ( wartosc_dokladnosc (9.000000) (5.000000) ) ) (8.000000);; *)
(* assert (a = false);; *)
(* let a = min_wartosc ( razy ( wartosc_dokladna (6.000000) ) ( wartosc_dokladnosc (3.000000) (8.000000) ) ) ;; *)
(* assert (a =. 16.56);; *)
(* let a = min_wartosc ( podzielic ( minus ( wartosc_od_do (-3.000000) (0.000000) ) ( wartosc_od_do (-6.000000) (-3.000000) ) ) ( wartosc_dokladnosc (6.000000) (7.000000) ) ) ;; *)
(* assert (a =. 0.);; *)
(* let a = max_wartosc ( plus ( razy ( podzielic ( wartosc_od_do (6.000000) (7.000000) ) ( plus ( wartosc_od_do (-9.000000) (0.000000) ) ( wartosc_od_do (0.000000) (5.000000) ) ) ) ( wartosc_dokladnosc (0.000000) (0.000000) ) ) ( wartosc_od_do (-8.000000) (5.000000) ) ) ;; *)
(* assert (a =. 5.);; *)
(* let a = in_wartosc ( minus ( razy ( podzielic ( razy ( wartosc_dokladnosc (1.000000) (3.000000) ) ( plus ( wartosc_dokladnosc (0.000000) (0.000000) ) ( wartosc_od_do (-10.000000) (0.000000) ) ) ) ( wartosc_od_do (-1.000000) (5.000000) ) ) ( podzielic ( wartosc_dokladnosc (9.000000) (2.000000) ) ( razy ( wartosc_dokladna (3.000000) ) ( wartosc_dokladna (4.000000) ) ) ) ) ( wartosc_dokladnosc (0.000000) (9.000000) ) ) (0.000000);; *)
(* assert (a = true);; *)
(* let a = podzielic (wartosc_dokladna 1.) (plus (podzielic (wartosc_dokladna 1.) (wartosc_od_do ~-.1. 1.)) (wartosc_dokladna 2.));; *)
(* assert (in_wartosc a 0.99 = false);; *)
