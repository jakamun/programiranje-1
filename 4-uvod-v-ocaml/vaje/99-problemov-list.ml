(*Dodatne vaje z interneta*)

(*funkcija last vrne zadnji element seznama*)
(*last [ "a" ; "b" ; "c" ; "d" ];;*)

let rec last = function
  | [] -> failwith "Ni elementov!"
  | x :: [] -> x 
  | _ :: xs -> last xs

(*Vrne zadna dva elementa seznama kot nabor*)
(*zadna_dva [ "a" ; "b" ; "c" ; "d" ];;*)

let rec zadna_dva = function
  | [] | _ :: [] -> failwith "Prekratek seznam!"
  | x :: y :: [] -> (x, y)
  | x :: xs -> zadna_dva xs

(*vrni k-ti element indiksiramo od 0 naprej*)
(*element 3 [ "a" ; "b"; "c"; "d"; "e" ];;*)

let rec element k = function
  | [] -> failwith "Napaka!"
  | x :: xs when k <= 0 -> x
  | x :: xs -> element (k-1) xs

(*dolzina seznama*)
(*dolzina_seznama [ "a" ; "b" ; "c"];;*)

let rec dolzina = function
  | [] -> 0
  | x :: xs -> 1 + dolzina xs

(*repno rekurzivna*)
let dolzina_seznama list = 
  let rec aux acc = function
    | [] -> acc
    | _ :: xs -> aux (acc + 1) xs
    in
    aux 0 list

(*vsota elementov seznama*)
(*vsota_elementov_seznama [1; 2; 3];;*)

let vsota_elementov_seznama list = 
  let rec vsota acc = function
    | [] -> acc
    | x :: xs -> vsota (acc + x) xs
    in
    vsota 0 list

(*reverse obrne seznam*)
(*obrni ["a" ; "b" ; "c"];;*)

let rec obrni = function
  | [] -> []
  | x :: xs -> (obrni xs) @ [x]

(*repno rekurzivna*)
let reverse list =
  let rec reverse' acc = function
    | [] -> acc
    | x :: xs -> reverse' (x :: acc) xs
    in
    reverse' [] list

(*preslika nek seznam s pomočjo neke funkcije*)
(*preslikaj_seznam succ [1; 2; 3;];;*)

let rec map f list =
  match list with
  | [] -> []
  | x :: xs -> f x :: map f xs 

(*repno rekurzivna*)
let preslikaj_seznam f list = 
  let rec preslikaj acc = function
    | [] -> List.rev acc
    | x :: xs -> preslikaj (f x :: acc) xs
    in
    preslikaj [] list

(*ali je seznam palindrom*)
(*je_palindrom [ "x" ; "a" ; "m" ; "a" ; "x" ];;*)

let je_palindrom list =
  let obrnjen = reverse list
  in
  list = obrnjen

(*izbrisi ponavlajoce elemente*)
(*pocisti ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
["a"; "b"; "c"; "d"; "e"]*)

let rec je_element y = function
  | [] -> false
  | x :: xs -> if y = x then true else je_element y xs

let pocisti list = 
  let rec aux acc = function
    | [] -> reverse acc
    | x :: xs -> if je_element x acc == true then aux acc xs else aux (x :: acc) xs
    in
    aux [] list

(*pobrisi samo grupirane pojavitve*)

let rec compress list =
  let rec aux acc sez =
  match acc, sez with
  | acc, [] -> List.rev acc
  | [], x :: xs -> aux (x :: acc) xs
  | y :: ys as acc, x :: xs -> if y = x then aux acc xs else aux (x :: acc) xs
  in
  aux [] list

(*naredi seznam seznamov grupiranih elementov
pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]*)

let pack list =
  let rec aux trenutni acc sez = 
    match trenutni, acc, sez with
    | [], acc, [] -> reverse acc
    | trenutni, acc, [] -> reverse (trenutni :: acc)
    | [], acc, x :: xs -> aux [x] acc xs
    | y :: ys as t, acc, x :: xs -> if y = x then aux (x :: t) acc xs else aux [x] (t :: acc) xs
    in
    aux [] [] list


(*nabor_pojavitev vrne seznam naborov kjer je v vsakem naburu na drugem mestu element seznam in na prvem kolikrat se pojavi*)
(*nabor_pojavitev ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]*)

let rec prestej_pobrisi y list =
  let rec aux st acc = function
    | [] -> (st, reverse acc)
    | x :: xs -> if y = x then aux (st+1) acc xs else aux st (x :: acc) xs
    in
    aux 0 [] list

let rec nabor_pojavitev list = 
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> 
    let st, sez = prestej_pobrisi x xs
    in
    aux ((st+1, x) :: acc) sez
  in
  aux [] list

(*podvoji elemente seznama*)
(*podvoji ["a";"b";"c";"c";"d"];;*)

let podvoji list = 
  let rec aux acc = function
    | [] -> reverse acc
    | x :: xs -> aux (x :: x :: acc) xs
    in
    aux [] list

(*dodaj n istih elementov v seznam
replicate ["a";"b";"c"] 3;;*)

let replicate list n = 
  let rec aux acc st = function
    | [] -> reverse acc
    | x :: xs -> if st > 0 then aux (x :: acc) (st-1) (x :: xs) else aux acc n xs
    in
    aux [] n list

(*izpusti vsak n-ti element seznama
drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
["a"; "b"; "d"; "e"; "g"; "h"; "j"]*)

let drop list n = 
  let rec aux acc st = function
    | [] -> reverse acc
    | x :: xs -> if st = n then aux acc 1 xs else aux (x :: acc) (st+1) xs
    in
    aux [] 1 list

(*razbij seznam na dva seznam dolzina prvega seznama je podana
split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])*)

let split list n =
  match list with
  | [] -> if n = 0 then ([], []) else failwith "Seznam je prazen"
  | x :: xs -> 
  let rec aux acc st = function
  | [] -> if st = n then (List.rev acc, []) else failwith "Seznam je prekratek"
  | x :: xs -> if st = n then (List.rev (x ::acc), xs) else aux (x :: acc) (st+1) xs
  in
  aux [] 1 list

(*rezreže seznam na i k rezino
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
["c"; "d"; "e"; "f"]*)

(*če izkoristim funkcijo slice*)
let slice list i k = 
  let (_, slice1) = split list i in
  let (slice2, _) = split slice1 (k-i) in
  slice2

(*zavrti seznam za n mest v levo
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
- : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]*)

let rotate list n = 
  let (list1, list2) = split list n in
  list2 @ list1

(*izloči k-ti element
remove_at 1 ["a";"b";"c";"d"];;
["a"; "c"; "d"]*)

let rec remove_at n list = 
  match list with
  | [] -> []
  | x :: xs -> if n = 0 then xs else x :: remove_at (n-1) xs

(*repno rekurzivna*)
let odstrani n list = 
  let rec aux acc n = function
  | [] -> if n=0 then acc else failwith "Seznam je prekratek"
  | x :: xs -> if n = 0 then (List.rev acc) @ xs else aux (x :: acc) (n-1) xs
  in
  aux [] n list

(*vstavi dani element na k-to mesto v seznamu
insert_at "alfa" 1 ["a";"b";"c";"d"];;
["a"; "alfa"; "b"; "c"; "d"]*)

let rec insert_at element k list = 
  match list with
  | [] when k=0 -> element :: []
  | [] -> []
  | x :: xs -> if k = 0 then x :: element :: xs else x :: insert_at element (k-1) xs

(*ustvari seznam, ki vsebuje vsa stevila na danem intervalu
range 4 9;;
- : int list = [4; 5; 6; 7; 8; 9]*)

let rec range i k =
  if i = k
  then k :: []
  else if i > k 
  then i :: range (i-1) k
  else i :: range (i+1) k
