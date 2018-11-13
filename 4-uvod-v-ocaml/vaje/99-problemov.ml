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

let dolzina_seznama list = 
  let rec dolzina acc = function
    | [] -> acc
    | _ :: xs -> dolzina (acc + 1) xs
    in
    dolzina 0 list

(*vsota elementov seznama*)
(*vsota_elementov_seznama [1; 2; 3];;*)

let vsota_elementov_seznama list = 
  let rec vsota acc = function
    | [] -> acc
    | x :: xs -> vsota (acc + x) xs
    in
    vsota 0 list

(*preslika nek seznam s pomočjo neke funkcije*)
(*preslikaj_seznam succ [1; 2; 3;];;*)

let preslikaj_seznam f list = 
  let rec preslikaj acc = function
    | [] -> List.rev acc
    | x :: xs -> preslikaj (f x :: acc) xs
    in
    preslikaj [] list

(*fakulteta stevila n*)
(*fakulteta 99;;*)

let fakulteta n = 
  let rec fakulteta' acc n = 
    if n <= 0 then acc else fakulteta' (n * acc) (n-1)
    in
    fakulteta' 1 n

(*reverse obrne seznam*)
(*obrni ["a" ; "b" ; "c"];;*)

let reverse list =
  let rec reverse' acc = function
    | [] -> acc
    | x :: xs -> reverse' (x :: acc) xs
    in
    reverse' [] list

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
["a"; "b"; "d"; "e"; "g"; "h"; "j"]
izpusti vsak tretji element*)

let drop list n = 
  let rec aux acc st = function
    | [] -> reverse acc
    | x :: xs -> if st = n then aux acc 1 xs else aux (x :: acc) (st+1) xs
    in
    aux [] 1 list

(*izloči k-ti element
remove_at 1 ["a";"b";"c";"d"];;
["a"; "c"; "d"]*)

let rec remove_at n list = 
  match list with
  | [] -> []
  | x :: xs -> if n = 0 then xs else x :: remove_at (n-1) xs

(*vstavi dani element na k-to mesto v seznamu
insert_at "alfa" 1 ["a";"b";"c";"d"];;
["a"; "alfa"; "b"; "c"; "d"]*)

let rec insert_at element k list = 
  match list with
  | [] when k=0 -> element :: []
  | [] -> []
  | x :: xs -> if k = 0 then x :: element :: xs else x :: insert_at element (k-1) xs
