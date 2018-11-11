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

let obrni list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
    in
    aux [] list

(*ali je seznam palindrom*)
(*je_palindrom [ "x" ; "a" ; "m" ; "a" ; "x" ];;*)

let je_palindrom list =
  let obrnjen = obrni list
  in
  list = obrnjen

(*izbrisi ponavlajoce elemente*)
(*pocisti ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;*)

let pocisti list = 
  let rec element acc = function
    | [] -> acc
    | x :: xs -> if 
