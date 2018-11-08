(*Dodatne vaje z interneta*)

(*funkcija last vrne zadnji element seznama*)
(*last [ "a" ; "b" ; "c" ; "d" ];;*)

let rec last = function
  | [] -> "Ni elementov!"
  | x :: [] -> x 
  | _ :: xs -> last xs

(*dolzina seznama*)

let rec dolzina = function
  | [] -> 0
  | _ :: [] -> 1
  | _ :: xs -> 1 + dolzina xs

  (*reverse obrne seznam*)

  let rec reverse = function
    | [] -> []
    | x :: [] -> [x]
    | x :: xs -> (reverse xs) @ [x] 
