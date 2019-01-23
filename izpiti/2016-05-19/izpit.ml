type 'a drevo = Drevo of 'a * 'a drevo list

let test_tree = Drevo (18, [Drevo (12, []); Drevo (10, [Drevo (5, [])]); Drevo (8, [Drevo (1, []); Drevo (3, [])])])

let ni_kopica = Drevo (18, [Drevo (12, []); Drevo (10, [Drevo (5, [])]); Drevo (8, [Drevo (1, []); Drevo (9, [])])])

let rec vsota = function
  | Drevo (x, []) -> x
  | Drevo (x, Drevo (y, ys) :: xs) -> x + vsota (Drevo (y, ys @ xs))

let rec sum = function
  | Drevo (x, xs) -> 
  let rec aux acc = function
  | [] -> acc
  | Drevo (y, sez) :: ys -> aux (acc + y) (sez @ ys) 
  in
  aux x xs

let rec je_kopica = function
  | Drevo (x, xs) ->
  let rec aux acc = function
  | [] -> true
  | Drevo (y, sez) :: ys -> 
  if y >= acc then false 
  else 
  aux acc ys && aux y sez
  in
  aux x xs

let rec maksimum = function
  | Drevo (x, xs) -> 
  let rec aux acc = function
  | [] -> acc
  | Drevo (y, ys) :: sez -> if y > acc then aux y (ys @ sez) else aux acc (ys @ sez)
  in
  aux x xs

let rec odstrani_max = function
  | Drevo (x, xs) -> 
  let najvecji = x in
  match xs with
  | [] -> (najvecji, None)
  | Drevo (y, sez) :: ys -> 
  let rec aux trenutni = 

  
