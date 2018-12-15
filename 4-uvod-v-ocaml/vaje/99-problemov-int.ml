(*Arithmetic*)

(*ali je praštevilo ali ne*)

let is_prime n =
  let n = abs n in
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
  n <> 1 && is_not_divisor 2;;

(*fakulteta stevila n*)
(*fakulteta 99;;*)

let fakulteta n = 
  let rec fakulteta' acc n = 
    if n <= 0 then acc else fakulteta' (n * acc) (n-1)
    in
    fakulteta' 1 n