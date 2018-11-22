type izraz =
  | Stevilo of int
  | Minus of izraz
  | Plus of izraz * izraz
  | Krat of izraz * izraz

let rec izracunaj = function
  | Stevilo n -> n
  | Minus izr -> -(izracunaj izr)
  | Plus (izr1, izr2) -> izracunaj izr1 + izracunaj izr2
  | Krat (izr1, izr2) -> izracunaj izr1 * izracunaj izr2

type 'a seznam =
  | Prazno
  | Sestavljeno of 'a * 'a seznam

let sez =  Sestavljeno (1, Sestavljeno (2, Sestavljeno (3,Prazno)))

let rec pretvori_seznam = function
  | Prazno -> []
  | Sestavljeno (x, xs) -> x :: pretvori_seznam xs 

type niz =
  | Prazen
  | Sestavljen of string * niz

let ime = Sestavljen ( "j", Sestavljen ("a", Sestavljen ("k", Sestavljen ("a", Prazen))));;

let rec pretvori_niz = function
  | Prazen -> ""
  | Sestavljen (glava, rep) -> glava ^ pretvori_niz rep
