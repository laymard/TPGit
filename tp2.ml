(*TP2 : Recursive*)
(*AYMARD Laurent & GIRAUDET Tom *)

(*EX 1 : Pair et impair *)

let rec  pair = function
  |0 -> true
  |1 -> false
  |n -> pair (pred (pred n));;

let impair = function n -> not (pair n);;

(* EX 2 : Variation *)
let rec sigma = function (a,b) -> if a>b then 0 else a + sigma(a+1,b);;

let rec sigma2 = function f -> function (a,b) -> if a>b then 0 else (f a) + sigma2 f ((a+1),b);;
(* val sigma2 : (int -> int) * (int * int) -> int = <fun>*)

let rec sigma3 = function f -> function (deb, inc, fin) -> function (fc, elementneutre) ->
  if deb > fin then elementneutre else (sigma3 f (deb+inc,inc,fin) (fc, (fc (f (deb)) elementneutre))) ;; 

let rec sigma4 = function f -> function (pred,finc) -> function (fc,vi) -> function a -> 
  if not (pred a) then vi else (sigma4 f (pred, finc) (fc , (fc (f a) vi)) (finc a));;

let cum = function f -> function (aa,bb,dx) -> function (fc,vi) ->
sigma4 f ((fun a -> if  a>bb then false else  true),(fun a -> a+.dx)) (fc,vi) aa;;

let integre = function f -> function (a,b,dx) -> 
cum (function roger -> ((f roger)*.dx)) (a,b,dx) ((function a -> function b -> a+.b),0.0) ;;


let rec maxi = function f -> function (a,b) ->
  let dx = 0.001 in
if a = b then f a 
else if f(a+.dx)< f(b-.dx) then maxi f (a+.dx,b)
      else maxi f (a, b-.dx);;



pair 4;;
pair 5;;
impair 3;;
sigma (1,6);;
sigma (6,1);;
sigma2 (fun x -> 2*x) (2,4);;
sigma3 (fun x ->2*x) (2,1,4) ((fun a b -> a+b),0);;
sigma3 (fun x -> x * x)(0,2,10)((fun x acc -> x :: acc),[]);;
sigma4 (fun x -> x * x) ((fun y -> if y>10 then false else true),(fun y -> y+2)) ((fun x acc -> x :: acc),[]) 0;;
sigma4 (fun x -> x) ((fun y -> if y>6 then false else true), (fun y -> y+1)) (( fun x y -> x + y),0) 1;;
cum (fun x -> x) (0.5,2.5, 0.25) ((fun a -> fun b -> a+.b),0.);;
integre (fun x -> 1. /. x) (1.,2.,0.001);;

