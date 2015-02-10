(*10/02/14*)
(*LES TYPES*)
open List;;
open Graphics;;

(*Types anonymes*)
(4,true,1.0);;

(*Types produits nommés*)
type point = {x : float ; y : float};;
let p1 = {x = 1.; y= 2.};;
p1.x;;

let symdiag = fun { x=a ; y=b} -> {x=b ; y=a};;
symdiag p1;;

let symdiag2 p = {x=p.y ; y=p.x};;
symdiag2 p1;;

let symdiag3 = function
  |{x=0.; y=b} -> {x=0. ; y=b}


type cercle = {centre : point ; rayon :float};;

let ycercle = function
  |{centre = {x=0. ; y=_}; rayon =_} -> true
  |_-> false;;

(*Les types Sommes*)
type direction = Nord | Sud | Est | Ouest;;
type direction2 = 1|2; (*on ne peu pas mettre des valeurs ou des Types*)
 (*Majuscules obligé!!!/*)
let d = Nord;;
let agauche = function
  |Nord->Ouest
  |Ouest -> Sud
  |Sud ->Est
  |Est-> Nord;;

agauche Nord;;

type personne = Nom of string | Nums of int;;
let nom s = Nom s;;

µom "Aµlan";;
nom "Alan";;

type couleur = Trefle|Carreau|Coeur|Pique;;
type hauteur = As|Roi|Dame|Valet|Diw|Neuf|Huit|Sept;;
type carte = Carte of hauteur * couleur ;;

let is_valet c = match c with
  |Carte(Valet,_) -> true
  |_->false;;

let _ =is_valet (Carte(Roi,Pique));;

(*LES LISTES*)
let f x = x*2;;
(map f) (4::5::[]) ;;

let rec map t =function
  |[] -> []
  |(a::reste) -> (t a):: map t reste;;

