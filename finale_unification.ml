#use "unif.ml";;


 let rec unif t1 t2 =
  let res = unifier t1 t2 in
  match t1 with
  | Var x -> imprimer (remplacer res t1)
  | Const a -> imprimer (Const a)
  | App (f, terme::args) ->
    let remplacer_terme = remplacer res terme in
    let remplacer_args = List.map (remplacer res) args in
    imprimer (App (f, remplacer_terme :: remplacer_args));
    print_newline()
  |_ -> raise (Echec"erreur")

 (* let t1= App("h", [Var"z"; Var"x"; Var"y"; Var"z"; Var"t"]) 
  let t2= App("h",[Var"t"; Var"y"; Var"z"; Const"2"; Var"x"])   *)
  
(* let t1 = App("h", [Var "x"; App("f", [Var "y"] ); Var"b"; App("k", [Var "s"])])
let t2 = App("h", [Var "u"; App("f", [Var "u"]); App("f", [Var "b"]); App("k", [Var "s"])])  *) 

 (* let t1= App("h", [Var"z"; Var"x"; Var"y"; Const"2"; Var"t"]) 
  let t2= App("h",[Var"z"; Var"y"; Var"z"; Const"2"; Var"x"]) *)
 
 
 
 (* let t1 = App("f", [Var"x"]) 
  let t2 = App("f", [Var"y"])   *)
  
  
  (* let t1= App("h", [Const"a"; Var"y"; Const"b"; Const"a"]) 
  let t2= App("h",[Var"x"; Const"b"; Var"z"; Var"w"]) *)
  
  
  
(* let t1= App("f", [Var"x"; Var"y"; Var"z"])
let t2=App("f", [Var"k"; Var"y"; Var"c"])  *)
  
let t1= App("f", [Var"x"; Var"y"; Var"z"]) 
let t2= App("f", [Var"y"; Var"z"; Const"2"]) 

(*let t1= App("f", [Var"x"; Var"y"; Var"z"]) 
let t2= App("f", [Var"y"; Var"z"; Var"x"]) *)
   
 
  (* let t1 = App("h", [Var"x"; Var"x"; App("g", [Const "a"; Var"z"])]) 
  let t2 = App("h", [Var"y"; Const "b"; App("g", [Var"y"; Var"y"])])    *)
  
  
  
  (*  let t1= App("h", [Const"7"; Var"y"; Var"b"; Var"7"; Var"a"]) 
  let t2= App("h",[Const"7"; Var"b"; Var"z"; Var"w"; Var"a"])  *)
  
  
  (* let t1= App("f", [Var"x"]) 
  let t2= Var"x" *)

let a = unif t1 t2

