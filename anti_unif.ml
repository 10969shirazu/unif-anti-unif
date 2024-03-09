exception Anti of string

type terme =
  | Var of string
  | App of string * terme list




let generate v =
  let n = 1000 in
  let suffix = string_of_int (Random.int n) in
  Var (v ^ "_" ^ suffix)


let rec anti_unif t1 t2 =
 if t1 = t2 then t1
 else  match (t1, t2) with
  | Var x, Var y -> generate x
  | App (f, argument1), App (g, argument2) ->
      if f <> g then raise (Anti "Fonctions différentes, pas de structure commune")
      else
        let rec anti_unif_args argument1 argument2 =
          match (argument1, argument2) with
          | [], [] -> []
          | _ :: _, [] | [], _ :: _ -> raise (Anti "Listes d'arguments de longueurs différentes donc des arités differentes")
          | arg1 :: rest1, arg2 :: rest2 ->
 
              let anti_arg = anti_unif arg1 arg2 in
              anti_arg :: anti_unif_args rest1 rest2
        in
        App (f, anti_unif_args argument1 argument2) 
       | Var x, App(f, args) | App(f, args), Var x -> raise (Anti"on n'unifie pas une variable et une fonction ") 
  






        



let rec imprimer = function
  | Var x -> print_string x
  | App (f, args) ->
      print_string f;
      print_string "(";
      List.iteri (fun i t -> imprimer t; if i < List.length args - 1 then print_string "," else ()) args;
      print_string ")";
      print_newline()
 (*     h(x, f(y), b, k(s)) et h(u, f(u), f(b), k(s))      *)
(*let t1 = App("h", [Var "x"; App("f", [Var "y"] ); Var"b"; App("k", [Var "s"])])
let t2 = App("h", [Var "u"; App("f", [Var "u"]); App("f", [Var "b"]); App("k", [Var "s"])])  *)




let t1= App("f", [Var"x"; Var"y"; Var"z"]) 
let t2= App("f", [Var"y"; Var"z"; Var"2"])  

(* let t1 = App("h", [Var"x"; Var"y"; App("h", [Var"z"])])
let t2 = App("h", [Var"a"; Var"b"; App("h", [Var"z"])]) *)


 (* h(y,x,y,z,t) et h(z,a,b,c,d *)
 (* let t1= App("h", [Var"y"; Var"x"; Var"y"; Var"z"; Var"t"]) 
  let t2= App("h",[Var"z"; Const"a"; Const"b"; Var"c"; Var"d"]) *)
  
  
   (* let t1= App("f", [Var"x"; Var"y"; Var"z"])
  let t2=App("f", [Var"k"; Var"y"; Var"c"]) *)
  
  
    (* let t1 = App("h", [Var"x"; Var"x"; App("g", [Var "a"; Var"z"])]) 
     let t2 = App("h", [Var"y"; Var "b"; App("g", [Var"y"; Var"y"])])  *)
  
  (* let t1= App("h", [Var"z"; Var"x"; Var"y"; Var"2"; Var"t"]) 
  let t2= App("h",[Var"z"; Var"y"; Var"z"; Var"2"; Var"x"])   *) 
  
    (* let t1 = App("f", [Var"x"]) 
  let t2 = App("f", [Var"y"])  *)
  
 (* let t1 = App("h", [Var"x"; Var"x"; App("g", [Var "a"; Var"z"])]) 
  let t2 = App("h", [Var"y"; Var "b"; App("g", [Var"y"; Var"y"])])
*)
   
   
      
 (*     (* f(x,g(y)) et f(h(z), y*)
let t1 = App("f", [Var "x"; App("g", [Var "y"]) ])
let t2 = App("f", [Var "x"; Var "y"])     *)
 (*     
let t1 = App ("f", [Var "x"; Var "x"; Var "z"])
let t2 = App ("f", [App ("g", [Var "x"]); App ("g", [Var "y"])])  checker les arrités differentes *)
let res = anti_unif t1 t2
let () = imprimer res


