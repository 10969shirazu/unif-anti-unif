exception Echec of string

type variable = string

type terme =
  | Var of variable
  | Const of string
  | App of string * terme list

             
let rec repetition x t =
  match t with
  | Var y -> x = y
  | App (_, args) -> List.exists (repetition x) args
  | _ -> false

let remplacer s t =
  let rec remplacer' t = match t with
    | Var x -> (try remplacer' (List.assoc x s) with Not_found -> t)  (* la sub peut avoir la forme [("x", Const("a"))]; et donc on a besoin de la clé*)
    | App (f, args) -> App (f, List.map remplacer' args)
    | _ -> t in
  remplacer' t



let rec unifier t1 t2 =
  match t1, t2 with
  | Const c1, Const c2 -> if c1 = c2 then [] else raise (Echec "on ne peut pas unifier les constantes")
  |Var x, Const y -> [(x, t2)]
  | Var x, Var y  ->  if x = y then [] else [(x, t2)]
  | Var x, t | t, Var x ->
  (match t with
  | App (f, args) -> raise (Echec "une variable ne s'unifie pas avec une fonction")
  | _ -> if repetition x t then raise (Echec "occurance") else [(x, t)])

  |Const a, t | t, Const a -> raise (Echec " unification impossible, les constantes ne se remplaçent pas")
  | App (f1, args1), App (f2, args2) ->
      if f1 <> f2 then raise (Echec "impossible d'unifier des fonction differentes") else
        unifier_lists args1 args2 []
  (*| _, _ -> raise (Echec "erreur") *)
  
  
and unifier_lists l1 l2 s =
  match l1, l2 with
  | [], [] -> s
  | t1::r1, t2::r2 -> let s' = unifier (remplacer s t1) (remplacer s t2) in
      unifier_lists (*(List.map (remplacer s') r1) (List.map (remplacer s') r2)  <- j'ai compliqué pour rien *) r1 r2 (concatener_les_subs s' s)
  | _, _ -> raise (Echec "listes avec longeurs differentes")

  

(*and concatener_les_subs s1 s2 =
    let s1' = List.map (fun (x, t) -> (x, remplacer s2 t)) s1 in
    let s2' = List.filter (fun (y, _) -> not (List.mem_assoc y s1)) s2 in 
    s1' @ s2'  *)
   
     
and concatener_les_subs s1 s2 =
 let s2' = List.map (fun (x, t) -> (x, remplacer s1 t)) s2 in (*car s1 contient des paires (var, terme) on veut renvoyer x = terme remplacé *)
 let s1' = List.filter (fun (y, _) -> not (List.mem_assoc y s2)) s1 in
  s1' @ s2' 

    
    
    
    
   let rec imprimer = function
  | Var x -> print_string x
  | Const a -> print_string a
  | App (f, args) ->
      print_string f;
      print_string "(";
      List.iteri (fun i t -> imprimer t; if i < List.length args - 1 then print_string "," else ()) args;
      print_string ")"
      
      
let imprimer_equation (x, t) =
  print_string x;
  print_string " = ";
  imprimer t;
  print_newline ()

let () =

let t1= App("f", [Var"x"; Var"y"; Var"z"]) in 
let t2= App("f", [Var"y"; Var"z"; Const"2"]) in   



(*let t1= App("f", [Var"x"; Var"y"; Var"z"])  in
let t2= App("f", [Var"y"; Var"z"; Var"x"]) in  *)




 (*let t1= App("h", [Var"z"; Var"x"; Var"y"; Var"z"; Var"t"]) in 
  let t2= App("h",[Var"t"; Var"y"; Var"z"; Const"2"; Var"x"]) in   *)
  
  
  
 (* let t1= App("h", [Const"a"; Var"y"; Var"b"; Var"7"; Const"7"]) in 
  let t2= App("h",[Var"x"; Var"b"; Var"z"; Var"w"; Const "7"]) in  *)

 (* let t1 = App("h", [App("f", [Var"x"]); App("g", [Var "y"])]) in
  let t2 = App("h", [App("f", [Var"y"]); App("g", [Var "x"])]) in *)
  
  
  (*let t1 = App("h", [Var"x"; Var"x"; App("g", [Const "a"; Var"z"])]) in 
  let t2 = App("h", [Var"y"; Const "b"; App("g", [Var"y"; Var"y"])]) in   *)
  
 (*let t1= App("f", [Var"x"]) in 
  let t2= Var"x" in  *)
  
  
  (*let t1 = Var"x" in 
  let t2= Var"a" in *)
  
  
  (* repetition: let t1= App("f", [Var"x"; App("h", [Var "y"])]) in 
  let t2= App("f", [App("f", [Var"x"]); Var "s"]) in *)
  
  (*let t1 = App("h", [Var"x"; Var"y"; App("f", [Var"z"])]) in
  let t2= App("h", [Var"k"; Var"c"; App("f", [Const"a"])]) in *)
  let subs = unifier t1 t2 in
  List.iter imprimer_equation subs

