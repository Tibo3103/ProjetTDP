open Graphstruct
open Lang
open Instr

type environment = { types: db_tp; bindings: (vname * label) list }

let initial_environment gt = { types = gt; bindings = [] }
let initial_result gt = Result.Ok (initial_environment gt)

exception FieldAccError of string
exception TypeError of string

type tc_result = (environment, string list) result

(* Fonctions pour manipuler l'environnement *)

let add_var vn t (env: environment) = 
  { env with bindings = (vn, t) :: env.bindings }

let remove_var vn env = 
  { env with bindings = List.remove_assoc vn env.bindings }

let types_unique ntdecls = 
  let rec no_duplicates = function
    | [] -> true
    | (x :: xs) -> not (List.mem x xs) && (no_duplicates xs)
  in
  no_duplicates (List.map (fun (DBN (n, _)) -> n) ntdecls)

(* Vérifie que le graphe n'a pas de déclarations de types dupliquées *)
let check_graph_types (DBG (ntdecls, rtdecls)) = 
  if types_unique ntdecls then 
    Result.Ok () 
  else 
    Result.Error " votre Déclaration de type est en double dans les types de nœuds"

(* Fonction auxiliaire pour vérifier le type d'un nœud dans l'environnement *)
let check_node_type vname label env =
  try
    let expected_label = List.assoc vname env.bindings in
    if expected_label = label then Ok env
    else Error ("Type mismatch for node " ^ vname ^ ": expected " ^ expected_label ^ ", found " ^ label)
  with Not_found -> Error ("Node " ^ vname ^ " not found in environment")

(* Fonction pour récupérer le type d'une expression *)
let rec tp_expr env = function
  | Const v -> IntT  (* Assumer que les constantes sont de type entier *)
  | AttribAcc (vn, fn) -> 
      (* Vérification d'attributs : ici on suppose que les attributs sont des entiers *)
      (try 
         let _ = List.assoc vn env.bindings in 
         IntT
       with Not_found -> raise (FieldAccError "Field access error"))
  | BinOp (bop, e1, e2) -> 
      let t1 = tp_expr env e1 in
      let t2 = tp_expr env e2 in
      if t1 = IntT && t2 = IntT then IntT
      else raise (TypeError "Binary operation expects two integer operands")

(* Vérification des types des expressions avec un type attendu *)
let check_expr e et env : tc_result = 
  try 
    if tp_expr env e = et then Result.Ok env
    else Result.Error ["Expression does not have expected type " ^ (show_attrib_tp et)]
  with 
  | TypeError s -> Result.Error [s]
  | FieldAccError s -> Result.Error [s]

(* Fonction de type-checking d'une instruction *)
let rec tc_instr (instr : instruction) (env : environment) : tc_result =
  match instr with
  | IActOnNode (_act, vn, label) -> 
      (* Vérifier que le nœud existe et que son type correspond à ce qui est attendu *)
      check_node_type vn label env

  | IActOnRel (_act, vn1, label, vn2) ->
      (* Vérifier que les deux nœuds existent et que leurs types sont valides *)
      let check1 = check_node_type vn1 label env in
      let check2 = check_node_type vn2 label env in
      if check1 = Ok env && check2 = Ok env then Ok env
      else 
        match check1 with
        | Error msg -> Error msg
        | Ok _ -> check2

  | IDeleteNode vn ->
      (* Vérifier que le nœud existe avant de le supprimer *)
      (try 
         let _ = List.assoc vn env.bindings in
         Ok env
       with Not_found -> Error ["Node " ^ vn ^ " not found for deletion"])

  | IDeleteRel (vn1, label, vn2) ->
      (* Vérifier que les nœuds de la relation existent avant de la supprimer *)
      let check1 = check_node_type vn1 label env in
      let check2 = check_node_type vn2 label env in
      if check1 = Ok env && check2 = Ok env then Ok env
      else 
        match check1 with
        | Error msg -> Error msg
        | Ok _ -> check2

  | IReturn vnames ->
      (* Vérifier que toutes les variables existent dans l'environnement *)
      let rec check_vars = function
        | [] -> Ok env
        | v::vs -> 
            (try 
               let _ = List.assoc v env.bindings in
               check_vars vs
             with Not_found -> Error ["Variable " ^ v ^ " not found in environment"])
      in
      check_vars vnames

  | IWhere expr ->
      (* Vérifier le type de l'expression dans la condition WHERE *)
      let expr_type = tp_expr env expr in
      if expr_type = IntT then Ok env
      else Result.Error ["Where condition must evaluate to an integer type"]

  | ISet (vn, field, expr) ->
      (* Vérifier que la variable existe et que l'expression est valide *)
      (try 
         let _ = List.assoc vn env.bindings in
         check_expr expr IntT env  (* Assumer que l'expression doit être de type entier *)
       with Not_found -> Error ["Variable " ^ vn ^ " not found for set"])

(* Fonction pour vérifier la liste des instructions avec arrêt sur erreur *)
let check_and_stop (res : tc_result) i : tc_result = Result.bind res (tc_instr i)

let tc_instrs_stop gt instrs : tc_result = 
  List.fold_left check_and_stop (initial_result gt) instrs

(* Fonction pour vérifier le programme avec gestion des erreurs *)
let typecheck_instructions continue gt instrs np = 
  let r = tc_instrs_stop gt instrs in
  match r with
  | Result.Error errs -> 
      Printf.printf "%s\n" (String.concat "\n" errs); 
      failwith "stopped"
  | Result.Ok _ -> np

(* Fonction principale pour vérifier le programme complet *)
let typecheck continue (NormProg (gt, NormQuery instrs) as np) = 
  match check_graph_types gt with
  | Result.Error egt -> 
      Printf.printf "%s\n" ("Undeclared types in\n" ^ egt);
      failwith "stopped"
  | _ -> typecheck_instructions continue gt instrs np
