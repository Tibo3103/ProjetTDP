open Graphstruct
open Lang
open Instr
 
type environment = { types:  db_tp; bindings: (vname * label) list }

let initial_environment gt = {types = gt; bindings = []}
let initial_result gt = Result.Ok (initial_environment gt)
  
exception FieldAccError of string
exception TypeError of string


type tc_result = (environment, string list) result

(* Functions for manipulating the environment *)

let add_var vn t (env:environment) = 
  {env with bindings = (vn,t)::env.bindings}

let remove_var vn env = 
  {env with bindings = List.remove_assoc vn env.bindings}

(* TODO: add more auxiliary functions here *)

(* TODO: fill in details *)
let check_graph_types (DBG (node_types, relation_types)) : (db_tp, string list) result =
  (* Vérifie qu'il n'y a pas de doublon dans les labels de nœuds *)
  let rec check_duplicates nodes seen errors =
    match nodes with
    | [] -> errors
    | DBN (label, _) :: rest ->
        if List.mem label seen then
          check_duplicates rest seen (errors @ [Printf.sprintf "Duplicate node type declaration: %s" label])
        else
          check_duplicates rest (label :: seen) errors
  in
  let errors1 = check_duplicates node_types [] [] in

  (* Vérifie pour chaque relation que le type source et le type cible existent *)
  let errors2 =
    List.fold_left (fun errs (DBR (src, rel, tgt)) ->
      let src_exists = List.exists (fun (DBN (label, _)) -> label = src) node_types in
      let tgt_exists = List.exists (fun (DBN (label, _)) -> label = tgt) node_types in
      let errs =
        if not src_exists then errs @ [Printf.sprintf "Relation '%s': source type '%s' not declared" rel src] else errs
      in
      if not tgt_exists then
        errs @ [Printf.sprintf "Relation '%s': target type '%s' not declared" rel tgt]
      else errs
    ) [] relation_types
  in

  let all_errors = errors1 @ errors2 in
  if all_errors = [] then Result.Ok (DBG (node_types, relation_types))
  else Result.Error all_errors




(* TODO: fill in details *)
let rec tp_expr env = function
  Const v -> IntT
| AttribAcc (vn, fn) -> IntT
| BinOp (bop, e1, e2) -> tp_expr env e1

(* check expression e with expected type et in environment env *)
let check_expr e et env : tc_result = 
  try 
    if tp_expr env e = et
    then Result.Ok env
    else Result.Error ["Expression does not have expected type " ^ (show_attrib_tp et)]
  with 
  | TypeError s -> Result.Error [s]
  | FieldAccError s -> Result.Error [s]
  

let tc_instr (i: instruction) (env: environment) : tc_result = 
  match i with
  | IActOnNode (_act, vn, lb) -> Result.Error ["not yet implemented"]
  | _  -> Result.Error ["also not implemented"]

(* type check list of instructions and stop on error *)
let check_and_stop (res : tc_result) i : tc_result = Result.bind res (tc_instr i)

let tc_instrs_stop gt instrs : tc_result = 
  List.fold_left check_and_stop (initial_result gt) instrs


  (* TODO: typecheck instructions *)
let typecheck_instructions continue gt instrs np = 
  let r = Result.Ok initial_environment in   (* call to real typechecker here *)
  match r with
  | Result.Error etc -> Printf.printf "%s\n" (String.concat "\n" etc); 
                        failwith "stopped"
  |_ -> np
  

  
  (* Typecheck program; 
     If continue is true, continue typechecking even 
     when errors have been discovered (can be ignored in a first version) *)  
let typecheck continue (NormProg(gt, NormQuery instrs) as np) = 
  match check_graph_types gt with
    | Result.Error egt -> Printf.printf "%s\n" ("Undeclared types in\n" ^ egt);
                            failwith "stopped"
    | _ -> typecheck_instructions continue gt instrs np
      