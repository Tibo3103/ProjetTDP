
(* Graph structures and bindings *)

type ('n, 'i) db_node = DBN of 'n * 'i
  [@@deriving show]
type ('n, 'r) db_rel = DBR of 'n * 'r * 'n
  [@@deriving show]
type ('n, 'i, 'r) db_graph = DBG of (('n, 'i) db_node list) * (('n, 'r) db_rel list)
  [@@deriving show]

let id_of_node (DBN(n, _nlb)) = n
let info_of_node (DBN(_n, nlb)) = nlb

let source_of_rel (DBR(s, _rlb, _t)) = s
let label_of_rel (DBR(_s, rlb, _t)) = rlb
let target_of_rel (DBR(_s, _rlb, t)) = t

let nodes_of_graph (DBG(ns,_rs)) = ns
let rels_of_graph (DBG(_ns,rs)) = rs

type nodeid = int
  [@@deriving show]
 

let empty_graph = DBG ([], [])

let add_nodes_to_graph new_nds (DBG (nds, rls)) = DBG (new_nds @ nds, rls)

(* An implicit assumption of the following is that the nodes adjacent to the relation exist in the graph *)
let add_rel_to_graph (DBG (nds, rls)) r = 
  let new_rls = if List.mem r rls then rls else (r::rls) in 
  DBG(nds, new_rls)





(* Représentation d'un nœud *)
type node = { id : int; (* l'identifiant du nœud *) }



(* Représentation d'un graphe comme une liste de nœuds et de ses arêtes *)
type graph = {
  nodes : node list;
  edges : edge list;
}



let find_node_with_id nid g = 
  List.find (fun (DBN(n, _)) -> n = nid) (nodes_of_graph g)


(* Fonction auxiliaire pour chercher un nœud par son identifiant *)
let find_node_by_id (g : graph) (id : int) : node option =
  try Some (List.find (fun node -> node.id = id) g)
  with Not_found -> None

(* Fonction pour ajouter un nœud à un graphe *)
let add_node (g : graph) (n : node) : graph =
  n :: g


(* Représentation d'une arête comme une paire de nœuds *)
type edge = { source : node; destination : node }


(* Fonction pour ajouter une arête entre deux nœuds *)
let add_edge (g : graph) (source : node) (destination : node) : graph =
  let edge = { source; destination } in
  { g with edges = edge :: g.edges }



(* Fonction pour vérifier si un nœud existe dans un graphe *)
let node_exists (g : graph) (id : int) : bool =
  List.exists (fun node -> node.id = id) g


(* Fonction pour supprimer un nœud d'un graphe *)
let remove_node (g : graph) (id : int) : graph =
  let nodes = List.filter (fun node -> node.id <> id) g.nodes in
  let edges = List.filter (fun e -> e.source.id <> id && e.destination.id <> id) g.edges in
  { nodes; edges }


(* Fonction pour parcourir tous les nœuds d'un graphe et afficher leur identifiant *)
let print_graph (g : graph) : unit =
  List.iter (fun node -> Printf.printf "Node ID: %d\n" node.id) g.nodes
 

(* fonction pour compter le nombre de noeuds*)
let count_nodes (g : graph) : int =
  List.length g.nodes



(*foncrion pour  verfier si le graphe est vide *)
let is_empty (g : graph) : bool =
  List.length g.nodes = 0
    