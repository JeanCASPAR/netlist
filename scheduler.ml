open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp (eq : equation) : ident list =
  let (id, e) = eq in
  let find_id_in_arg arg acc = match arg with
  | Avar id -> id :: acc
  | Aconst _ -> acc
  in
  let find_idents e = match e with
  | Earg arg -> find_id_in_arg arg []
  | Ereg _ -> []
  | Enot arg -> find_id_in_arg arg []
  | Ebinop (_, arg1, arg2) ->
    [] |> find_id_in_arg arg1 |> find_id_in_arg arg2
  | Emux (arg1, arg2, arg3) ->
    [] |> find_id_in_arg arg1 |> find_id_in_arg arg2 |> find_id_in_arg arg3
  | Erom (_, _, arg) -> find_id_in_arg arg []
  | Eram (_, _, arg, _, _, _) ->
    [] |> find_id_in_arg arg
  | Econcat (arg1, arg2) ->
    [] |> find_id_in_arg arg1 |> find_id_in_arg arg2
  | Eslice (_, _, arg) -> find_id_in_arg arg []
  | Eselect (_, arg) -> find_id_in_arg arg []
  in
  List.sort_uniq String.compare (find_idents e)

let schedule p =
  let g = mk_graph () in
  Env.iter (fun key _ -> Graph.add_node g key) p.p_vars;
  List.iter (fun eq ->
    let output = fst eq in
    List.iter (fun input -> Graph.add_edge g input output) (read_exp eq)
  ) p.p_eqs;

  try
    let topo = Graph.topological g in
    let l = List.mapi (fun i x -> (x, i)) topo in
    let topo_compare (id1, _) (id2, _) = compare (List.assoc id1 l) (List.assoc id2 l) in
    { p with p_eqs = List.sort topo_compare p.p_eqs; }
  with
    Graph.Cycle -> raise Combinational_cycle
