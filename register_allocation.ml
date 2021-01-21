type vertex = int
module SV = Set.Make(Int)

type edge = {src : vertex; dst : vertex}
type graph = {vertexes: SV.t; edges: edge list}

let ind_list : 'a list -> 'b -> ('a -> 'b -> 'b)-> 'b =
  fun xs y f -> List.fold_right f xs y

let iter_list : 'a list -> ('a -> unit) -> unit =
  fun xs f -> ind_list xs () (fun x () -> f x)

let vertex_of_max_weight : (vertex, int) Hashtbl.t -> vertex = fun wt ->
  let (v,w) = Seq.fold_left
                (fun (v1, w1) (v2, w2) ->
                  if w2 > w1 then (v2, w2) else (v1, w1))
                (-1,-1)
                (Hashtbl.to_seq wt)
  in v

let neighborhood : graph -> vertex -> SV.t = fun g v ->
  let rec helper : edge list -> SV.t = function
    | [] ->
      SV.empty
    | {src; dst} :: es ->
      let vs = helper es in
      if src = v then
        SV.add dst vs
      else if dst = v then
        SV.add src vs
      else
        vs
  in
  let {vertexes; edges} = g in
  helper edges

let max_cardinality_search : graph -> vertex list = fun g ->
  let {vertexes; edges} = g in
  let n = SV.cardinal vertexes in
  let weights : (vertex, int) Hashtbl.t = Hashtbl.create(n) in
  let to_visit : SV.t ref = ref vertexes in
  SV.iter (fun v -> Hashtbl.add weights v 0) vertexes;
  let vs : vertex list ref = ref [] in
  for i = 0 to n - 1 do
    let v = vertex_of_max_weight weights in
    vs := v :: ! vs;
    SV.iter (fun u -> ()) (SV.inter (! to_visit) (neighborhood g v));
    to_visit := SV.remove v (! to_visit)
  done;
  List.rev (! vs)

type color = int
module SC = Set.Make(Int)

let greedy_coloring : graph -> vertex list -> (vertex, color) Hashtbl.t =
  fun g vs ->
    let n = List.length vs in
    let col = Hashtbl.create n in
    let lowest_avaiable_color g vs = 0 in
    iter_list vs
      (fun v ->
        let c : color = lowest_avaiable_color g (neighborhood g v) in
        Hashtbl.add col v c);
    col
