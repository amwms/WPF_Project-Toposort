(* Topological sort *)

(* exeption if the graph given is not a DAG *)
exception Cykliczne;;

(* converting given list representing a graph
   to an array of arrays graph representation *)
let create_graph n g graph =
  for i = 0 to n - 1 do
    let j = fst g.(i)
    and el = snd g.(i) in
    graph.(j) <- el;
  done;
  ();;

let reset_array tab =
  for i = 0 to (Array.length tab) - 1 do
    tab.(i) <- 0;
  done;
  ();;

let topol graph_list =
  let n = List.length graph_list in
  let q = Queue.create () in
  let vertecies_to_v = Array.make (n + 1) 0 in
  let answer = ref [] in
  let g = Array.of_list graph_list in
  let graph = Array.make (n + 1) [] in

  create_graph n g graph;

  reset_array vertecies_to_v;

  (* setting the number of edges going into each vertex of the graph *)
  for i = 1 to n do
    List.iter (fun v ->
        vertecies_to_v.(v) <- vertecies_to_v.(v) + 1;
      ) graph.(i);
  done;

  (* pushing vertecies with zero edges going into them onto queue *)
  for i = 1 to n do
    if vertecies_to_v.(i) = 0 then
      Queue.push i q;
  done;

  while not (Queue.is_empty q) do
    let w = Queue.take q in
    answer := w :: !answer;
    List.iter (fun v ->
        vertecies_to_v.(v) <- vertecies_to_v.(v) - 1;
        if vertecies_to_v.(v) = 0 then
          Queue.push v q;
      ) graph.(w);
  done;

  (* if the given graph is not a DAG then raise the exeption *)
  for i = 1 to n do
    if vertecies_to_v.(i) > 0 then
      raise Cykliczne
    else
      ()
  done;
  List.rev !answer;;

(* test *)
let test = [(1, [2; 3]); (2, [4]); (3, [4]); (4, [5]); (5, [3])];;
let test2 = [(1, [2; 3]); (2, [4]); (3, [4; 5]); (4, []); (5, [])];;

let t = try topol test with |Cykliczne -> [];;
let t2 = topol test2;;

assert (t = []);;
assert (t2 = [1; 2; 3; 4; 5]);;
