(* Topological sort *)

(* exeption if the graph given is not a DAG *)
exception Cykliczne;;

(* converting given list representing a graph
   to an array of arrays graph representation *)
let create_graph n graph_list graph =
  let g = Array.of_list graph_list in
  for i = 0 to n - 1 do
    let j = fst g.(i)
    and el = snd g.(i) in
    graph.(j) <- el;
  done;
  ();;

let reset_array tab el =
  for i = 0 to (Array.length tab) - 1 do
    tab.(i) <- el;
  done;
  ();;

(* number of vertecies in a graph given as a list 
   eg.: [(1, [2; 3]); (2, [4]); (3, [4; 5])]*)
let rec num_of_vertecies lst n =
  match lst with
  | [] -> n
  | (v, edges) :: tail ->
    let rec pom l maxi =
      match l with
      | [] -> maxi
      | h :: t -> pom t (max maxi h) in
    num_of_vertecies tail (max n (pom edges 0))
;;

let topol graph_list =
  let n = num_of_vertecies graph_list 0 in
  let len = List.length graph_list in
  let q = Queue.create () in
  let vertecies_to_v = Array.make (n + 1) 0 in
  let answer = ref [] in
  let graph = Array.make (n + 1) [] in

  reset_array vertecies_to_v 0;
  reset_array graph [];

  create_graph len graph_list graph;

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
(* let test = [(1, [2; 3]); (2, [4]); (3, [4]); (4, [5]); (5, [3])];;
let test2 = [(1, [2; 3]); (2, [4]); (3, [4; 5]); (4, []); (5, [])];;
let test3 = [(1, [2; 3]); (2, [4]); (3, [4; 5])];;


let t = try topol test with | Cykliczne -> [];;
let t2 = topol test2;;
let t3 = topol test3;;

assert (t = []);;
assert (t2 = [1; 2; 3; 4; 5]);;
assert (t3 = t2);; *)
