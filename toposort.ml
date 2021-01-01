(* Topological sort *)

(* exeption if the graph given is not a DAG *)
exception Cykliczne;;

(* convertion of 'a graph to int graph *)
(* creating a maps with int and 'a as keys for vertecies of graph given as graph_list
    and counting the number of vertecies in the given graph *)
let rec convert_to_map graph_list graph_map graph_map2 n =
  match graph_list with
  | [] -> (graph_map, graph_map2, n)
  | (v, edges) :: tail ->
    let rec pom l g_map g_map2 pomn =
      match l with
      | [] -> (g_map, g_map2, pomn)
      | h :: t ->
        if not (PMap.exists h g_map2) then
          pom t (PMap.add (pomn + 1) h g_map) (PMap.add h (pomn + 1) g_map2) (pomn + 1)
        else
          pom t g_map g_map2 pomn
    in
    if not (PMap.exists v graph_map2) then
      let (g_m, g_m2, pom_n) =
        pom edges (PMap.add (n + 1) v graph_map) (PMap.add v (n + 1) graph_map2) (n + 1) in
      convert_to_map tail g_m g_m2 pom_n
    else
      let (g_m, g_m2, pom_n) =
        pom edges graph_map graph_map2 n in
      convert_to_map tail g_m g_m2 pom_n
;;

let rec convert_to_int_list lst graph_map acc =
  match lst with
  | [] -> acc
  | h :: t ->
    convert_to_int_list t graph_map ((PMap.find h graph_map) :: acc)
;;

(* converting 'a graph given as a list to int graph
   represented as an array of arrays *)
let convert graph_list graph_map graph n =
  let graph_lst = ref graph_list in
  for i = 0 to n - 1 do
    let (v, edges) = List.hd !graph_lst in
    let int_edges = convert_to_int_list edges graph_map [] in
    let v = PMap.find v graph_map in
    graph.(v) <- int_edges;
    graph_lst := List.tl !graph_lst;
  done;
  ();;

(* convert int vertecies back to original 'a values *)
let rec decode int_list a_list graph_map =
  match int_list with
  | [] -> a_list
  | h :: t -> decode t ((PMap.find h graph_map) :: a_list) graph_map
;;


(* number of vertecies in a graph given as a list
   eg.: [(1, [2; 3]); (2, [4]); (3, [4; 5])]*)
(* let rec num_of_vertecies lst n =
  match lst with
  | [] -> n
  | (v, edges) :: tail ->List.rev (
    let rec pom l maxi =
      match l with
      | [] -> maxi
      | h :: t -> pom t (max maxi h) in
    num_of_vertecies tail (max n (pom edges v));; *)

(* converting given list representing a graph
   to an array of arrays graph representation *)
(* let create_graph n graph_list graph =
  let g = Array.of_list graph_list in
  for i = 0 to n - 1 do
    let j = fst g.(i)
    and el = snd g.(i) in
    graph.(j) <- el;
  done;
  ();; *)

let reset_array tab el =
  for i = 0 to (Array.length tab) - 1 do
    tab.(i) <- el;
  done;
  ();;

let topol (graph_list : ('a * 'a list) list) =
  let (map_decode, map_code, n) = convert_to_map graph_list PMap.empty PMap.empty 0 in
  (* let n = num_of_vertecies graph_list 0 in *)
  let len = List.length graph_list in
  let q = Queue.create () in
  let vertecies_to_v = Array.make (n + 1) 0 in
  let answer = ref [] in
  let graph = Array.make (n + 1) [] in

  reset_array vertecies_to_v 0;
  reset_array graph [];

  convert graph_list map_code graph len;

  (* create_graph len graph_list graph; *)

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
  (* List.rev !answer;; *)
  decode !answer [] map_decode;;
  (* graph;; *)

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
