open! Core
module City = String
module Highway = struct
  include String
  let default = ""
end

module Network = struct
  module Connection = struct
    module T = struct
      type t = City.t * City.t * Highway.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s. This is needed
       to defined our [Network.t] type later. Using this [Comparable.Make] functor also
       gives us immutable maps, which might come in handy later. *)
    include Comparable.Make (T)

    let find_combos list edge =
      let repaired = List.map list ~f:(fun x -> String.map x ~f:(function
      | '.'
      | ' ' -> '_'
      | c -> c)) in
      let combos = List.cartesian_product repaired repaired in
      let filtered = List.filter combos ~f:(fun (a,b) -> not (String.equal a b)) in
      List.map filtered ~f:(fun (a,b) -> (City.of_string a, City.of_string b, Highway.of_string edge))
    ;;
    let of_string s = 
      match String.split s ~on:',' with
      | [] -> None
      | highway :: cities -> Some (find_combos cities highway)
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:(fun s ->
        match Connection.of_string s with
        | Some list -> list
        | _ ->
          printf "ERROR: Could not parse line as connection; dropping. %s\n" s;
          [])
    in
    Connection.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.of_file input_file in
        printf !"%{sexp: Network.t}\n" network]
;;

module G = Graph.Imperative.Graph.ConcreteLabeled (City) (Highway)
module Dot = Graph.Graphviz.Dot (struct
    include G
    let edge_attributes (_, label, _) = [`Label label; `Dir `None]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file listing all interstates and the cities they go through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (city1, city2, highway) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the graph if
             they don't already exist. *)
          G.add_edge_e graph (city1, highway, city2));
        Dot.output_graph (Out_channel.create (File_path.to_string output_file)) graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
