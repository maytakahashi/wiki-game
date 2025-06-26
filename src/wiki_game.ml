open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href]"
  |> to_list
  |> List.map ~f:(R.attribute "href")
  |> List.filter ~f:(fun x ->
    match Wikipedia_namespace.namespace x with None -> true | _ -> false)
  |> List.filter ~f:(fun x -> String.is_prefix ~prefix:"/wiki/" x)
  |> List.remove_consecutive_duplicates ~equal:String.equal
;;

let%expect_test "get_linked_articles" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Cat"
  in
  List.iter (get_linked_articles contents) ~f:print_endline;
  [%expect
    {|
    /wiki/Domestication_of_the_cat
    /wiki/Species
    /wiki/Carnivore
    /wiki/Mammal
    |}]
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

module Wiki = String

module Network = struct
  module Connection = struct
    module T = struct
      type t = Wiki.t * Wiki.t [@@deriving compare, sexp]
    end

    include Comparable.Make (T)
  end

  type t = Connection.Set.t [@@deriving sexp_of]
end

module G = Graph.Imperative.Graph.Concrete (Wiki)

let format str =
  match
    if String.is_prefix str ~prefix:"/wiki/"
    then String.chop_prefix str ~prefix:"/wiki/"
    else String.chop_prefix str ~prefix:"wiki/"
  with
  | None -> str ^ " - Wikipedia"
  | Some suffix ->
    String.map suffix ~f:(function '_' -> ' ' | c -> c) ^ " - Wikipedia"
;;

module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `Forward ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None

    let vertex_attributes v =
      [ `Shape `Box; `Label (format v); `Fillcolor 1000 ]
    ;;

    let vertex_name v = Printf.sprintf "\"%s\"" (format v)
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let visited = Wiki.Hash_set.create () in
  let graph = G.create () in
  let rec bfs node visited n =
    if n > 0 && not (Hash_set.mem visited node)
    then (
      let contents = File_fetcher.fetch_exn how_to_fetch ~resource:node in
      let adj = get_linked_articles contents in
      List.iter adj ~f:(fun adj ->
        G.add_edge graph adj node;
        if not (Hash_set.mem visited adj) then (
          Hash_set.add visited node;
          bfs adj visited (n - 1))))
  in
  G.add_vertex graph origin;
  bfs origin visited max_depth;
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
