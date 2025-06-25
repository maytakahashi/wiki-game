open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and return a list
   of strings containing that actor's main credits. *)
let get_credits contents : string list =
  let open Soup in
  parse contents
  $$ ".ipc-primary-image-list-card__title"
  |> to_list
  |> List.map ~f:(fun li -> texts li |> String.concat ~sep:"" |> String.strip)
;;

let%expect_test "get_credits" =
  let contents =
    File_fetcher.fetch_exn Remote ~resource:"https://www.imdb.com/name/nm0000706/?ref_=fn_al_nm_1"
  in
  List.iter (get_credits contents) ~f:print_endline;
  [%expect
    {|
    Everything Everywhere All at Once    
    Crouching Tiger, Hidden Dragon
    Wicked
    Kung Fu Panda 2
    |}]
;; 

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"given an IMDB page for an actor, print out a list of their main credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group ~summary:"imdb commands" [ "print-credits", print_credits_command ]
;;
