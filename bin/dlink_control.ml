open !StdLabels
open !StringLabels
open !MoreLabels
open Protocol_conv_json

module StringMap = Map.Make(String)

type state = On | Off [@@deriving protocol ~driver:(module Json)]

(* TODO: Add timestamp for last update *)
(* We also want some cleanup of old states *)
type status = { id: string; current: state; expected: state } [@@deriving protocol ~driver:(module Json)]

let state_dir = Sys.getenv "STATE_DIR"
let string_of_state = function
  | On -> "1"
  | Off -> "0"

let state_of_string = function
  | "0" -> Off
  | "1" -> On
  | state -> failwith ("Unknown state: " ^ state)

let sanitize =
  String.map ~f:(function
    | ('a'..'z'
      | 'A'..'Z'
      | '0'..'9'
      | '-' ) as c -> c
    | _ -> '_')

let get_state ~id dir =
  let statefile = dir ^ "/" ^ (sanitize id) ^ ".json" in
  try
    let json = Yojson.Safe.from_file statefile in
    status_of_json_exn json
  with
  | _ -> { id; current=Off; expected = Off }

let save_state dir status =
  let statefile = dir ^ "/" ^ (sanitize status.id) ^ ".json" in
  let json = status_to_json status in
  Yojson.Safe.to_file statefile json

let all_status ~dir =
  let rec inner handle =
    match Unix.readdir handle with
    | file ->
      let status = Yojson.Safe.from_file file |> status_of_json_exn in
      status :: inner handle
    | exception _ ->
      []
  in
  Unix.opendir dir |> inner


let get_arg =
  let args =
    Sys.getenv_opt "QUERY_STRING"
    |> Option.value ~default:""
    |> String.split_on_char ~sep:'&'
    |> List.fold_left ~init:StringMap.empty ~f:(fun acc arg ->
      match String.split_on_char ~sep:'=' arg with
      | key :: value ->
        StringMap.add ~key ~data:(String.concat ~sep:"=" value) acc
      | [] -> acc)
  in
  fun key -> StringMap.find_opt key args


let return_html body =
  print_endline "Content-Type:text/html\n\n";
  print_endline "<html>";
  print_endline "<body>";
  print_endline body;
  print_endline "</body>";
  print_endline "</html>";
  ()

let return_text text =
  print_endline "Content-Type:text/plain\n\n";
  print_endline text

let do_get () =
  let id = get_arg "id" |> Option.get in
  let status = get_state ~id state_dir in

  let state = match get_arg "state" with
    | Some state -> state_of_string state
    | None -> failwith "State unset"
  in
  let new_status = { status with current = state } in
  save_state state_dir new_status;
  (* Return 0 or 1 *)
  return_text (string_of_state status.expected)

let do_status () =
  (* Print html and create links to call on and off *)
  return_html "Status called"

let do_set () =
  let expected = get_arg "state" |> Option.get |> state_of_string in
  let id = get_arg "id" |> Option.get in
  let status = match id with
    | "all" -> all_status ~dir:state_dir
    | id -> [ get_state ~id state_dir ]
  in
  status
  |> List.map ~f:(fun status -> { status with expected = expected })
  |> List.iter ~f:(save_state state_dir);

  return_html "Set called"

let do_error path = return_html ("Error called. Unknown path: " ^ path)

let main () =
  match Sys.getenv "DOCUMENT_URI" with
  | "/dlink/get" -> do_get ()
  | "/dlink/status" -> do_status ()
  | "/dlink/set" -> do_set ()
  | path -> do_error path

let () =
  try
    main ()
  with
  | exn -> return_html ("Error: " ^ (Printexc.to_string exn))
