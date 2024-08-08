open !StdLabels
open !StringLabels
open !MoreLabels
open Protocol_conv_json
module StringMap = Map.Make(String)

type state = On | Off [@@deriving protocol ~driver:(module Json)]

(* TODO: Add timestamp for last update *)
(* We also want some cleanup of old states *)
type status = { id: string; update: int; in_state:int; current: state; expected: state } [@@deriving protocol ~driver:(module Json)]

let table_style =
  {|
    <style>
    table {
      font-family: arial, sans-serif;
      border-collapse: collapse;
      width: 100%;
    }

    td, th {
      border: 1px solid #070707;
      text-align: left;
      padding: 8px;
    }

    tr:nth-child(even) {
      background-color: #dddddd;
    }
    </style>
  |}

let meta_equiv tpe =
  let content =
    match tpe with
    | `Refresh seconds -> string_of_int seconds
    | `Redirect url -> "0; url=" ^ url
  in
   Printf.sprintf {|<meta http-equiv="refresh" content="%s">|} content

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
  | _ ->
    let now = Unix.gettimeofday () |> int_of_float in
    { id; update=now; in_state=now; current=Off; expected = Off }

let save_state dir status =
  let statefile = dir ^ "/" ^ (sanitize status.id) ^ ".json" in
  let json = status_to_json status in
  Yojson.Safe.to_file statefile json

let all_status ~dir =
  let rec inner handle =
    match Unix.readdir handle with
    | file when Sys.is_regular_file (dir ^ "/" ^ file) ->
      let status = Yojson.Safe.from_file (dir ^ "/" ^ file) |> status_of_json_exn in
      status :: inner handle
    | _ -> inner handle
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


let return_html ?(headers=[]) body =
  print_string "Content-Type:text/html\n\n";
  print_endline "<head>";
  List.iter ~f:print_endline headers;
  print_endline "</head>";

  print_endline "<html>";
  print_endline "<body>";
  print_endline body;
  print_endline "</body>";
  print_endline "</html>";
  ()

let return_text text =
  print_string "Content-Type:text/plain\n\n";
  print_endline text

let do_get () =
  let id = get_arg "id" |> Option.get in
  let status = get_state ~id state_dir in

  let state = match get_arg "state" with
    | Some state -> state_of_string state
    | None -> failwith "State unset"
  in
  let now = Unix.gettimeofday () |> int_of_float in
  let in_state = match status.current = state with
    | true -> status.in_state
    | false -> now
  in
  let new_status = { status with update = now; current = state; in_state } in
  save_state state_dir new_status;
  (* Return 0 or 1 *)
  return_text (string_of_state status.expected)

let do_status () =
  let since update =
    let now = Unix.gettimeofday () |> int_of_float in
    let since = (now - update) in
    let sec = since mod 60 in
    let min = since / 60 mod 60 in
    let hour = since / 60 / 60 in
    Printf.sprintf "%02d:%02d:%02d" hour min sec
  in

  let icon s = Printf.sprintf {| <img src="icons/%s.png" style="width:42px;height:42px;"> |} s in
  let on_switch = icon "switch-on" in
  let off_switch = icon "switch-off" in
  let on_icon = icon "power-on" in
  let off_icon = icon "power-off" in

  let print_row buffer { id; update; in_state; current; expected } =
    Buffer.add_string buffer "<tr>\n";
    Buffer.add_string buffer ("<td>" ^ id ^ "</td>\n");
    Buffer.add_string buffer ("<td>" ^ (since update) ^ "</td>\n");

    let state = match current with
      | On -> on_icon
      | Off -> off_icon
    in

    Buffer.add_string buffer ("<td>" ^ state ^ "</td>\n");
    Buffer.add_string buffer ("<td>" ^ (since in_state) ^ "</td>\n");

    let toggle_image = match expected with
      | On -> on_switch
      | Off -> off_switch
    in
    let next_state = match expected with
      | On -> 0
      | Off -> 1
    in
    let button = Printf.sprintf "<a href=\"set?id=%s&state=%d\">%s</a>" id next_state toggle_image in
    Buffer.add_string buffer ("<td>" ^ button ^ "</td>\n");

    Buffer.add_string buffer "</tr>\n";
  in

  let devices = all_status ~dir:state_dir in

  (* Print html and create links to call on and off *)
  let buffer = Buffer.create 256 in
  Buffer.add_string buffer "<table>\n";
  Buffer.add_string buffer "<tr>\n";
  Buffer.add_string buffer "<th>Device</th>\n";
  Buffer.add_string buffer "<th>Last seen</th>\n";
  Buffer.add_string buffer "<th>State</th>\n";
  Buffer.add_string buffer "<th>In state</th>\n";
  Buffer.add_string buffer "<th>Switch</th>\n";
  Buffer.add_string buffer "</tr>\n";
  List.iter ~f:(print_row buffer) devices;
  Buffer.add_string buffer "</table>\n";
  Buffer.add_string buffer "<table>\n";
  Buffer.add_string buffer "<tr>\n";
  Buffer.add_string buffer "<td>All on</td>\n";
  Buffer.add_string buffer ("<td><a href=\"set?id=all&state=1\">" ^ on_icon ^ "</a></td>\n");
  Buffer.add_string buffer "</tr>\n";
  Buffer.add_string buffer "<tr>\n";
  Buffer.add_string buffer "<td>All off</td>\n";
  Buffer.add_string buffer ("<td><a href=\"set?id=all&state=0\">" ^ off_icon ^ "</a></td>\n");
  Buffer.add_string buffer "</tr>\n";


  Buffer.add_string buffer "</table>\n";

  return_html
    ~headers:[table_style; meta_equiv (`Refresh 2);]
    (Buffer.contents buffer)

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

  return_html ~headers:[meta_equiv (`Redirect "status")] "Success"

let do_error path = return_html ("Error called. Unknown path: " ^ path)

let main () =
  match Sys.getenv "DOCUMENT_URI" with
  | "/get" -> do_get ()
  | "/status" -> do_status ()
  | "/set" -> do_set ()
  | path -> do_error path

let () =
  try
    main ()
  with
  | exn -> return_html ("Error: " ^ (Printexc.to_string exn))
