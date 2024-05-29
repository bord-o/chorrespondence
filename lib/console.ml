(* The console *)
open Cohttp_eio
open Node

let ( let* ) x f = Result.bind x f
let parse_addr = Ipaddr.with_port_of_string ~default:8080

let handle_errors r =
  match r with
  | Ok _ -> ()
  | Error `PortParseError -> Eio.traceln "Error parsing port"
  | Error `IpAddrParseError -> Eio.traceln "Error parsing ip addr"
  | Error `NoSuccessor -> Eio.traceln "No successor node set"
  | Error (`Msg s) -> Eio.traceln "Error Msg: %s" s
  | Error `ErrorStatus -> Eio.traceln "Error Status"

exception ExitConsole

let get ~addr ~endpoint ~sw ~env =
  let* ip, port = parse_addr addr in
  let client = Client.make ~https:None env#net in
  let resp, body =
    Client.get ~sw client
      (Uri.of_string
      @@ Printf.sprintf "http://%s:%i/%s" (Ipaddr.to_string ip) port endpoint)
  in
  if Http.Status.compare resp.status `OK = 0 then
    let res = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
    Ok res
  else (
    Eio.traceln "Error Status:%s" @@ Http.Status.to_string resp.status;
    Error `ErrorStatus)

let post ~addr ~endpoint ~json ~sw ~env =
  let* ip, port = parse_addr addr in
  let client = Client.make ~https:None env#net in
  let node = Yojson.Safe.to_string json in
  let body_json = Cohttp_eio.Body.of_string node in
  let resp, body =
    Client.post ~body:body_json ~sw client
      (Uri.of_string
      @@ Printf.sprintf "http://%s:%i/%s" (Ipaddr.to_string ip) port endpoint)
  in
  if Http.Status.compare resp.status `OK = 0 then
    let res = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
    Ok res
  else (
    Eio.traceln "Error Status:%s" @@ Http.Status.to_string resp.status;
    Error `ErrorStatus)

(* TODO: extract lookup/store/get/set into functions to be shared with the join/find_succ algorithms *)
(* TODO: write the find_successor function *)
(* TODO: write the notify function *)
(* TODO: write the stabilize function *)
(* TODO: figure out what needs to be scheduled*)

let spawn sw env node =
  print_endline "entering console...\n";
  while true do
    Eio.Buf_write.with_flow ~initial_size:100 (Eio.Stdenv.stdout env)
    @@ fun f ->
    Eio.Buf_write.printf f "> ";

    let buf_stdin = Eio.Buf_read.of_flow ~max_size:100 (Eio.Stdenv.stdin env) in
    let input = Eio.Buf_read.line buf_stdin in
    let () = Eio.traceln "You said: %s" input in

    match String.split_on_char ' ' input with
    | [] -> ()
    | [ "leave" ] ->
        Eio.traceln "Exiting...";
        Eio.Switch.fail sw ExitConsole
        (* TODO: make this leave the network gracefully *)
    | "store" :: "@" :: id :: msg ->
        let msg = String.concat " " msg in
        Eio.traceln "Storing message %s with content: %s" id msg
    | "set_succ" :: who :: what :: _ ->
        Eio.traceln "setting succ of %s to %s" who what;
        let body =
          Json_types.node_to_yojson
            Json_types.
              {
                sha1_hex =
                  Digestif.SHA1.digest_string what |> Digestif.SHA1.to_hex;
                addr = what;
              }
        in
        post ~json:body ~addr:who ~endpoint:"succ" ~sw ~env |> handle_errors
    | "get_succ" :: who :: _ ->
        Eio.traceln "getting succ of %s" who;
        get ~addr:who ~endpoint:"succ" ~sw ~env |> handle_errors
    | "set_pred" :: who :: what :: _ ->
        Eio.traceln "setting pred of %s to %s" who what;
        let body =
          Json_types.node_to_yojson
            Json_types.
              {
                sha1_hex =
                  Digestif.SHA1.digest_string what |> Digestif.SHA1.to_hex;
                addr = what;
              }
        in
        post ~json:body ~addr:who ~endpoint:"pred" ~sw ~env |> handle_errors
    | "get_pred" :: who :: _ ->
        Eio.traceln "getting pred of %s" who;
        let res = get ~addr:who ~endpoint:"pred" ~sw ~env in
        Eio.traceln "pred is %s" (res |> Result.get_ok)
    | "store" :: _ -> Eio.traceln "Usage: "
    | [ "lookup"; id ] -> Eio.traceln "Looking up message %s" id
    | "debug" :: _ ->
        Eio.traceln "Current node details: \n%s" @@ Node.show !node
    | "help" :: _ ->
        Eio.traceln
          {|Usage:
    lookup [id] 
    store [@id] [message] 
    set_succ [id] [value]
    set_pred [id] [value]
    get_succ [id]
    get_pred [id]

    stabilize
    notify
    fingers

    set_my_succ [value]
    set_my_pred [value]

    debug 
    leave 
    help|}
    (* =======================MANUAL STATE MAINTANENCE COMMANDS======================================= *)
    | "lookup" :: _ -> Eio.traceln "Usage: lookup [id]"
    | "stabilize" :: _ -> Eio.traceln "Stabilizing..."
    | "notify" :: _ -> Eio.traceln "Notifying..."
    | "fingers" :: _ -> Eio.traceln "Fixing fingers table..."
    | "set_my_succ" :: addr :: _ ->
        Eio.traceln "Manually setting successor to %s" addr;
        let _ = parse_addr addr in
        node :=
          { !node with succ = Some (Digestif.SHA1.digest_string addr, addr) }
    | "set_my_pred" :: addr :: _ ->
        Eio.traceln "Manually setting predecessor to %s" addr;
        let _ = parse_addr addr in
        node :=
          { !node with pred = Some (Digestif.SHA1.digest_string addr, addr) }
    | s :: _ ->
        (* all other routes hit our successor for testing *)
        (let* succ =
           if Option.is_none !node.succ then Error `NoSuccessor
           else Ok (Option.get !node.succ)
         in
         get ~addr:(snd succ) ~endpoint:s ~sw ~env)
        |> handle_errors
  done;
  Ok ()
