(* The console *)
open Node
open Net

let ( let* ) x f = Result.bind x f
let parse_addr = Ipaddr.with_port_of_string ~default:8080

let usage () =
  {|Usage:
    lookup [id] 
    store [id] [message] 
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

let handle_errors r =
  match r with
  | Ok _ -> ()
  | Error `PortParseError -> Eio.traceln "Error parsing port"
  | Error `IpAddrParseError -> Eio.traceln "Error parsing ip addr"
  | Error `NoSuccessor -> Eio.traceln "No successor node set"
  | Error (`Msg s) -> Eio.traceln "Error Msg: %s" s
  | Error `ErrorStatus -> Eio.traceln "Error Status"

exception ExitConsole

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
    | [ "leave" ] ->
        Eio.traceln "Exiting...";
        Eio.Switch.fail sw ExitConsole
        (* TODO: make this leave the network gracefully *)
    | [ "lookup"; id ] ->
        (Eio.traceln "calling lookup at %s" id;
         let sha1_id = Digestif.SHA1.digest_string id in
         let* id_succ = find_successor !node (sha1_id, id) ~sw ~env in
         Eio.traceln "find succ successful: %s" (snd id_succ);
         let body =
           Json_types.lookup_to_yojson
             Json_types.{ sha1_hex = sha1_id |> Digestif.SHA1.to_hex }
         in
         Eio.traceln "calling lookup with %s" (snd id_succ);
         let* resp =
           post ~json:body ~addr:(snd id_succ) ~endpoint:"lookup" ~sw ~env
         in
         let* json =
           Yojson.Safe.from_string resp
           |> Json_types.found_of_yojson
           |> Result.map_error (fun _ -> `Msg "failed to get json")
         in
         Eio.traceln "value is: %s" json.payload;
         Ok ())
        |> handle_errors
    | "store" :: id :: msg ->
        (* to store a value we need to hash the id, find the successor for it, request the found successor to store the value, then report success *)
        (Eio.traceln "calling store at %s" id;
         let sha1_id = Digestif.SHA1.digest_string id in
         let* id_succ = find_successor !node (sha1_id, id) ~sw ~env in
         Eio.traceln "find succ successful: %s" (snd id_succ);
         let listing =
           List.fold_left (fun acc s -> acc ^ s ^ " ") "" msg |> String.trim
         in
         let nonce_str, hash = Pow.proof_of_work listing 0 in
         Eio.traceln "Found proof of work \nnonce: %s\nhash: %s" nonce_str
           (Digestif.BLAKE2B.to_hex hash);
         let body =
           Json_types.store_req_to_yojson
             Json_types.
               {
                 nonce = nonce_str;
                 payload =
                   {
                     sha1_hex = sha1_id |> Digestif.SHA1.to_hex;
                     payload = listing;
                   };
               }
         in
         Eio.traceln "calling store with %s" (snd id_succ);
         let _ =
           post ~json:body ~addr:(snd id_succ) ~endpoint:"store" ~sw ~env
         in
         Ok ())
        |> handle_errors
    | [ "find_succ"; id ] -> (
        match find_successor !node (addr_pair id) ~sw ~env with
        | Ok succ -> Eio.traceln "successor is: %s" (snd succ)
        | Error _ -> Eio.traceln "error finding succ")
    | [ "set_succ"; who; what ] ->
        Eio.traceln "setting succ of %s to %s" who what;
        let body =
          Json_types.node_to_yojson
            Json_types.{ sha1_hex = addr_hex what; addr = what }
        in
        post ~json:body ~addr:who ~endpoint:"succ" ~sw ~env |> handle_errors
    | [ "get_succ"; who ] ->
        Eio.traceln "getting succ of %s" who;
        let res = get ~addr:who ~endpoint:"succ" ~sw ~env in
        Eio.traceln "succ is %s" (res |> Result.get_ok)
    | [ "set_pred"; who; what ] ->
        Eio.traceln "setting pred of %s to %s" who what;
        let body =
          Json_types.node_to_yojson
            Json_types.{ sha1_hex = addr_hex what; addr = what }
        in
        post ~json:body ~addr:who ~endpoint:"pred" ~sw ~env |> handle_errors
    | [ "get_pred"; who ] ->
        Eio.traceln "getting pred of %s" who;
        let res = get ~addr:who ~endpoint:"pred" ~sw ~env in
        Eio.traceln "pred is %s" (res |> Result.get_ok)
    | "debug" :: _ ->
        Eio.traceln "Current node details: \n%s" @@ Node.show !node
    | "help" :: _ -> Eio.traceln "%s" @@ usage ()
    (* =======================MANUAL STATE MAINTANENCE COMMANDS======================================= *)
    | "redistribute" :: _ -> Net.redistribute node ~sw ~env |> handle_errors
    | "stabilize" :: _ -> Net.stabilize node ~sw ~env |> handle_errors
    | "fingers" :: _ -> Eio.traceln "Fixing fingers table..."
    | "set_my_succ" :: addr :: _ ->
        Eio.traceln "Manually setting successor to %s" addr;
        let _ = parse_addr addr in
        node := { !node with succ = Some (addr_pair addr) }
    | "set_my_pred" :: addr :: _ ->
        Eio.traceln "Manually setting predecessor to %s" addr;
        let _ = parse_addr addr in
        node := { !node with pred = Some (addr_pair addr) }
    | "endpoint" :: s :: _ ->
        (* for testing *)
        Eio.traceln "Unknown endpoint";
        (let* succ =
           if Option.is_none !node.succ then Error `NoSuccessor
           else Ok (Option.get !node.succ)
         in
         get ~addr:(snd succ) ~endpoint:s ~sw ~env)
        |> handle_errors
    | _ -> Eio.traceln "%s" @@ usage ()
  done;
  Ok ()
