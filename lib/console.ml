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
    | [] -> ()
    | [ "leave" ] ->
        Eio.traceln "Exiting...";
        Eio.Switch.fail sw ExitConsole
        (* TODO: make this leave the network gracefully *)
    | "store" :: "@" :: id :: msg ->
        let msg = String.concat " " msg in
        Eio.traceln "Storing message %s with content: %s" id msg
    | "store" :: _ -> Eio.traceln "Usage: "
    | [ "lookup"; id ] -> Eio.traceln "Looking up message %s" id
    | "lookup" :: _ -> Eio.traceln "Usage: lookup [id]"
    | "set_succ" :: addr :: _ ->
        Eio.traceln "Manually setting successor to %s" addr;
        let _ = parse_addr addr in
        node :=
          { !node with succ = Some (Digestif.SHA1.digest_string addr, addr) }
    | "set_pred" :: addr :: _ ->
        Eio.traceln "Manually setting predecessor to %s" addr;
        let _ = parse_addr addr in
        node :=
          { !node with pred = Some (Digestif.SHA1.digest_string addr, addr) }
    | "stabilize" :: _ -> Eio.traceln "Stabilizing..."
    | "notify" :: _ -> Eio.traceln "Notifying..."
    | "fingers" :: _ -> Eio.traceln "Fixing fingers table..."
    | "debug" :: _ ->
        Eio.traceln "Current node details: \n%s" @@ Node.show !node
    | "help" :: _ ->
        Eio.traceln
          "Usage: lookup [id] | store [@id] [message] | debug | leave | help"
    | s :: _ ->
        (let* succ =
           if Option.is_none !node.succ then Error `NoSuccessor
           else Ok (Option.get !node.succ)
         in
         let* ip, port = parse_addr @@ snd succ in

         let client = Client.make ~https:None env#net in
         let resp, body =
           Client.get ~sw client
             (Uri.of_string
             @@ Printf.sprintf "http://%s:%i/%s" (Ipaddr.to_string ip) port s)
         in
         if Http.Status.compare resp.status `OK = 0 then (
           Eio.traceln "%s"
           @@ Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int;
           Ok ())
         else (
           Eio.traceln "Error Status:%s" @@ Http.Status.to_string resp.status;
           Ok ()))
        |> handle_errors
  done;
  Ok ()
