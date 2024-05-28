(* let () = *)
(* print_endline *)
(* "      _                                                    \ *)
    (* _                     \n\ *)
   (* \  ___| |__   ___  _ __ _ __ ___  ___ _ __   ___  _ __   __| | ___ _ __   \ *)
    (* ___ ___ \n\ *)
   (* \ / __| '_ \\ / _ \| '__| '__/ _ \/ __| '_ \\ / _ \| '_ \\ / _` |/ _ \\ '_ \ *)
    (* \\ / __/ _ \\\n\ *)
    (* | (__| | | | (_) | |  | | |  __/\__ \\ |_) | (_) | | | | (_| |  __/ | | | \ *)
    (* (_|  __/\n\ *)
   (* \ \___|_| |_|\___/|_|  |_|  \___||___/ .__/ \___/|_| |_|\__,_|\___|_| \ *)
    (* |_|\___\___|\n\ *)
   (* \                                    \ *)
    (* |_|                                          \n\n\n" *)

open Chorrespondence
open Chorrespondence.Node
open Cohttp_eio

let ( let* ) x f = Result.bind x f

let () =
  print_endline "\n\n===================================================\n\n"

let usage_msg = "chorrespondence [-i] [-j HOST:PORT]"
let join_addr = ref ""
let me_addr = ref ""
let init = ref false
let anon_fun _ = ()

let speclist =
  [
    ( "-j",
      Arg.Set_string join_addr,
      "HOST:PORT\t        Set address of an initial node used to join the \
       network." );
    ( "-a",
      Arg.Set_string me_addr,
      "HOST:PORT\t        Set the local node address" );
    ("-i", Arg.Set init, "\t\t\tInitialze the network as the first node.");
  ]

let validate () =
  let () = Arg.parse speclist anon_fun usage_msg in
  let () =
    Printf.printf "init: %b, me_addr %s, join_addr %s\n\n" !init !me_addr
      !join_addr
  in
  if
    Array.length Sys.argv < 2
    || ((not @@ Array.mem "-j" Sys.argv) && (not @@ Array.mem "-i" Sys.argv))
  then (
    Arg.usage speclist usage_msg;
    failwith "Usage")
  else if not @@ Array.mem "-a" Sys.argv then (
    Arg.usage speclist usage_msg;
    failwith "Local address is required.")
  else ()
(* Main functionality here *)

let parse_addr addr =
  match addr |> String.split_on_char ':' with
  | [ ip; port ] ->
      let port_int = int_of_string_opt port in
      if Option.is_none port_int then Error `PortParseError
      else Ok (ip, Option.get port_int)
  | _ -> Error `IpAddrParseError

(* Upon starting th program, we can either be the first node, or join *)
(* After this, we can use the rest of the api *)
let get_task init me_addr join_addr =
  if init then `Init me_addr else `Join (me_addr, join_addr)

let () = Logs.set_reporter (Logs_fmt.reporter ())
and () = Logs.Src.set_level Cohttp_eio.src (Some Debug)

let log_warning ex = Logs.warn (fun f -> f "%a" Eio.Exn.pp ex)

let handle_errors r =
  match r with
  | Ok _ -> ()
  | Error `PortParseError -> Eio.traceln "Error parsing port"
  | Error `IpAddrParseError -> Eio.traceln "Error parsing ip addr"
  | Error `NoSuccessor -> Eio.traceln "No successor node set"

let spawn_server sw env node =
  (* let (Ok (ip, port)) = parse_addr @@ snd !node.id in *)
  let* ip, port = parse_addr @@ snd !node.id in
  let text = "testtesttest" in

  let handler _socket request _body =
    match Http.Request.resource request with
    | "/" -> (Http.Response.make (), Cohttp_eio.Body.of_string text)
    | s ->
        ( Http.Response.make (),
          Cohttp_eio.Body.of_string (Printf.sprintf "Your route is: %s" s) )
    (* | _ -> *)
    (* (Http.Response.make ~status:`Not_found (), Cohttp_eio.Body.of_string "") *)
  in
  let socket =
    Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  and server = Cohttp_eio.Server.make ~callback:handler () in
  let () = Cohttp_eio.Server.run socket server ~on_error:log_warning in
  Ok ()

exception ExitConsole

let spawn_client sw env node =
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
             (Uri.of_string @@ Printf.sprintf "http://%s:%i/%s" ip port s)
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

let main () =
  let () = validate () in
  let task = get_task !init !me_addr !join_addr in
  let me : Node.t =
    match task with
    | `Init me_addr ->
        print_endline "Initializing connection and picking a port to use...";
        {
          id = (Digestif.SHA1.digest_string me_addr, me_addr);
          succ = None;
          pred = None;
          map = [];
          in_ring = true;
        }
    | `Join (me_addr, join_addr) ->
        Printf.printf "Joining the network using peer %s...\n" join_addr;
        {
          id = (Digestif.SHA1.digest_string me_addr, me_addr);
          succ = Some (Digestif.SHA1.digest_string join_addr, join_addr);
          pred = None;
          map = [];
          in_ring = true;
        }
  in

  let shared_me = ref me in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.fork ~sw (fun () -> spawn_server sw env shared_me |> handle_errors);
  Eio.Fiber.fork ~sw (fun () -> spawn_client sw env shared_me |> handle_errors)
(* TODO: the server and client need to share the node.t memory *)
(* I could just use a queue to hold the changes *)

(* TODO: initialize Eio and pass the switch to the node me to be used in server and client *)

let () = main ()
