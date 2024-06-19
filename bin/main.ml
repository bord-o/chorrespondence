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

let () =
  print_endline "\n\n===================================================\n\n"

let ( let* ) x f = Result.bind x f
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

(* Upon starting th program, we can either be the first node, or join *)
(* After this, we can use the rest of the api *)
let get_task init me_addr join_addr =
  if init then `Init me_addr else `Join (me_addr, join_addr)

let handle_errors r =
  match r with
  | Ok _ -> ()
  | Error `PortParseError -> Eio.traceln "Error parsing port"
  | Error `IpAddrParseError -> Eio.traceln "Error parsing ip addr"
  | Error `NoSuccessor -> Eio.traceln "No successor node set"
  | Error (`Msg s) -> Eio.traceln "Error Msg: %s" s
  | Error `ErrorStatus -> Eio.traceln "Errorstatus"

let main () =
  let () = validate () in
  let task = get_task !init !me_addr !join_addr in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let me : Node.t =
    match task with
    | `Init me_addr ->
        print_endline "Initializing connection and picking a port to use...";
        let id = (Digestif.SHA1.digest_string me_addr, me_addr) in
        { id; succ = None; pred = None; map = []; in_ring = true }
    | `Join (me_addr, join_addr) ->
        Printf.printf "Joining the network using peer %s...\n" join_addr;
        let id = (Digestif.SHA1.digest_string me_addr, me_addr) in
        { id; succ = None; pred = None; map = []; in_ring = false }
  in

  let shared_me = ref me in
  let () =
    (match task with
    | `Init _ ->
        Eio.traceln "ok";
        Ok ()
    | `Join (_, join_addr) -> (
        let succsucc =
          Eio.traceln "sending request";
          let* resp = Net.get ~addr:join_addr ~endpoint:"succ" ~sw ~env in
          resp |> Yojson.Safe.from_string |> Json_types.node_of_yojson
          |> Result.map_error (fun e ->
                 `Msg (Printf.sprintf "Failed to find prec node: %s" e))
        in
        match succsucc with
        | Ok succsucc ->
            let ask =
              {
                id = addr_pair join_addr;
                succ =
                  Some (succsucc.sha1_hex |> Digestif.SHA1.of_hex, succsucc.addr);
                pred = None;
                map = [];
                in_ring = true;
              }
            in

            Eio.traceln "Joining the network using peer %s...\n" join_addr;
            let* res = Net.find_successor ask me.id ~sw ~env in
            Ok (shared_me := { !shared_me with succ = Some res })
        | Error (`Msg _) ->
            Ok
              (shared_me :=
                 { !shared_me with succ = Some (addr_pair join_addr) })
        | Error e -> Error e))
    |> handle_errors
  in

  Eio.Fiber.fork ~sw (fun () ->
      Endpoints.spawn sw env shared_me |> handle_errors);
  Eio.Fiber.fork ~sw (fun () -> Console.spawn sw env shared_me |> handle_errors);
  Eio.Fiber.fork ~sw (fun () -> Daemon.spawn sw env shared_me)
(* TODO: the server and client need to share the node.t memory *)
(* I could just use a queue to hold the changes *)
(* Or just use a mutex *)

let () = main ()
