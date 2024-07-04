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
  let addr_string =
    match ip with
    | V4 a -> Ipaddr.V4.to_string a
    | V6 a -> Printf.sprintf "[%s]" @@ Ipaddr.V6.to_string a
  in
  let client = Client.make ~https:None env#net in
  let resp, body =
    Client.get ~sw client
      (Uri.of_string
      @@ Printf.sprintf "http://%s:%i/%s" addr_string port endpoint)
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

let closest_preceeding_node me id ~sw ~env =
  let* succ =
    match me.succ with Some s -> Ok s | None -> Error (`Msg "Succ not set")
  in
  if within_range_oe me.id succ id then
    let* succsucc =
      Eio.traceln "sending request";
      let* resp = get ~addr:(snd succ) ~endpoint:"succ" ~sw ~env in
      resp |> Yojson.Safe.from_string |> Json_types.node_of_yojson
      |> Result.map_error (fun e ->
             `Msg (Printf.sprintf "Failed to find prec node: %s" e))
    in
    Ok
      {
        id = succ;
        succ = Some (succsucc.sha1_hex |> Digestif.SHA1.of_hex, succsucc.addr);
        pred = None;
        map = [];
        in_ring = true;
      }
  else (
    Eio.traceln "ok";
    Ok me)

let rec find_successor me id ~sw ~env =
  Eio.traceln "asking %s for the successor of %s" (snd me.id) (snd id);
  let* succ =
    match me.succ with Some s -> Ok s | None -> Error (`Msg "Succ not set")
  in
  if id = me.id then Ok me.id
  else if within_range_ce me.id id succ then (
    Eio.traceln "in range...";
    Ok succ)
  else (
    Eio.traceln "getting closer";
    let* closest = closest_preceeding_node me id ~sw ~env in
    if closest.id = me.id then Ok me.id
    else
      (* this keeps everything working for the first node that joins *)
      find_successor closest id ~sw ~env)

let stabilize me ~sw ~env =
  Eio.traceln "Stabilizing...";
  let* succ =
    match !me.succ with Some s -> Ok s | None -> Error (`Msg "Succ not set")
  in
  let* res = get ~addr:(snd succ) ~endpoint:"pred" ~sw ~env in
  Eio.traceln "json pred %s" res;
  let () =
    match res |> Yojson.Safe.from_string |> Json_types.node_of_yojson with
    | Ok x ->
        if
          Node.within_range_ce !me.id
            (x.sha1_hex |> Digestif.SHA1.of_hex, x.addr)
            succ
        then
          me :=
            {
              !me with
              succ = Some (x.sha1_hex |> Digestif.SHA1.of_hex, x.addr);
            }
        else ()
    | Error _ -> Eio.traceln "No pred set"
  in

  let my_id =
    Json_types.node_to_yojson
      { sha1_hex = addr_hex (snd !me.id); addr = snd !me.id }
  in
  let* _ = post ~addr:(snd succ) ~endpoint:"notify" ~json:my_id ~sw ~env in
  Ok ()

let notify me (node : Json_types.node) =
  Eio.traceln "Notifying...";
  let node = (node.sha1_hex |> Digestif.SHA1.of_hex, node.addr) in
  if Option.is_none !me.succ then me := { !me with succ = Some node } else ();
  match !me.pred with
  | None -> me := { !me with pred = Some node }
  | Some id ->
      if Node.within_range_ce id node !me.id then
        me := { !me with pred = Some node }
      else ()

let redistribute me ~sw ~env =
  (* Ask succ if they are storing anything that I should be storing *)
  (* TODO: add an endpoint for redistribution. GET /redistribute which returns any nodes that the requesting node should be storing instead.
     The callee should then be able to just store the resulting nodes json *)
  Eio.traceln "Redistributing...";
  let* succ =
    match !me.succ with Some s -> Ok s | None -> Error (`Msg "Succ not set")
  in
  let* res = get ~addr:(snd succ) ~endpoint:"redistribute" ~sw ~env in
  Ok (Eio.traceln "redistributing: %s" res)
(* let () = *)
(* match res |> Yojson.Safe.from_string |> Json_types.node_of_yojson with *)
