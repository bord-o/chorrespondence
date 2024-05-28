(* The endpoints *)
open Cohttp_eio
open Node

let ( let* ) x f = Result.bind x f
let parse_addr = Ipaddr.with_port_of_string ~default:8080

let () = Logs.set_reporter (Logs_fmt.reporter ())
and () = Logs.Src.set_level Cohttp_eio.src (Some Debug)

let log_warning ex = Logs.warn (fun f -> f "%a" Eio.Exn.pp ex)

let handle_errors r =
  match r with
  | Ok _ -> ()
  | Error `PortParseError -> Eio.traceln "Error parsing port"
  | Error `IpAddrParseError -> Eio.traceln "Error parsing ip addr"
  | Error `NoSuccessor -> Eio.traceln "No successor node set"
  | Error (`Msg s) -> Eio.traceln "Error Msg: %s" s

let spawn sw env node =
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
      (`Tcp (Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets ip), port))
  and server = Cohttp_eio.Server.make ~callback:handler () in
  let () = Cohttp_eio.Server.run socket server ~on_error:log_warning in
  Ok ()
