(* The endpoints *)
open Node
open Json_types

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

  let handler _socket request (body : Cohttp_eio.Body.t) =
    Eio.traceln "LOG: %s " (Http.Request.resource request);

    match Http.Request.resource request with
    | "/" -> (Http.Response.make (), Cohttp_eio.Body.of_string text)
    | "/lookup" when Http.Request.meth request = `POST -> (
        let body_str = Eio.Flow.read_all body in
        let body_json =
          lookup_of_yojson (Yojson.Safe.from_string body_str) |> Result.get_ok
          (* TODO: handle errors here. Just need to return results for endpoints and add cases to handle_errors and call it at the end of the handler *)
        in
        (* TODO: we should really validate that the data is good before we just mutate our state based on an arbitrary request *)
        let found =
          !node.map
          |> List.find_opt (fun (sha, _) ->
                 sha = Digestif.SHA1.of_hex body_json.sha1_hex)
        in
        match found with
        | None ->
            let resp_json = {|"error": "node not found"|} in
            (Http.Response.make (), Cohttp_eio.Body.of_string resp_json)
        | Some (_, payload) ->
            let found =
              { success = "node found"; payload } |> Json_types.found_to_yojson
            in
            let resp_json = found |> Yojson.Safe.to_string in
            (Http.Response.make (), Cohttp_eio.Body.of_string resp_json))
    | "/store" when Http.Request.meth request = `POST ->
        Eio.traceln "Storing endpoint";
        let body_str = Eio.Flow.read_all body in
        let body_json =
          payload_of_yojson (Yojson.Safe.from_string body_str) |> Result.get_ok
          (* TODO: handle errors here. Just need to return results for endpoints and add cases to handle_errors and call it at the end of the handler *)
        in
        (* TODO: we should really validate that the data is good before we just mutate our state based on an arbitrary request *)
        node :=
          {
            !node with
            map =
              (body_json.sha1_hex |> Digestif.SHA1.of_hex, body_json.payload)
              :: !node.map;
          };

        let resp_json = {|"success": "payload stored successfully"|} in
        (Http.Response.make (), Cohttp_eio.Body.of_string resp_json)
    | "/succ" when Http.Request.meth request = `POST ->
        let body_str = Eio.Flow.read_all body in
        let body_json =
          node_of_yojson (Yojson.Safe.from_string body_str) |> Result.get_ok
          (* TODO: handle errors here. Just need to return results for endpoints and add cases to handle_errors and call it at the end of the handler *)
        in
        (* TODO: we should really validate that the data is good before we just mutate our state based on an arbitrary request *)
        node :=
          {
            !node with
            succ = Some (Digestif.SHA1.of_hex body_json.sha1_hex, body_json.addr);
          };

        let resp_json = {|"success": "successor set successfully"|} in
        (Http.Response.make (), Cohttp_eio.Body.of_string resp_json)
    | "/pred" when Http.Request.meth request = `POST ->
        let body_str = Eio.Flow.read_all body in
        let body_json =
          node_of_yojson (Yojson.Safe.from_string body_str) |> Result.get_ok
          (* TODO: handle errors here. Just need to return results for endpoints and add cases to handle_errors and call it at the end of the handler *)
        in
        (* TODO: we should really validate that the data is good before we just mutate our state based on an arbitrary request *)
        node :=
          {
            !node with
            pred = Some (Digestif.SHA1.of_hex body_json.sha1_hex, body_json.addr);
          };

        let resp_json = {|"success": "predecessor set successfully"|} in
        (Http.Response.make (), Cohttp_eio.Body.of_string resp_json)
    | "/succ" when Http.Request.meth request = `GET ->
        let json =
          match !node.succ with
          | Some (sha, addr) ->
              { sha1_hex = Digestif.SHA1.to_hex sha; addr }
              |> node_to_yojson |> Yojson.Safe.to_string
          | None -> "{\"error\":\"no successor set\"}"
        in
        (Http.Response.make (), Cohttp_eio.Body.of_string json)
    | "/pred" when Http.Request.meth request = `GET ->
        let json =
          match !node.pred with
          | Some (sha, addr) ->
              { sha1_hex = Digestif.SHA1.to_hex sha; addr }
              |> node_to_yojson |> Yojson.Safe.to_string
          | None -> "{\"error\":\"no predecessor set\"}"
        in

        (Http.Response.make (), Cohttp_eio.Body.of_string json)
    | "/notify" when Http.Request.meth request = `POST ->
        let body_str = Eio.Flow.read_all body in
        let body_json =
          node_of_yojson (Yojson.Safe.from_string body_str) |> Result.get_ok
          (* TODO: handle errors here. Just need to return results for endpoints and add cases to handle_errors and call it at the end of the handler *)
        in
        (* TODO: we should really validate that the data is good before we just mutate our state based on an arbitrary request *)
        Net.notify node body_json;

        let resp_json = {|"success": "notify success"|} in
        (Http.Response.make (), Cohttp_eio.Body.of_string resp_json)
    | "/redistribute" when Http.Request.meth request = `GET ->
        let map = !node.map in
        (* for each value in the map, check if the key is between the pred and me, if it is not then it should be sent to the predecessor to redistribute *)
        let pred = Option.get !node.pred in
        let to_move =
          map
          |> List.filter (fun kv ->
                 not @@ Node.within_range_ce pred kv !node.id)
        in
        let payload =
          to_move
          |> List.map (fun (k, v) ->
                 { sha1_hex = Digestif.SHA1.to_hex k; payload = v })
          |> Json_types.redistribution_to_yojson |> Yojson.Safe.to_string
        in
        (Http.Response.make (), Cohttp_eio.Body.of_string payload)
    | s ->
        (* any other enpoints hit this *)
        ( Http.Response.make (),
          Cohttp_eio.Body.of_string (Printf.sprintf "Your route is: %s" s) )
  in
  Eio.traceln "Creating socket on ip:%s and port:%i" (Ipaddr.to_string ip) port;
  let socket =
    Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets ip), port))
  and server = Cohttp_eio.Server.make ~callback:handler () in
  let () = Cohttp_eio.Server.run socket server ~on_error:log_warning in
  Ok ()
