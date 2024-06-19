let ( let* ) x f = Result.bind x f

let handle_errors r =
  match r with
  | Ok _ -> ()
  | Error `PortParseError -> Eio.traceln "Error parsing port"
  | Error `IpAddrParseError -> Eio.traceln "Error parsing ip addr"
  | Error `NoSuccessor -> Eio.traceln "No successor node set"
  | Error (`Msg s) -> Eio.traceln "Error Msg: %s" s
  | Error `ErrorStatus -> Eio.traceln "Error Status"

let spawn sw env node =
  for i = 0 to 10 do
    Eio.traceln "Stabilizing...";
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.5;
    Net.stabilize node ~sw ~env |> handle_errors
  done;
  while true do
    Eio.traceln "Stabilizing...";
    Eio.Time.sleep (Eio.Stdenv.clock env) 30.;
    Net.stabilize node ~sw ~env |> handle_errors
  done
