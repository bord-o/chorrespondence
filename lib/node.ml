open Digestif

type t = {
  id : SHA1.t * string;
  succ : (SHA1.t * string) option;
  pred : (SHA1.t * string) option;
  map : (SHA1.t * string) list;
  in_ring : bool;
}
[@@deriving show]

(* TODO: extract lookup/store/get/set into functions to be shared with the join/find_succ algorithms *)
(* TODO: write the notify function *)
(* TODO: write the stabilize function *)
(* TODO: figure out what needs to be scheduled*)

let dist_sha1 (lh, _) (rh, _) =
  let aux sha = "0x" ^ Digestif.SHA1.to_hex sha |> Z.of_string in
  let m = Z.pow (Z.of_int 2) 160 in
  let l = aux lh in
  let r = aux rh in
  Z.((r - l + m) mod m)

let within_range_ce start id end_ =
  let dist_start_id = dist_sha1 start id in
  let dist_start_end = dist_sha1 start end_ in
  dist_start_id > Z.of_int 0 && dist_start_id <= dist_start_end

let within_range_oe start id end_ =
  let dist_start_id = dist_sha1 start id in
  let dist_start_end = dist_sha1 start end_ in
  dist_start_id > Z.of_int 0 && dist_start_id < dist_start_end

(* type response = *)
(* | LookupSuccess of Message.t *)
(* | LookupFailure of string *)
(* | StoreSuccess of t *)
(* | StoreFailure of string *)

(* TENANCY *)
(* ======= *)

(* TODO: need to get the data I'm responsible for *)
(* let join me entry_node_addr_port = *)
(* { *)
(* me with *)
(* succ = Some (SHA1.digest_string entry_node_addr_port, entry_node_addr_port); *)
(* pred = None (* TODO: need to query for succ's pred and set it here *); *)
(* in_ring = true; *)
(* } *)

(* let leave me = { me with succ = None; pred = None; in_ring = false } *)

(* SEARCHING *)
(* ========= *)

(* let lookup sha me = *)
(* if List.mem_assoc sha me.map then LookupSuccess (List.assoc sha me.map) *)
(* else *)
(* failwith *)
(* "TODO: call succ's lookup endpoint and return the response Message.t" *)

(* let store (sha, msg) me = *)
(* if fst me.id >= sha && sha >= fst (Option.get me.pred) then *)
(* StoreSuccess { me with map = (sha, msg) :: me.map } *)
(* else failwith "TODO: call succ's store enpoint and return response" *)

(* NETWORK MAINTENANCE *)
(* =================== *)

(* let stabilize me = me *)
(* let notify me = me *)
(* let fix_fingers me = me *)

(* ENDPOINTS TO MAKE EVERYTHING WORK *)
(* ================================= *)
