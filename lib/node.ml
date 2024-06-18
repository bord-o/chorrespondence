open Digestif

type t = {
  id : SHA1.t * string;
  succ : (SHA1.t * string) option;
  pred : (SHA1.t * string) option;
  map : (SHA1.t * string) list;
  in_ring : bool;
}
[@@deriving show]

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
