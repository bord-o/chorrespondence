open Digestif

type t = {
  id : SHA1.t * string;
  succ : (SHA1.t * string) option;
  pred : (SHA1.t * string) option;
  map : (SHA1.t * string) list;
  in_ring : bool;
} [@@deriving show]

(* type response *)
(*
(* TENANCY *)
(* ======= *)

val join : t -> string -> t (* attempts to join our node, using node n as an entrypoint, returning our node after updating it to reflect our new position in the ring *)
val leave : t -> t (* leaves the network by updating our tables to be empty *)


(* SEARCHING *)
(* ========= *)

val lookup : Digestif.SHA1.t -> t -> response
val store : (Digestif.SHA1.t * Message.t) -> t -> response


(* NETWORK MAINTENANCE *)
(* =================== *)

val stabilize : t -> t (* n asks succ for its pred, and updates accordingly *)
val notify : t -> t (* notifies n's succ of its existence so it can change its pred to n *)
val fix_fingers : t -> t (* updates finger tables TODO: fingers will be implemented later *)


(* ENDPOINTS TO MAKE EVERYTHING WORK *)
(* ================================= *)


(*
  we need to query the new succ's pred in order to update ours (get_pred)
  we need to update the new succ's pred when we join the network (update_pred id)

  we need to do lookups by checking our state, then calling lookup on our successor if the node is not found   (lookup {sha} 200ok w/ response)
  we need to do stores by checking if we should store, then calling store on our successor if not (store {sha:val} 200ok w/ response)
*)
*)
