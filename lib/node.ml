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

let addr_pair addr = (Digestif.SHA1.digest_string addr, addr)
let addr_hex addr = Digestif.SHA1.digest_string addr |> Digestif.SHA1.to_hex

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
