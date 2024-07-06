(* six seems to be good, can improve granularity by comparing on binary representation *)
let difficulty = 7

let hash_listing listing nonce =
  let combined = listing ^ nonce in
  Digestif.BLAKE2B.digest_string combined

let validate hash =
  let prefix = String.make difficulty '0' in
  String.sub (Digestif.BLAKE2B.to_hex hash) 0 difficulty = prefix

let rec proof_of_work data nonce =
  let nonce_str = string_of_int nonce in
  let hash = hash_listing data nonce_str in
  if validate hash then (nonce_str, hash) else proof_of_work data (nonce + 1)

(* let () = *)
(* let listing = "Coffee Maker For Sale\n\n$15\n\n2699109887" in *)
(* let nonce_str, hash = proof_of_work listing 0 in *)
(* Printf.printf "Found! \nnonce: %s\nhash: %s\n" nonce_str *)
(* (Digestif.BLAKE2B.to_hex hash); *)
(* Printf.printf "Validating...\n%b\n" *)
(* (validate (hash_listing listing nonce_str)) *)
