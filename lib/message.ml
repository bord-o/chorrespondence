type t = {
  chat_id : string;
  message_index : int;
  sender_id : string;
  timestamp : float;
  data : string;
}
[@@deriving show]

let gen_key msg =
  Digestif.SHA1.digest_string
    (msg.chat_id ^ "|" ^ Int.to_string msg.message_index)
