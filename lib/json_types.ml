type node = { sha1_hex : string; addr : string } [@@deriving yojson]
type payload = { sha1_hex : string; payload : string } [@@deriving yojson]
type lookup = { sha1_hex : string } [@@deriving yojson]
type found = { success : string; payload : string } [@@deriving yojson]
