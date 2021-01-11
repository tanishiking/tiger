exception Error of (Sourcepos.pos * string) [@@deriving show, eq]

let error loc fmt = Format.ksprintf (fun msg -> raise (Error (loc, msg))) fmt

let fatal fmt = Format.ksprintf failwith fmt
