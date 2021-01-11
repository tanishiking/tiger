type pos = {
  lnum : int; [@equal fun _ _ -> true]
  bol : int; [@equal fun _ _ -> true]
}
[@@deriving show, eq]
