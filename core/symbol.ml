type symbol = (string * int[@equal fun (a, _) (b, _) -> a = b])
[@@deriving show, eq]

let nextsym = ref 0

let size_hint = 128

let hashtable = Hashtbl.create size_hint

let name (s, _) = s

let string_of_symbol (sym : symbol) : string =
  match sym with name, i -> name ^ "$" ^ string_of_int i

let symbol (name : string) =
  try
    let i = Hashtbl.find hashtable name in
    (name, i)
  with Not_found ->
    let i = !nextsym in
    nextsym := i + 1;
    Hashtbl.add hashtable name i;
    (name, i)
