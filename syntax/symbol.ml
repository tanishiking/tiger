type symbol = (string * int[@equal fun (a, _) (b, _) -> a = b])
[@@deriving show, eq]

let nextsym = ref 0

let string_of_symbol (sym : symbol) : string =
  match sym with name, i -> name ^ "$" ^ string_of_int i

let symbol (name : string) : symbol =
  let i = !nextsym in
  nextsym := i + 1;
  (name, i)

let mk_symbol name sym = (name, sym)
