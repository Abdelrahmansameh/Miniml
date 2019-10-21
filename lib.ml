module StringOrd =
  struct
    type t = string
    let compare = compare
  end
module StringMap = Map.Make( StringOrd )
