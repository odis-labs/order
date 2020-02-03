open Order

let compare a b =
  let legacy_cmp : 'a -> 'a -> int = Stdlib.compare in
  let order = legacy_cmp a b in
  if order < 0 then
    `Less
  else
  if order > 0 then
    `Greater
  else
  `Equal

let equal = Stdlib.( = )

let ( = )  = Stdlib.( =  )
let ( <> ) = Stdlib.( <> )
let ( <  ) = Stdlib.( <  )
let ( >  ) = Stdlib.( >  )
let ( <= ) = Stdlib.( <= )
let ( >= ) = Stdlib.( >= )

let min = Stdlib.min
let max = Stdlib.max

let comparing f =
  Comparator.by f compare
