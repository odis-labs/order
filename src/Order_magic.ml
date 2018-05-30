open Order

let compare a b =
  let legacy_cmp : 'a -> 'a -> int = Pervasives.compare in
  let order = legacy_cmp a b in
  if order < 0 then
    `Less
  else
  if order > 0 then
    `Greater
  else
  `Equal

let equal = Pervasives.( = )

let ( = )  = Pervasives.( =  )
let ( <> ) = Pervasives.( <> )
let ( <  ) = Pervasives.( <  )
let ( >  ) = Pervasives.( >  )
let ( <= ) = Pervasives.( <= )
let ( >= ) = Pervasives.( >= )

let min = Pervasives.min
let max = Pervasives.max

let comparing f =
  Comparator.by f compare
