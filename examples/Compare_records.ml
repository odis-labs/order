open Order

type row = {
  id: string;
  count: int;
  avg: float;
}

let id t = t.id
let count t = t.count
let avg t = t.avg

let r1 = { id = "a1"; count = 21; avg = 13.8 }
let r2 = { id = "a2"; count = 8;  avg = 18.4 }

(* Compare by avg, count, id *)
let compare =
  let open Comparator in
  lexical [
    by avg float;
    by count int;
    by id string;
  ]


