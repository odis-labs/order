
type ordering =
  [ `Less
  | `Equal
  | `Greater
  ]

module Ordering = struct
  type t = ordering

  let inspect formatter t =
    match t with
    | `Less    -> Format.fprintf formatter "Less"
    | `Equal   -> Format.fprintf formatter "Equal"
    | `Greater -> Format.fprintf formatter "Greater"

  let display formatter t =
    match t with
    | `Less    -> Format.fprintf formatter "<"
    | `Equal   -> Format.fprintf formatter "="
    | `Greater -> Format.fprintf formatter ">"

  let equal t1 t2 =
    match t1, t2 with
    | `Less, `Less
    | `Equal, `Equal
    | `Greater, `Greater -> true
    | _ -> false

  let compare t1 t2 =
    match t1, t2 with
    | `Less, `Less -> `Equal
    | `Equal, `Equal -> `Equal
    | `Greater, `Greater -> `Equal
    | `Less, _  -> `Less
    | `Equal, `Less -> `Greater
    | `Equal, `Greater -> `Less
    | `Greater, _  -> `Greater

  let to_int t =
    match t with
    | `Less -> -1
    | `Equal -> 0
    | `Greater -> 1

  let of_int i =
    if i < 0 then `Less    else
    if i > 0 then `Greater else
    `Equal

  let invert t =
    match t with
    | `Less -> `Greater
    | `Equal -> `Equal
    | `Greater -> `Less
end

type 'a equality = 'a -> 'a -> bool

module Equality = struct
  type 'a t = 'a equality

  (* Equality testing functions *)
  let unit () () = true
  let bool : bool equality = Pervasives.(=)
  let char : char equality = Pervasives.(=)
  let int : int equality = Pervasives.(=)
  let int32 : int32 equality = Int32.equal
  let int64 : int64 equality = Int64.equal
  let float : float equality = Pervasives.(=)
  let string : string equality = Pervasives.(=)
  let bytes : bytes equality = Pervasives.(=)

  let rec list eq t1 t2 =
    match t1, t2 with
    | [], [] -> true
    | [], _ | _, [] -> false
    | a1 :: t1', a2 :: t2' ->
      eq a1 a2 && list eq t1' t2'

  exception Arrays_not_eq

  let array eq t1 t2 =
    if Array.length t1 <> Array.length t2 then false
    else if Array.length t1 = 0 then true
    else try
      Array.iter2
        (fun a1 a2 ->
           if not (eq a1 a2)
           then raise Arrays_not_eq)
        t1 t2;
      true
    with Arrays_not_eq -> false

  let option eq t1 t2 =
    match t1, t2 with
    | None, None -> true
    | Some _, None | None, Some _ -> false
    | Some a1, Some a2 -> eq a1 a2

  let result eq_ok eq_err t1 t2 =
    match t1, t2 with
    | Error b1, Error b2 -> eq_err b1 b2
    | Ok _, Error _ | Error _, Ok _ -> false
    | Ok a1, Ok a2 -> eq_ok a1 a2

  let ref eq t1 t2 = eq !t1 !t2

  let pair eq_a eq_b (a1, b1) (a2, b2) =
    eq_a a1 a2 && eq_b b1 b2

  let by proj eq =
    fun a1 a2 -> eq (proj a1) (proj a2)
end

type 'a comparator = 'a -> 'a -> ordering

module Comparator = struct
  type 'a t = 'a comparator

  let unit () () = `Equal

  let bool t1 t2 =
    let legacy_cmp : bool -> bool -> int = Pervasives.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let char t1 t2 =
    let legacy_cmp : char -> char -> int = Pervasives.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let int t1 t2 =
    let legacy_cmp : int -> int -> int = Pervasives.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let int32 t1 t2 =
    let legacy_cmp : int32 -> int32 -> int = Int32.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let int64 t1 t2 =
    let legacy_cmp : int64 -> int64 -> int = Int64.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let float t1 t2 =
    let legacy_cmp : float -> float -> int = Pervasives.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let string t1 t2 =
    let legacy_cmp : string -> string -> int = Pervasives.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let string t1 t2 =
    let legacy_cmp : string -> string -> int = Pervasives.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let bytes t1 t2 =
    let legacy_cmp : bytes -> bytes -> int = Pervasives.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let rec list cmp t1 t2 =
    match t1, t2 with
    | [], [] -> `Equal
    | [],  _ -> `Less
    | _ , [] -> `Greater
    | a1 :: t1', a2 :: t2' ->
      begin match cmp a1 a2 with
      | `Equal -> list cmp t1' t2'
      | other -> other
      end

  exception Array_cmp of ordering

  let array cmp t1 t2 =
    let t1_len = Array.length t1 in
    let t2_len = Array.length t2 in
    if t1_len = 0 && t2_len = 0 then `Equal
    else try
      Array.iter2
        (fun a1 a2 ->
           let ordering = cmp a1 a2 in
           if ordering <> `Equal then
             raise (Array_cmp ordering))
        t1 t2;
      `Equal
    with Array_cmp ordering -> ordering

  let option cmp t1 t2 =
    match t1, t2 with
    | None, None -> `Equal
    | None, Some _ -> `Less
    | Some _, None -> `Greater
    | Some a1, Some a2 -> cmp a1 a2

  let result cmp_ok cmp_err t1 t2 =
    match t1, t2 with
    | Error b1, Error b2 -> cmp_err b1 b2
    | Error _, Ok _ -> `Less
    | Ok _, Error _  -> `Greater
    | Ok a1, Ok a2 -> cmp_ok a1 a2

  let ref cmp t1 t2 = cmp !t1 !t2

  let pair cmp_a cmp_b (a1, b1) (a2, b2) =
    match cmp_a a1 a2 with
    | `Equal -> cmp_b b1 b2
    | other -> other

  let invert cmp a1 a2 =
    Ordering.invert (cmp a1 a2)

  let ascending cmp = cmp
  let descending = invert

  let by proj cmp =
    fun a1 a2 -> cmp (proj a1) (proj a2)

  let rec lexical comparators r1 r2 =
    match comparators with
    | [] -> `Equal
    | cmp :: rest ->
      begin match cmp r1 r2 with
      | `Equal -> lexical rest r1 r2
      | other -> other
      end

  let to_integral cmp =
    fun a1 a2 ->
      Ordering.to_int (cmp a1 a2)
end


(* Equality *)
module type Equal0 = sig
  type t
  val equal : t equality
end

module Equal0 = struct
  module type Extension = sig
    type t
    val equal : t equality
    val not_equal : t -> t -> bool
    val ( = ) : t -> t -> bool
    val ( <> ) : t -> t -> bool
  end

  module Extend (Base : Equal0) = struct
    let equal = Base.equal
    let not_equal t1 t2 = not (Base.equal t1 t2)
    let (=) = equal
    let (<>) = not_equal
  end
end


module type Equal1 = sig
  type 'a t
  val equal : 'a equality -> 'a t -> 'a t -> bool
end

module Equal1 = struct
  module type Extension = sig
    type 'a t
    val equal : 'a equality -> 'a t -> 'a t -> bool
    val not_equal : 'a equality -> 'a t -> 'a t -> bool
  end

  module Extend (Base : Equal1) = struct
    let equal = Base.equal
    let not_equal equal_a t1 t2 = not (Base.equal equal_a t1 t2)
  end
end

module type Equal2 = sig
  type ('a, 'b) t
  val equal : 'a equality -> 'b equality -> ('a, 'b) t -> ('a, 'b) t -> bool
end

module Equal2 = struct
  module type Extension = sig
    type ('a, 'b) t
    val equal : 'a equality -> 'b equality -> ('a, 'b) t -> ('a, 'b) t -> bool
    val not_equal : 'a equality -> 'b equality -> ('a, 'b) t -> ('a, 'b) t -> bool
  end

  module Extend (Base : Equal2) = struct
    let equal = Base.equal
    let not_equal equal_a equal_b t1 t2 = not (Base.equal equal_a equal_b t1 t2)
  end
end

module type Equal = Equal0
module Equal = Equal0


(* Ordering *)
module type Ordered0 = sig
  type t
  val compare : t -> t -> ordering
end

module Ordered0 = struct
  module type Extension = sig
    type t
    val compare : t -> t -> ordering

    include Equal0.Extension with type t := t

    val ( < ) : t -> t -> bool
    val ( > ) : t -> t -> bool
    val ( <= ) : t -> t -> bool
    val ( >= ) : t -> t -> bool
    val min : t -> t -> t
    val max : t -> t -> t
    val comparing  : ('a -> t) -> 'a comparator
    val between : min: t -> max: t -> t -> bool
    val clamp : min: t -> max: t -> t -> t
  end

  module Extend (Base : Ordered0) = struct
    let compare = Base.compare

    include Equal0.Extend(struct
        type nonrec t = Base.t
        let equal a b =
          match Base.compare a b with
          | `Equal -> true
          | `Less | `Greater -> false
      end)

    let less a b =
      match Base.compare a b with
      | `Less -> true
      | `Equal | `Greater -> false

    let greater a b =
      match Base.compare a b with
      | `Greater -> true
      | `Less | `Equal -> false

    let less_or_equal a b = not (greater a b)
    let greater_or_equal a b = not (less a b)

    let (<) = less
    let (>) = greater
    let (<=) = less_or_equal
    let (>=) = greater_or_equal

    let min a b = if a < b then a else b
    let max a b = if a > b then a else b

    let comparing f =
      Comparator.by f compare

    let between ~min ~max x =
      if min <= max then
        raise (Invalid_argument "between: min <= max")
      else
      if x < min then false else
      if x > max then false else
      true

    let clamp ~min:min_val ~max:max_val x =
      if min_val <= max_val then
        raise (Invalid_argument "between: min <= max")
      else
        min max_val (max min_val x)
  end

end


(* Ord1 *)
module type Ordered1 = sig
  type 'a t
  val compare : ('a -> 'a -> ordering) -> 'a t -> 'a t -> ordering
end

module Ordered1 = struct
  module type Extension = sig
    type 'a t
    val compare : ('a -> 'a -> ordering) -> 'a t -> 'a t -> ordering
    val min : ('a -> 'a -> ordering) -> 'a t -> 'a t -> 'a t
    val max : ('a -> 'a -> ordering) -> 'a t -> 'a t -> 'a t
  end

  module Extend (Base : Ordered1) = struct
    let compare = Base.compare

    let min cmp_a t1 t2 =
      match Base.compare cmp_a t1 t2 with
      | `Less -> t1
      | `Equal | `Greater -> t2

    let max cmp_a t1 t2 =
      match Base.compare cmp_a t1 t2 with
      | `Greater -> t1
      | `Equal | `Less -> t2
  end
end


(* Ord2 *)
module type Ordered2 = sig
  type ('a, 'b) t

  val compare :
    ('a -> 'a -> ordering) ->
    ('b -> 'b -> ordering) ->
    ('a, 'b) t -> ('a, 'b) t -> ordering
end

module Ordered2 = struct
  module type Extension = sig
    type ('a, 'b) t

    val compare :
      ('a -> 'a -> ordering) ->
      ('b -> 'b -> ordering) ->
      ('a, 'b) t -> ('a, 'b) t -> ordering
    val min :
      ('a -> 'a -> ordering) ->
      ('b -> 'b -> ordering) ->
      ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    val max :
      ('a -> 'a -> ordering) ->
      ('b -> 'b -> ordering) ->
      ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  end

  module Extend (Base : Ordered2) = struct
    let compare = Base.compare

    let min cmp_a cmp_b a b =
      match Base.compare cmp_a cmp_b a b with
      | `Less -> a
      | `Equal | `Greater -> b

    let max cmp_a cmp_b a b =
      match Base.compare cmp_a cmp_b a b with
      | `Greater -> a
      | `Equal | `Less -> b
  end
end

module Ordered = Ordered0
module type Ordered = Ordered0

let compare : int -> int -> ordering = Comparator.int

let ( = )  : int -> int -> bool = Pervasives.( = )
let ( <> ) : int -> int -> bool = Pervasives.( <> )
let ( < )  : int -> int -> bool = Pervasives.( < )
let ( > )  : int -> int -> bool = Pervasives.( > )
let ( <= ) : int -> int -> bool = Pervasives.( <= )
let ( >= ) : int -> int -> bool = Pervasives.( >= )
let min    : int -> int -> int  = Pervasives.min
let max    : int -> int -> int  = Pervasives.max
let is = Pervasives.(==)
let (==) = `Deprecated Pervasives.(==)


module Magic = struct
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
end
