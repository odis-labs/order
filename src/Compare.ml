
type ordering =
  | Less
  | Equal
  | Greater

module Ordering = struct
  type t = ordering =
    | Less
    | Equal
    | Greater

  let to_int t =
    match t with
    | Less -> -1
    | Equal -> 0
    | Greater -> 1

  let of_int i =
    if i < 0 then Less    else
    if i > 0 then Greater else
    Equal

  let is_less t =
    match t with
    | Less -> true
    | _ -> false

  let is_equal t =
    match t with
    | Equal -> true
    | _ -> false

  let is_greater t =
    match t with
    | Greater -> true
    | _ -> false
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
end

type 'a comparator = 'a -> 'a -> ordering

module Comparator = struct
  type 'a t = 'a comparator

  let unit () () = Equal

  let bool t1 t2 =
    let legacy_cmp : bool -> bool -> int = Pervasives.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let char t1 t2 =
    let legacy_cmp : char -> char -> int = Pervasives.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let int t1 t2 =
    let legacy_cmp : int -> int -> int = Pervasives.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let float t1 t2 =
    let legacy_cmp : float -> float -> int = Pervasives.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let string t1 t2 =
    let legacy_cmp : string -> string -> int = Pervasives.compare in
    Ordering.of_int (legacy_cmp t1 t2)

  let rec list cmp t1 t2 =
    match t1, t2 with
    | [], [] -> Equal
    | [],  _ -> Less
    | _ , [] -> Greater
    | a1 :: t1', a2 :: t2' ->
      let ordering = cmp a1 a2 in
      if ordering = Equal
      then list cmp t1' t2'
      else ordering

  exception Array_cmp of ordering

  let array cmp t1 t2 =
    let t1_len = Array.length t1 in
    let t2_len = Array.length t2 in
    if t1_len = 0 && t2_len = 0 then Equal
    else try
      Array.iter2
        (fun a1 a2 ->
           let ordering = cmp a1 a2 in
           if ordering <> Equal then
             raise (Array_cmp ordering))
        t1 t2;
      Equal
    with Array_cmp ordering -> ordering

  let option cmp t1 t2 =
    match t1, t2 with
    | None, None -> Equal
    | None, Some _ -> Less
    | Some _, None -> Greater
    | Some a1, Some a2 -> cmp a1 a2

  let result cmp_ok cmp_err t1 t2 =
    match t1, t2 with
    | Error b1, Error b2 -> cmp_err b1 b2
    | Error _, Ok _ -> Less
    | Ok _, Error _  -> Greater
    | Ok a1, Ok a2 -> cmp_ok a1 a2

  let ref cmp t1 t2 = cmp !t1 !t2

  let pair cmp_a cmp_b (a1, b1) (a2, b2) =
    let order = cmp_a a1 a2 in
    if order = Equal then
      cmp_b b1 b2
    else order
end


(* Equality *)

module type Equal0 = sig
  type t
  val equal : t equality
  val not_equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module Equal0 = struct
  module type Base = sig
    type t
    val equal : t equality
  end

  module Make (Base : Base) = struct
    let equal = Base.equal
    let not_equal t1 t2 = not (Base.equal t1 t2)
    let (=) = equal
    let (<>) = not_equal
  end
end


module type Equal1 = sig
  type 'a t
  val equal : 'a equality -> 'a t -> 'a t -> bool
  val not_equal : 'a equality -> 'a t -> 'a t -> bool
end

module Equal1 = struct
  module type Base = sig
    type 'a t
    val equal : 'a equality -> 'a t -> 'a t -> bool
  end

  module Make (Base : Base) = struct
    let equal = Base.equal
    let not_equal equal_a t1 t2 = not (Base.equal equal_a t1 t2)
  end
end

module type Equal2 = sig
  type ('a, 'b) t
  val equal : 'a equality -> 'b equality -> ('a, 'b) t -> ('a, 'b) t -> bool
  val not_equal : 'a equality -> 'b equality -> ('a, 'b) t -> ('a, 'b) t -> bool
end

module Equal2 = struct
  module type Base = sig
    type ('a, 'b) t
    val equal : 'a equality -> 'b equality -> ('a, 'b) t -> ('a, 'b) t -> bool
  end

  module Make (Base : Base) = struct
    let equal = Base.equal
    let not_equal equal_a equal_b t1 t2 = not (Base.equal equal_a equal_b t1 t2)
  end
end

module Equal = Equal0
module type Equal = Equal0


(* Ordering *)

module type Ordered0 = sig
  type t
  val compare : t -> t -> ordering

  include Equal0 with type t := t

  val less : t -> t -> bool
  val greater : t -> t -> bool
  val less_or_equal : t -> t -> bool
  val greater_or_equal : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
  val between : min: t -> max: t -> t -> bool
  val clamp : min: t -> max: t -> t -> t
end

module Ordered0 = struct
  module type Base = sig
    type t
    val compare : t -> t -> ordering
  end

  module Make (Base : Base) = struct
    let compare = Base.compare

    include Equal0.Make(struct
        type nonrec t = Base.t
        let equal a b =
          match Base.compare a b with
          | Equal -> true
          | Less | Greater -> false
      end)

    let less a b =
      match Base.compare a b with
      | Less -> true
      | Equal | Greater -> false

    let greater a b =
      match Base.compare a b with
      | Greater -> true
      | Less | Equal -> false

    let less_or_equal a b = not (greater a b)
    let greater_or_equal a b = not (less a b)

    let (<) = less
    let (>) = greater
    let (<=) = less_or_equal
    let (>=) = greater_or_equal

    let min a b = if a < b then a else b
    let max a b = if a > b then a else b

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
  val min : ('a -> 'a -> ordering) -> 'a t -> 'a t -> 'a t
  val max : ('a -> 'a -> ordering) -> 'a t -> 'a t -> 'a t
end
module Ordered1 = struct
  module type Base = sig
    type 'a t
    val compare : ('a -> 'a -> ordering) -> 'a t -> 'a t -> ordering
  end

  module Make (Base : Base) = struct
    let compare = Base.compare

    let min cmp_a t1 t2 =
      match Base.compare cmp_a t1 t2 with
      | Less -> t1
      | Equal | Greater -> t2

    let max cmp_a t1 t2 =
      match Base.compare cmp_a t1 t2 with
      | Greater -> t1
      | Equal | Less -> t2
  end
end

module type Ordered2 = sig
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

(* Ord2 *)
module Ordered2 = struct
  module type Base = sig
    type ('a, 'b) t

    val compare :
      ('a -> 'a -> ordering) ->
      ('b -> 'b -> ordering) ->
      ('a, 'b) t -> ('a, 'b) t -> ordering
  end

  module Make (Base : Base) = struct
    let compare = Base.compare

    let min cmp_a cmp_b a b =
      match Base.compare cmp_a cmp_b a b with
      | Less -> a
      | Equal | Greater -> b

    let max cmp_a cmp_b a b =
      match Base.compare cmp_a cmp_b a b with
      | Greater -> a
      | Equal | Less -> b
  end
end

module Ordered = Ordered0
(** Alias for interface builder for ordered monomorphic types. *)

module type Ordered = Ordered0

(* Public *)
let ( = )  : int -> int -> bool = Pervasives.( = )
let ( <> ) : int -> int -> bool = Pervasives.( <> )
let ( < )  : int -> int -> bool = Pervasives.( < )
let ( > )  : int -> int -> bool = Pervasives.( > )
let ( <= ) : int -> int -> bool = Pervasives.( <= )
let ( >= ) : int -> int -> bool = Pervasives.( >= )
let min    : int -> int -> int  = Pervasives.min
let max    : int -> int -> int  = Pervasives.max
let is = Pervasives.(==)
let (==) = Pervasives.(==)


module Magic = struct
  let compare a b =
    let legacy_cmp : 'a -> 'a -> int = Pervasives.compare in
    let order = legacy_cmp a b in
    if order < 0 then
      Less
    else
    if order > 0 then
      Greater
    else
      Equal

  let equal = Pervasives.( = )

  let ( = )  = Pervasives.( =  )
  let ( <> ) = Pervasives.( <> )
  let ( <  ) = Pervasives.( <  )
  let ( >  ) = Pervasives.( >  )
  let ( <= ) = Pervasives.( <= )
  let ( >= ) = Pervasives.( >= )

  let min = Pervasives.min
  let max = Pervasives.max
end

