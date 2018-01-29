
(**
    Interfaces, modules and operations for ordering and comparison of OCaml values.

    This library provides the following features:

    {ul
    {- New {{: #type-order} [order]} type to replace integer-based ordering.}
    {- Extended {{: module-type-Equal/index.html} [Equal]} and {{:
    module-type-Ordered/index.html} [Ordered]} interfaces for custom types.}
    {- {{: #public} Public} comparison operations specialized to integers}
    {- Optional {{: #generic} Generic} module for polymorphic comparisons.}
    {- {{: Equal/index.html#funcs} Equality} and {{:
    Ordered/index.html#funcs} ordering} functions for common data
    types.}
    {- New physical equality operator {{: #val-is} [is]} and deprecated {{: #val-(==)} [==]}. }}*)

(** {1 Types} *)

(** Defines the relative order of two values. *)
type order =
  | Less    (** [a < b], [a] is less than [b]. *)
  | Equal   (** [a = b], [a] is equal to [b]. *)
  | Greater (** [a > b], [a] is greater than [b]. *)

(** Module for the {!type:order} type. *)
module Order : sig
  (** Defines the relative order of two values. *)
  type t = order =
    | Less    (** [a < b], [a] is less than [b]. *)
    | Equal   (** [a = b], [a] is equal to [b]. *)
    | Greater (** [a > b], [a] is greater than [b]. *)

  val to_int : t -> int
  (** Represents an ordering as an integers.

      The following mapping is used:

      {ul
      {li [Less] is [-1]}
      {li [Equal] is [0]}
      {li [Greater] is [1]}} *)

  val of_int : int -> t
  (** Converts an integer to an ordering.

      [of_int i] is translated as:

      {ul
      {li [Less]    if [i < 0]}
      {li [Equal]   if [i = 0]}
      {li [Greater] if [i > 0]}} *)

  val is_less : t -> bool
  (** Checks if the order is the [Lt] value. *)

  val is_equal : t -> bool
  (** Checks if the order is the [Eq] value. *)

  val is_greater : t -> bool
  (** Checks if the order is the [Gt] value. *)
end

type 'a equality = 'a -> 'a -> bool
(** The type of equality testing functions. *)

type 'a comparator = 'a -> 'a -> order
(** The type of order comparison functions. *)


(** {1:equality Equality}

    Equality comparisons for monomorphic and polymorphic types.

    This module defines interfaces and operations for equality comparison
    between values. Equality is an {{:
    https://en.wikipedia.org/wiki/Equivalence_relation} equivalence relation},
    which means that it must be: {i reflexive}, {i symmetric} and {i
    transitive}.

    Functions for both structural and physical equality are defined.

    User-defined types can implement the {!modtype:Equal}, {!modtype:Equal1} or
    {!modtype:Equal2} interfaces (according to the arity of the main type) to
    include specialized equality comparison functions.

    {b Note:} The extended version of [Equal] for polymorphic types does not
    include infix functions since they are only useful with two arguments and
    [Equal1] and [Equal2] would require extra arguments for each type
    parameter.

    {2 Example}

{[
open Ordering

module Book = struct
  type t = {
    title : string;
    author : string;
    isbn : int;
  }

  (* Equality by ISBN. *)
  include Equal.Make(struct
    type nonrec t = t

    let equal t1 t2 =
      Equal.string t1.isbn t2.isbn
  end)
end
]} *)


(** {2:equal Monomorphic Types}

    Equality comparisons for monomorphic types, like integers and strings. *)

(** Extended interface for equatable monomorphic types. *)
module type Equal0 = sig
  type t
  (** Abstract type for equatable values. *)

  val equal : t equality
  (** [equal t1 t2] tests if the values [t1] and [t2] are equal, and is used by
      the [=] and [<>] oreprators. *)

  val not_equal : t equality
  (** [not_equal t1 t2] tests if the values [t1] and [t2] are {i not} equal,
      and is used by [<>].

      [not_eq t1 t2] is a shorthand for [not (equal t1 t2)]. *)

  val ( = ) : t equality
  (** Operator alias for {!equal}. *)

  val ( <> ) : t equality
  (** Operator alias for {!not_equal}. *)
end

(** Extension builder module for equatable monomorphic types. *)
module Equal0 : sig

  (** Base interface for equatable monomorphic types. *)
  module type Base = sig
    type t
    (** Abstract type for equatable values. *)

    val equal : t equality
    (** [equal t1 t2] tests if the values [t1] and [t2] are equal, and is used by
        the [=] and [<>] oreprators. *)
  end

  (** Extends the base definition for equatable monomorphic types. *)
  module Make (Base : Base) : Equal0 with type t := Base.t


  (** {2:funcs Common equality testing functions} *)

  val unit : unit equality
  (** Equality testing function for values of type [unit]. *)

  val bool : bool equality
  (** Equality testing function for values of type [bool]. *)

  val char : char equality
  (** Equality testing function for values of type [char]. *)

  val int : int equality
  (** Equality testing function for values of type [int]. *)

  val int32 : int32 equality
  (** Equality testing function for values of type [int32]. *)

  val int64 : int64 equality
  (** Equality testing function for values of type [int64]. *)

  val float : float equality
  (** Equality testing function for values of type [float]. *)

  val string : string equality
  (** Equality testing function for values of type [string]. *)

  val bytes : bytes equality
  (** Equality testing function for values of type [bytes]. *)

  val list : 'a equality -> 'a list equality
  (** Equality testing function for values of type [list]. *)

  val array : 'a equality -> 'a array equality
  (** Equality testing function for values of type [array]. *)

  val option : 'a equality -> 'a option equality
  (** Equality testing function for values of type [option]. *)

  val result : 'a equality -> 'b equality -> ('a, 'b) result equality
  (** Equality testing function for values of type [result]. *)

  val ref : 'a equality -> 'a ref equality
  (** Equality testing function for values of type [ref]. *)

  val pair : 'a equality -> 'b equality -> ('a * 'b) equality
  (** Equality testing function for pairs of type ['a * 'b]. *)
end


(** {2:equal1 Polymorphic Unary Types}

    Equality comparisons for polymorphic unary types, like lists and option
    values. *)

(** Extended interface for equatable polymorphic unary types. *)
module type Equal1 = sig
  type 'a t
  (** Abstract type for equatable values. *)

  val equal : 'a equality -> 'a t equality
  (** [equal equal_a t1 t2] tests if the values [t1] and [t2] are equal using
      the [equal_a] function to compare the values of type ['a]. *)

  val not_equal : 'a equality -> 'a t equality
  (** [not_equal equal_a t1 t2] tests if the values [t1] and [t2] are {i not}
      equal using the [equal_a] function to compare the values of type ['a].

      [not_equal equal_a t1 t2] is a shorthand for [not (equal equal_a t1
      t2)]. *)
end

(** Extension builder module for equatable monomorphic types. *)
module Equal1 : sig

  (** Minimum interface for equatable polymorphic unary types. *)
  module type Base = sig
    type 'a t
    (** Abstract type for equatable values. *)

    val equal : 'a equality -> 'a t equality
    (** [equal equal_a t1 t2] tests if the values [t1] and [t2] are equal using
        the [equal_a] function to compare the values of type ['a]. *)
  end

  (** Interface builder for equatable polymorphic unary types. *)
  module Make (Base : Base) : Equal1 with type 'a t := 'a Base.t
end


(** {2:equal2 Polymorphic Binary Types}

    Equality comparisons for polymorphic binary types, like results or either
    types. *)

(** Extended interface for equatable polymorphic binary types. *)
module type Equal2 = sig
  type ('a, 'b) t
  (** Abstract type for equatable values. *)

  val equal : 'a equality -> 'b equality -> ('a, 'b) t equality
  (** [eq equal_a equal_b t1 t2] tests if the values [t1] and [t2] are equal
      using the [equal_a] and [equal_b] functions to compare the contained
      values of type ['a] and ['b] respectively. *)

  val not_equal : 'a equality -> 'b equality -> ('a, 'b) t equality
  (** [not_eq equal_a equal_b t1 t2] tests if the values [t1] and [t2] are {i
      not} equal using the [equal_a] and [equal_b] functions to compare the
      values of type ['a] and ['b] respectively.

      [not_equal equal_a equal_b t1 t2] is a shorthand for [not (equal
      equal_a equal_b t1 t2)]. *)
end

(** Extension builder module for equatable monomorphic types. *)
module Equal2 : sig

  (** Minimum interface for equatable polymorphic binary types. *)
  module type Base = sig
    type ('a, 'b) t
    (** Abstract type for equatable values. *)

    val equal : 'a equality -> 'b equality -> ('a, 'b) t equality
    (** [eq equal_a equal_b t1 t2] tests if the values [t1] and [t2] are equal
        using the [equal_a] and [equal_b] functions to compare the contained
        values of type ['a] and ['b] respectively. *)
  end

  (** Interface builder for equatable polymorphic binary types. *)
  module Make (Base : Base) : Equal2 with type ('a, 'b) t := ('a, 'b) Base.t
end

(** {2 Default Aliases} *)

module Equal = Equal0
(** Alias for interface builder for equatable monomorphic types. *)

module type Equal = Equal0
(** Alias for extended interface for equatable monomorphic types. *)


(** {1:ordering Ordering}

    This section defines types, interfaces and operations for values that form
    a total order relation.

    An order is a total order if it is (for all [a], [b] and [c]):

    {ul
    {- {i total} and {i antisymmetric}: exactly one of [a < b], [a = b] or [a
    > b] is true;}
    {- {i transitive}, [a < b] and [b < c] implies [a < c]. The same must hold
    for [=] and [>].}}

    User-defined types can implement the {!Ordered}, {!Ordered1} or {!Ordered2}
    interfaces (based on to the arity of the main type) to included a
    specialized comparison functions.

    {2 Example}
{[
open Ordering

module Person = struct
  type t = {
    name : string;
    age : int;
  }

  (* Ordering by age. *)
  module By_age = Ordered.Make(struct
    type nonrec t = t

    let compare t1 t2 =
      Ordered.int t1.age t2.age
  end)
end

let alice = Person.{ name = "Alice"; age = 23 }
let bob   = Person.{ name = "Bob";   age = 28 }
let craig = Person.{ name = "Craig"; age = 43 }

let () =
  assert Person.By_age.(bob > alice);
  assert Person.By_age.(max bob craig = craig);
  assert Person.By_age.(bob |> between ~min:alice ~max:craig)
]} *)

(** {2 Monomorphic Types}

  Ordering comparisons for monomorphic types. *)

(** Extended interface for ordered types. *)
module type Ordered0 = sig
  type t
  (** The type for ordered values. *)

  val compare : t -> t -> order
  (** Returns an {!type:ordering} between two values. *)

  include Equal0 with type t := t

  val less : t -> t -> bool
  (* Test whether one value is {i strictly less than} another. *)

  val greater : t -> t -> bool
  (** Test whether one value is {i strictly greater than} another. *)

  val less_or_equal : t -> t -> bool
  (** Test whether one value is {i non-strictly less than} another. *)

  val greater_or_equal : t -> t -> bool
  (** Test whether one value is {i non-strictly greater than} another. *)

  val ( < ) : t -> t -> bool
  (** Operator version of {!val:less}. *)

  val ( > ) : t -> t -> bool
  (** Operator version of {!val:greater}. *)

  val ( <= ) : t -> t -> bool
  (** Operator version of {!val:less_or_eq}. *)

  val ( >= ) : t -> t -> bool
  (** Operator version of {!val:greater_or_eq}. *)

  val min : t -> t -> t
  (** Take the minimum of two values. If they are considered equal, the first
      argument is chosen. *)

  val max : t -> t -> t
  (** Take the maximum of two values. If they are considered equal, the first
      argument is chosen. *)

  val between : min: t -> max: t -> t -> bool
  (** [between ~min ~max x] tests whether the value [x] is between a minimum
      and a maximum (inclusive).

{[
let f = between ~min:0 ~max:10 in
assert (f 0);
assert (f 3);
assert (not (f 15);
assert (not (f (-5)))
]} *)

  val clamp : min: t -> max: t -> t -> t
  (** [clamp ~min ~max x] clamps the value [x] between a minimum and a maximum.

{[
let f = clamp ~min:0 ~max:10
assert (f (-5) = 0);
assert (f 5 = 5);
assert (f 15 = 10)
]} *)
end

(** Extension builder module for ordered monomorphic types. *)
module Ordered0 : sig
  (** Minimal interface for ordered types. *)
  module type Base = sig
    type t
    (** The type for ordered values. *)

    val compare : t -> t -> order
    (** Returns an {!type:ordering} between two values. *)
  end

  (** Interface builder for ordered types. *)
  module Make (Base : Base) : Ordered0 with type t := Base.t

  (** {2:funcs Order comparison functions} *)

  val unit : unit comparator
  (** Order comparison function for values of type [unit]. *)

  val bool : bool comparator
  (** Order comparison function for values of type [bool]. *)

  val char : char comparator
  (** Order comparison function for values of type [char]. *)

  val int : int comparator
  (** Order comparison function for values of type [int]. *)

  val float : float comparator
  (** Order comparison function for values of type [float]. *)

  val string : string comparator
  (** Order comparison function for values of type [string]. *)

  val list : 'a comparator -> 'a list comparator
  (** Order comparison function for values of type [list]. *)

  val array : 'a comparator -> 'a array comparator
  (** Order comparison function for values of type [array]. *)

  val option : 'a comparator -> 'a option comparator
  (** Order comparison function for values of type [option]. *)

  val result : 'a comparator -> 'b comparator -> ('a, 'b) result comparator
  (** Order comparison function for values of type [result]. *)

  val ref : 'a comparator -> 'a ref comparator
  (** Order comparison function for values of type [ref]. *)

  val pair : 'a comparator -> 'b comparator -> ('a * 'b) comparator
  (** Order comparison function for pairs of type ['a * 'b]. *)
end

(** {2 Polymorphic Unary Types}

    Ordering comparisons for polymorphic unary types. *)

(** Extended interface for ordered types. *)
module type Ordered1 = sig
  type 'a t

  val compare : ('a -> 'a -> order) -> 'a t -> 'a t -> order

  val min : ('a -> 'a -> order) -> 'a t -> 'a t -> 'a t
  val max : ('a -> 'a -> order) -> 'a t -> 'a t -> 'a t
end

(** Extension builder module for ordered polymorphic unary types. *)
module Ordered1 : sig
  (** Minimal interface for ordered types. *)
  module type Base = sig
    type 'a t

    val compare : ('a -> 'a -> order) -> 'a t -> 'a t -> order
  end

  (** Interface builder for ordered types. *)
  module Make (Base: Base) : Ordered1 with type 'a t := 'a Base.t
end


(** {2 Polymorphic Binary Types}

    Ordering comparisons for polymorphic binary types. *)

(** Extended interface for ordered types. *)
module type Ordered2 = sig
  type ('a, 'b) t

  val compare :
    ('a -> 'a -> order) ->
    ('b -> 'b -> order) ->
    ('a, 'b) t -> ('a, 'b) t -> order

  val min :
    ('a -> 'a -> order) ->
    ('b -> 'b -> order) ->
    ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

  val max :
    ('a -> 'a -> order) ->
    ('b -> 'b -> order) ->
    ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
end

(** Extension builder module for ordered polymorphic binary types. *)
module Ordered2 : sig

  (** Minimal interface for ordered types. *)
  module type Base = sig
    type ('a, 'b) t

    val compare :
      ('a -> 'a -> order) ->
      ('b -> 'b -> order) ->
      ('a, 'b) t -> ('a, 'b) t -> order
  end

  (** Interface builder for ordered types. *)
  module Make (Base : Base) : Ordered2 with type ('a, 'b) t := ('a, 'b) Base.t
end

(** {2 Default Aliases} *)

module Ordered = Ordered0
(** Alias for interface builder for ordered monomorphic types. *)

module type Ordered = Ordered0
(** Alias for extended interface for ordered monomorphic types. *)


(** {1:public Public}

    Public comparison operations included when the top-level [Ordering] module
    is open.

    {b Note:} By default the standard comparison functions are specialised to
    integers. To compare other types consider using the combinators provided
    in the [Equal] and [Ordered] modules. Alternatively you may open the
    {{: #generic} [Generic]} module. *)

val ( = ) : int -> int -> bool
(** Public alias for {!val:Equal0.(=)} specialised to integers. *)

val ( <> ) : int -> int -> bool
(** Public alias for {!val:Equal0.(<>)} specialised to integers. *)

val ( < ) : int -> int -> bool
(** Public alias for {!val:Ordered0.(<)} specialised to integers. *)

val ( > ) : int -> int -> bool
(** Public alias for {!val:Ordered0.(>)} specialised to integers. *)

val ( <= ) : int -> int -> bool
(** Public alias for {!val:Ordered0.(<=)} specialised to integers. *)

val ( >= ) : int -> int -> bool
(** Public alias for {!val:Ordered0.(>=)} specialised to integers. *)

val min : int -> int -> int
(** Public alias for {!val:Ordered0.min} specialised to integers. *)

val max : int -> int -> int
(** Public alias for {!val:Ordered0.max} specialised to integers. *)


(** {2:physical_equality Physical Equality} *)

val is : 'a -> 'a -> bool
(** [is a b] tests for physical equality of [a] and [b].

    On mutable types such as references, arrays, byte sequences, records with
    mutable fields and objects with mutable instance variables, [is a b] is
    true if and only if physical modification of [a] also affects [b].
    On non-mutable types, the behavior of [is] is implementation-dependent;
    however, it is guaranteed that [is a b] implies [a = b].

    To check if two values are physically distinct use [not (is a b)]. *)

val (==) : 'a -> 'a -> bool
[@@ocaml.deprecated "Please use \"is\" instead of \"==\"."]
(** Using the [==] is discouraged because of its visual similarity  with [=]
    and different semantics. The {{: #val-is} [is]} operator should be used instead.

    {b Note:} This API spec is included to raise a deprecation warning during
    compilation. *)


(** {1:generic Generic}

    Polymorphic "magic" functions for structural comparison.

{[
open Ordering.Generic

let () =
  assert ('A' < 'Z');
  assert (max "abc" "xyz" = "xyz")
]}
    {b Warning:} The polymorphic functions in {!Generic} are provided for
    convenience and should not be used in performance-sensitive code. For
    example, instead of using {!val:Generic.min} prefer the monomorphic
    {!val:min} if you are workign with integer values.  Custom data types can
    implement their own specialized {{: #equality} equality} and {{: #ordering}
    ordering} intefaces. *)

module Generic : sig

  (** {1:generic_equality Generic Equality} *)

  val ( = ) : 'a -> 'a -> bool
  (** [a = b] tests for structural equality of [a] and [b]. Mutable
      structures ({i e.g.} references and arrays) are equal if and only if
      their current contents are structurally equal, even if the two mutable
      objects are not the same physical object (as tested with {!val:is}).

      @raise Invalid_argument if function values are compared for equality.

      {b Warning:} Equality between cyclic data structures may not terminate.

      {b See also: } {{: #val-equal} [equal]}, {{: #val-is} [is]} *)

  val ( <> ) : 'a -> 'a -> bool
  (** [a <> b] is [not (a = b)], {i i.e.}, the negation of {!(=)}. *)

  val equal : 'a -> 'a -> bool
  (** [equal a b] is equivalent to [a == b]. *)


  (** {1:generic_ordering Generic Ordering} *)

  val compare : 'a -> 'a -> order
  (** [compare a b] returns [0] if [a] is equal to [b], [-1] if [a] is less than
      [b], and [1] if [a] is greater than [b].
      The ordering implemented by [compare] is compatible with the comparison
      predicates [==], [<] and [>] defined above, with one difference on the
      treatment of the float value {!nan}. Namely, the comparison predicates
      treat [nan] as different from any other float value, including itself;
      while [compare] treats [nan] as equal to itself and less than any other
      float value. This treatment of [nan] ensures that [compare] defines a
      total ordering relation.
      @raise Invalid_argument if function values are compared for equality.
      {b Warning:} Equality between cyclic data structures may not terminate. *)

  val ( <  ) : 'a -> 'a -> bool
  val ( >  ) : 'a -> 'a -> bool
  val ( <= ) : 'a -> 'a -> bool
  val ( >= ) : 'a -> 'a -> bool
  (** Structural ordering functions.
      These functions coincide with the usual orderings over integers,
      characters, strings, byte sequences and floating-point numbers, and extend
      them to a total ordering over all types.  The ordering is compatible with
      {!(==)}. As in the case of {!(==)}, mutable structures are compared by
      contents.

      @raise Invalid_argument if function values are compared for equality.

      {b Warning:} Equality between cyclic data structures may not terminate. *)

  val min : 'a -> 'a -> 'a
  (** [min a b] returns the smaller of the two arguments.
      The result is unspecified if one of the arguments contains
      the float value [nan].

      @raise Invalid_argument if function values are compared for equality.

      {b Warning:} Equality between cyclic data structures may not terminate.
{[
assert (min 2 5 = 2);
assert (min [1; 2; 3] [2; 3; 4] = [1; 2; 3])
]} *)

  val max : 'a -> 'a -> 'a
  (** [max a b] returns the greater of the two arguments.
      The result is unspecified if one of the arguments contains
      the float value [nan].

      @raise Invalid_argument if function values are compared for equality.

      {b Warning:} Equality between cyclic data structures may not terminate.
{[
assert (max 2 5 = 5);
assert (max [1; 2; 3] [2; 3; 4] = [2; 3; 4])
]} *)
end

