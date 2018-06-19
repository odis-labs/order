(* Copyright (c) 2018 Rizo I <rizo@odis.io>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

(** Functionality for comparison and ordering of OCaml values. *)


(** {1:ordering Ordering}


    Ordering values are produced by {i comparators} - functions that compare
    two values. Comparators for common data types can be found
     in the {{: Comparator/index.html} [Comparator]} module.

    The complementary {!Ordering} module includes operations on [ordering]
    values.

    {4 Examples}

{[
open Order

(* 2 < 2 *)
let compare = Comparator.int in
assert (is Less (compare 1 2));

(* [1; 2] = [1; 2] *)
let compare = Comparator.(list int) in
assert (is Equal (compare [1; 2]));

(* (42, "abc") > (42, "def") *)
let compare = Comparator.(pair int string) in
assert (is Greater (compare (42, "abc") (42, "def")));
]} *)

type ordering =
  [ `Less    (** {e a < b}, {e a} is less than {e b}. *)
  | `Equal   (** {e a = b}, {e a} is equal to {e b}. *)
  | `Greater (** {e a > b}, {e a} is greater than {e b}. *)
  ]
(** Defines the relative ordering of two values. *)

(** Module for the {!type:ordering} type. *)
module Ordering : sig

  (** {3:ordering_definition Definition} *)

  (** Defines the relative order of two values. *)
  type t =
    [ `Less    (** {e a < b}, {e a} is less than {e b}. *)
    | `Equal   (** {e a = b}, {e a} is equal to {e b}. *)
    | `Greater (** {e a > b}, {e a} is greater than {e b}. *)
    ]


  (** {3:ordering_basics Basics} *)

  val invert : t -> t
  (** Inverts a given ordering. *)


  (** {3:ordering_pretty_printing Pretty-printing} *)

  val inspect : Format.formatter -> t -> unit
  (** Pretty-printer for values of type [t] in debugging context. *)

  val display : Format.formatter -> t -> unit
  (** Pretty-printer for values of type [t] suitable for user-facing output.
      The constructors [Less], [Equal] and [Greater] are rendered as
      mathematical symbols [<], [=] and [>] respectively. *)


  (** {3:equality_and_comparison Equality and Comparison} *)

  val equal : t -> t -> bool
  (** Checks if two ordering values are equal. This makes [t] an instance of
      [Equal0]. *)

  val compare : t -> t -> t
  (** Relative ordering of two orderings. This makes [t] an instance of
      [Ordered0]. *)


  (** {3:ordering_conversions Conversions} *)

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
end


(** {2:equality Equality}

    Equality comparisons for monomorphic and polymorphic types.

    This module defines interfaces and operations for equality comparison
    between values. Equality is an {{:
    https://en.wikipedia.org/wiki/Equivalence_relation} equivalence relation},
    which means that it must be: {i reflexive}, {i symmetric} and {i
    transitive}.

    User-defined types can implement the {!modtype:Equal0}, {!modtype:Equal1} or
    {!modtype:Equal2} interfaces (according to the arity of the main type) to
    include specialized equality comparison functions.

    {b Note:} The extended version of [Equal] for polymorphic types does not
    include infix functions since they are only useful with two arguments and
    [Equal1] and [Equal2] would require extra arguments for each type
    parameter.

    {4 Examples}

{[
open Order

module Book = struct
  type t = {
    title : string;
    author : string;
    isbn : int;
  }

  (* Equality by ISBN. *)
  include Equal0.Extend(struct
    type nonrec t = t

    let equal t1 t2 =
      Equality.string t1.isbn t2.isbn
  end)
end
]} *)


(** {3:equality_functions Equality functions} *)

type 'a equality = 'a -> 'a -> bool
(** The type of equality testing functions. *)

(** Module for the {!type:ordering} function type. Includes implementations for
    common data types. *)
module Equality : sig
  type 'a t = 'a equality
  (** The type of equality testing functions. *)

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

  val result : 'a equality -> 'b equality -> ('a, 'b) Result.result equality
  (** Equality testing function for values of type [result]. *)

  val ref : 'a equality -> 'a ref equality
  (** Equality testing function for values of type [ref]. *)

  val pair : 'a equality -> 'b equality -> ('a * 'b) equality
  (** Equality testing function for pairs of type ['a * 'b]. *)

  val triple : 'a equality -> 'b equality -> 'c equality
    -> ('a * 'b * 'c) equality
  (** Equality testing function for triples of type ['a * 'b * 'c]. *)

  val by : ('a -> 'b) -> 'b equality -> 'a equality
  (** [by f equal] is a comparator that applies the projection function [f]
      before testing the equality with [equal].

      This function can be used to, for example, extract fields or apply
      transformations on values before comparing them. The provided [equal]
      function will be applied to the values produced by [f].

    {4 Examples}

{[
let l1 = [0; 1; 2; 3] in
let l2 = [4; 5; 6; 7] in
let equal_by_length = Equality.(by List.length int) in
assert (equal_by_length l1 l2))
]}*)
end


(** {3:equal Monomorphic Types}

    Equality comparisons for monomorphic types, like integers and strings. *)

(** Base interface for equatable monomorphic types. *)
module type Equal0 = sig
  type t
  (** Abstract type for equatable values. *)

  val equal : t equality
  (** [equal t1 t2] tests if the values [t1] and [t2] are equal, and is used by
      the [=] and [<>] operators. *)
end

(** Extension builder module for equatable monomorphic types. *)
module Equal0 : sig
  (** Extended interface for equatable monomorphic types. *)
  module type Extension = sig
    include Equal0

    val not_equal : t equality
    (** [not_equal t1 t2] tests if the values [t1] and [t2] are {i not} equal,
        and is used by [<>].

        [not_eq t1 t2] is a shorthand for [not (equal t1 t2)]. *)

    val ( = ) : t equality
    (** Operator alias for {!equal}. *)

    val ( <> ) : t equality
    (** Operator alias for {!not_equal}. *)
  end

  (** Extends the base definition for equatable monomorphic types. *)
  module Extend (Base : Equal0) : Extension with type t := Base.t
end


(** {3:equal1 Polymorphic Unary Types}

    Equality comparisons for polymorphic unary types, like lists and option
    values. *)

(** Base interface for equatable polymorphic unary types. *)
module type Equal1 = sig
  type 'a t
  (** Abstract type for equatable values. *)

  val equal : 'a equality -> 'a t equality
  (** [equal equal_a t1 t2] tests if the values [t1] and [t2] are equal using
      the [equal_a] function to compare the values of type ['a]. *)
end

(** Extension builder module for equatable monomorphic types. *)
module Equal1 : sig
  (** Extended interface for equatable polymorphic unary types. *)
  module type Extension = sig
    type 'a t
    (** Abstract type for equatable values. *)

    val equal : 'a equality -> 'a t equality
    (** [equal equal_a t1 t2] tests if the values [t1] and [t2] are equal using
        the [equal_a] function to compare the values of type ['a]. *)

    val not_equal : 'a equality -> 'a t equality
    (** [not_equal equal_a t1 t2] tests if the values [t1] and [t2] are {i not}
        equal using the [equal_a] function to compare the values of type ['a].

        [not_equal equal_a t1 t2] is a shorthand for [not (equal equal_a t1 t2)]. *)
  end

  (** Interface builder for equatable polymorphic unary types. *)
  module Extend (Base : Equal1) : Extension with type 'a t := 'a Base.t
end


(** {3:equal2 Polymorphic Binary Types}

    Equality comparisons for polymorphic binary types, like results or either
    types. *)

(** Base interface for equatable polymorphic binary types. *)
module type Equal2 = sig
  type ('a, 'b) t
  (** Abstract type for equatable values. *)

  val equal : 'a equality -> 'b equality -> ('a, 'b) t equality
  (** [eq equal_a equal_b t1 t2] tests if the values [t1] and [t2] are equal
      using the [equal_a] and [equal_b] functions to compare the contained
      values of type ['a] and ['b] respectively. *)
end

(** Extension builder module for equatable monomorphic types. *)
module Equal2 : sig
  (** Extended interface for equatable polymorphic binary types. *)
  module type Extension = sig
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

        [not_equal equal_a equal_b t1 t2] is a shorthand for
        [not (equal equal_a equal_b t1 t2)]. *)
  end

  (** Interface builder for equatable polymorphic binary types. *)
  module Extend (Base : Equal2) : Extension with type ('a, 'b) t := ('a, 'b) Base.t
end

(** {3 Default Aliases} *)

module type Equal = Equal0
(** Alias for extended interface for equatable monomorphic types. *)

module Equal = Equal0
(** Alias for interface builder for equatable monomorphic types. *)


(** {1:comparisons Comparisons}

    This section defines types, interfaces and operations for values that form
    a total order relation.

    An order is a total order if it is (for all [a], [b] and [c]):

    {ul
    {- {i total} and {i antisymmetric}: exactly one of [a < b], [a = b] or
    [a > b] is true;}
    {- {i transitive}, [a < b] and [b < c] implies [a < c]. The same must hold
    for [=] and [>].}}

    User-defined types can implement the {!Ordered0}, {!Ordered1} or {!Ordered2}
    interfaces (based on to the arity of the main type) to included a
    specialized comparison functions.

    {4 Example}
{[
open Order

module Person = struct
  type t = {
    name : string;
    age : int;
  }

  (* Ordering by age. *)
  module By_age = Ordered0.Extend(struct
    type nonrec t = t

    let compare t1 t2 =
      Comparator.int t1.age t2.age
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


(** {3:comparison_functions Comparison functions} *)

type 'a comparator = 'a -> 'a -> ordering
(** The type of order comparison functions.

    Multiple comparators can be composed to handle complex polymorphic types.
    The {{: Comparator/index.html} [Comparator]} module defines comparison
    functions for common monomorphic and polymorphic types and can be used for
    that.

    {4 Examples}

    An arrays of optional pairs with ints and strings can be
    compared with:

{[
let my_compare =
  Comparator.(array (option (pair int string)))

let () =
  let a1 = [|Some (42, "foo"); Some (43, "bar"); Some (100, "a")|] in
  let a2 = [|Some (42, "foo"); Some (10, "baz"); None|] in

  (* a1 is greater than a2 because the nested 43 is greater than 10. *)
  assert (is Greater (my_compare a1 a2));

  (* The composed comparator can also be used inline: *)
  assert (is Less Comparator.(array (option (pair int string)) a2 a1))
]} *)

(** Module for the {!type:comparator} function type. Includes implementations for
    common data types. *)
module Comparator : sig
  type 'a t = 'a comparator
  (** The type of order comparison functions. *)

  val unit : unit comparator
  (** Order comparison function for values of type [unit]. *)

  val bool : bool comparator
  (** Order comparison function for values of type [bool]. *)

  val char : char comparator
  (** Order comparison function for values of type [char]. *)

  val int : int comparator
  (** Order comparison function for values of type [int]. *)

  val int32 : int32 comparator
  (** Order comparison function for values of type [int32]. *)

  val int64 : int64 comparator
  (** Order comparison function for values of type [int64]. *)

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

  val result : 'a comparator -> 'b comparator -> ('a, 'b) Result.result comparator
  (** Order comparison function for values of type [result]. *)

  val ref : 'a comparator -> 'a ref comparator
  (** Order comparison function for values of type [ref]. *)

  val pair : 'a comparator -> 'b comparator -> ('a * 'b) comparator
  (** Order comparison function for pairs of type ['a * 'b]. *)

  val triple : 'a comparator -> 'b comparator -> 'c comparator
    -> ('a * 'b * 'c) comparator
  (** Order comparison function for triples of type ['a * 'b * 'c]. *)

  val invert : 'a comparator -> 'a comparator
  (** Inverts a given comparator function.

      {b See also:} {{: #val-descending} [descending]}, {{: #val-ascending}
      [ascending]}

      {4 Examples}

{[
let result =
  [1; 2; 3; 4; 5]
  |> List.sort (Comparator.(invert int |> to_integral)) in
assert (Equality.(list int) result [5; 4; 3; 2; 1])
]} *)

  val descending : 'a comparator -> 'a comparator
  val ascending : 'a comparator -> 'a comparator
  (** Ascending and descending comparators are intended to be mnemonic when
      used to sort elements: [List.sort ~by:Comparator.(descending int)].

      {ul
      {- [descending] is an alias for {!val:invert}.}
      {- [ascending] is {i identity}.}} *)

  val by : ('a -> 'b) -> 'b comparator -> 'a comparator
  (** [by f comparator] is a comparator that applies the projection function [f]
      before applying [comparator].

      This function can be used to, for example, extract fields or apply
      transformations on values before comparing them. The provided
      [comparator] function will be applied to the values produced by [f].

      {4 Examples}

{[
let l1 = [1; 2; 3; 4; 5] in
let l2 = [1; 2; 3] in
let compare_by_length = by List.length Comparator.int in
assert (Ordering.is_greater (compare_by_length l1 l2))
]}*)

  val lexical : 'a comparator list -> 'a comparator
  (** Performs a {{: https://en.wikipedia.org/wiki/Lexicographical_order}
      lexical} comparison using a list of comparators.

      [(lexical comparators) a1 a2] compares the values [a1] and [a2] by
      sequentially applying functions in the [comparators] list. The comparison
      terminates as soon as the first non-[Equal] ordering is produced by a
      comparator. If the provided [comparators] list is empty the values are
      considered to be equal.

      Combined with "{!val:by}" this function can be used to define comparator
      functions for records, tuples or other complex data types.

      {4 Examples}

{[
type player = {
  name: string;
  score: int;
  health: float;
}

(* Field getters. *)
let name t = t.name
let score t = t.score
let health t = t.health

(* Compare by score, health, name. *)
let compare_players =
  let open Comparator in
  lexical [
    by score int;
    by health float;
    by name string;
  ]
]} *)

  val to_integral : 'a comparator -> 'a -> 'a -> int
  (** [to_integral compare] produces a comparator-like function that denotes
      ordering by integers.

      {b Note: } This function produces comparators compatible with OCaml's
      standard library.

      The following mapping is used:

      {ul
      {li [Less] is [-1]}
      {li [Equal] is [0]}
      {li [Greater] is [1]}} *)
end


(** {3 Monomorphic Types}

  Ordering comparisons for monomorphic types. *)

(** Base interface for ordered types. *)
module type Ordered0 = sig
  type t
  (** The type for ordered values. *)

  val compare : t comparator
  (** Returns an {!type:ordering} between two values. *)
end

(** Extension builder module for ordered monomorphic types. *)
module Ordered0 : sig
  (** Extended interface for ordered types. *)
  module type Extension = sig
    type t
    (** The type for ordered values. *)

    val compare : t comparator
    (** Returns an {!type:ordering} between two values. *)

    include Equal0.Extension with type t := t

    val ( < ) : t -> t -> bool
    (** Test whether one value is {i strictly less than} another. *)

    val ( > ) : t -> t -> bool
    (** Test whether one value is {i strictly greater than} another. *)

    val ( <= ) : t -> t -> bool
    (** Test whether one value is {i non-strictly less than} another. *)

    val ( >= ) : t -> t -> bool
    (** Test whether one value is {i non-strictly greater than} another. *)

    val min : t -> t -> t
    (** Take the minimum of two values. If they are considered equal, the first
        argument is chosen. *)

    val max : t -> t -> t
    (** Take the maximum of two values. If they are considered equal, the first
        argument is chosen. *)

    val comparing  : ('a -> t) -> 'a comparator
    (** Applies a projection function to obtain a comparable value of type [t].

        [comparing f] is defined as [Comparator.by f compare].

        {4 Examples}

{[
type person = { name : string; age : int }

let by_age_descending : person comparator =
  Comparator.descending (Ordered_int.comparing (fun p -> p.age))

let () =
  [{ name = "Alice"; age = 24 };
   { name = "Bob";   age = 19 };
   { name = "Craig"; age = 45 }]
  |> List.sort (Comparator.to_integral by_age_descending)
  |> List.iter (fun p -> print_endline p.name)
]} *)

    val between : min: t -> max: t -> t -> bool
    (** [between ~min ~max x] tests whether the value [x] is between a minimum
        and a maximum (inclusive on both ends).

        {4 Examples}

{[
let f = between ~min:0 ~max:10 in
assert (f 0);
assert (f 3);
assert (not (f 15);
assert (not (f (-5)))
]} *)

    val clamp : min: t -> max: t -> t -> t
    (** [clamp ~min ~max x] clamps the value [x] between a minimum and a maximum.

        {4 Examples}

{[
let f = clamp ~min:0 ~max:10
assert (f (-5) = 0);
assert (f 5 = 5);
assert (f 15 = 10)
]} *)
  end

  (** Interface builder for ordered types. *)
  module Extend (Base : Ordered0) : Extension with type t := Base.t
end

(** {3 Polymorphic Unary Types}

    Ordering comparisons for polymorphic unary types. *)

(** Base interface for ordered types. *)
module type Ordered1 = sig
  type 'a t

  val compare : 'a comparator -> 'a t comparator
end

(** Extension builder module for ordered polymorphic unary types. *)
module Ordered1 : sig
  (** Extended interface for ordered types. *)
  module type Extension = sig
    type 'a t

    val compare : 'a comparator -> 'a t comparator

    val min : 'a comparator -> 'a t -> 'a t -> 'a t
    val max : 'a comparator -> 'a t -> 'a t -> 'a t
  end

  (** Interface builder for ordered types. *)
  module Extend (Base: Ordered1) : Extension with type 'a t := 'a Base.t
end


(** {3 Polymorphic Binary Types}

    Ordering comparisons for polymorphic binary types. *)

(** Base interface for ordered types. *)
module type Ordered2 = sig
  type ('a, 'b) t

  val compare : 'a comparator -> 'b comparator -> ('a, 'b) t comparator
end

(** Extension builder module for ordered polymorphic binary types. *)
module Ordered2 : sig
  (** Extended interface for ordered types. *)
  module type Extension = sig
    type ('a, 'b) t

    val compare : 'a comparator -> 'b comparator -> ('a, 'b) t comparator

    val min : 'a comparator -> 'b comparator -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    val max : 'a comparator -> 'b comparator -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  end

  (** Interface builder for ordered types. *)
  module Extend (Base : Ordered2) : Extension with type ('a, 'b) t := ('a, 'b) Base.t
end

(** {3 Default Aliases} *)

module type Ordered = Ordered0
(** Alias for extended interface for ordered monomorphic types. *)

module Ordered = Ordered0
(** Alias for interface builder for ordered monomorphic types. *)


(** {2:physical_equality Physical Equality} *)

val is : 'a -> 'a -> bool
(** [is a b] tests for physical equality of [a] and [b].

    On mutable types such as references, arrays, byte sequences, records with
    mutable fields and objects with mutable instance variables, [is a b] is
    true if and only if physical modification of [a] also affects [b].
    On non-mutable types, the behavior of [is] is implementation-dependent;
    however, it is guaranteed that [is a b] implies [a = b].

    To check if two values are physically distinct use [not (is a b)]. *)

val (==) : [`Deprecated of 'a -> 'a -> bool ]
[@@ocaml.deprecated "Please use \"is\" instead of \"==\"."]
(** Using the [==] is discouraged because of its visual similarity  with [=]
    and different semantics. The {{: #val-is} [is]} operator should be used instead.

    {b Note:} This operator is included to raise a deprecation warning during
    compilation. *)


(** {3:monomorphic_comparison Monomorphic Comparison}

    Public comparison operations included when the top-level [Order] module
    is open.

    By default the standard comparison functions are specialized to integers.
    To compare other types consider using the combinators provided in the
    [Equality] and [Comparator] modules. Alternatively you may consider opening
    the {{: #magic} [Magic]} module. *)

val compare : int -> int -> ordering
(** Produces the ordering between two integers. *)

val ( = ) : int -> int -> bool
(** [a = b] tests if the integers [a] and [b] are equal. *)

val ( <> ) : int -> int -> bool
(** [a <> b] tests if the integers [a] and [b] are {e not} equal. *)

val ( < ) : int -> int -> bool
(** [a < b] tests if the integer [a] is less than [b]. *)

val ( > ) : int -> int -> bool
(** [a > b] tests if the integer [a] is greater than [b]. *)

val ( <= ) : int -> int -> bool
(** [a <= b] tests if the integer [a] is less than or equal to [b]. *)

val ( >= ) : int -> int -> bool
(** [a >= b] tests if the integer [a] is greater than or equal to [b]. *)

val min : int -> int -> int
(** Takes the minimum of two integers. *)

val max : int -> int -> int
(** Takes the maximum of two integers. *)


(** {1:comparison_with_stdlib Comparison with Stdlib}

    There are three main differences between Order and the OCaml standard library:

    {ul
    {- The comparison operations implemented in the OCaml standard library are
    polymorphic, but in Order they are monomorphic (specialized to integers), by default. }
    {- In the OCaml standard library the base function used in all comparisons,
    [compare], produces integer-based ordering values, while Order uses its own
    {{: #type-ordering} [ordering]} type. }
    {- Physical equality can be tested with {{: #val-is} [is]}, and the
    standard library's {{: #val-(==)} [(==)]} operator is deprecated. }}

    The following sections explain in detail the listed differences and the
    rationale behind them.


    {3 Monomorphic Comparison}

    The reason why polymorphic operations are not included in this library is
    because they are less efficient than the specialized versions and may cause
    runtime errors if functions, or other non-comparable values (like
    definitions in C bindings), are compared.

{[
let hello name =
  Printf.printf "Hello, %s!" name
in
  Pervasives.(hello = hello)
(* Exception: Invalid_argument "compare: functional value". *)
]}

    In this example it is clear that a function is being compared, but consider
    the case where a nested generic data type is being compared which happens
    to have a function value inside (like a comparator). The comparison will
    unexpectedly break.

    Using monomorphic comparison eliminates the possibility of accidentally
    comparing functions and encourages the creation of custom comparators for
    user-defined types.


    {3 Ordering Type}

    The integer-based relative ordering is a historical artifact inherited from
    the low-level languages like C. As any convention not enforced by the
    type-system it can lead to errors, like accidentally using the ordering in
    numerical expressions or forgetting to handle a case. In ML languages
    ordering can be defined as a union type, providing clear semantics.


    {3 Equality Operators}

    Confusing for new-comers.

    Inconsistent with type definition semantics:

{[
type type1 = type2 = A | B
let var1 = var2 = 1
]} *)

