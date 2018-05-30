
open Order

(** Polymorphic {{: https://blog.janestreet.com/the-perils-of-polymorphic-compare/} "magic"}
    functions for structural comparison.

    {[
    open Order.Magic

    let () =
    assert ('A' < 'Z');
    assert (max "abc" "xyz" = "xyz")
    ]}

    {b Warning:} The polymorphic functions in {!Magic} are provided for
    convenience and must be used with care: they are less efficient than the
    specialized versions and may cause runtime errors if functions, or other
    non-comparable values, are compared. Instead of using {!val:Magic.min}
    prefer the monomorphic {!val:min} if you are working with integer values.
    Custom data types can implement their own specialized {{: #equality}
    equality} and {{: #ordering} ordering} interfaces. *)


(** {2:generic_equality Polymorphic Equality} *)

val ( = ) : 'a -> 'a -> bool
(** [a = b] tests for structural equality of [a] and [b]. Mutable
    structures ({i e.g.} references and arrays) are equal if and only if
    their current contents are structurally equal, even if the two mutable
    objects are not the same physical object (as tested with {!val:is}).

    @raise Invalid_argument if function values are compared for equality.

    {b Warning:} Equality between cyclic data structures may not terminate.

    {b See also:} {{: #val-equal} [equal]}, {{: #val-is} [is]} *)

val ( <> ) : 'a -> 'a -> bool
(** [a <> b] is [not (a = b)], {i i.e.}, the negation of {!(=)}. *)

val equal : 'a -> 'a -> bool
(** [equal a b] is equivalent to [a == b]. *)


(** {2:generic_ordering Polymorphic Ordering} *)

val compare : 'a comparator
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

val comparing  : ('a -> 'b) -> 'a comparator
(** Applies a projection function to obtain a comparable value of type ['b].

    [comparing f] is defined as [Comparator.by f Magic.compare].

    {[
    type person = { name : string; age : int }

    let by_age_descending : person comparator =
    Comparator.descending (comparing (fun p -> p.age))

    let () =
      [{ name = "Alice"; age = 24 };
       { name = "Bob";   age = 19 };
       { name = "Craig"; age = 45 }]
      |> List.sort (Comparator.to_integral by_age_descending)
      |> List.iter (fun p -> print_endline p.name)
    ]} *)

val ( <  ) : 'a -> 'a -> bool
val ( >  ) : 'a -> 'a -> bool
val ( <= ) : 'a -> 'a -> bool
val ( >= ) : 'a -> 'a -> bool
(** {b Structural ordering functions.}

    These functions coincide with the usual orderings over integers,
    characters, strings, byte sequences and floating-point numbers, and extend
    them to a total ordering over all types. The ordering is compatible with
    {!(=)}. As in the case of {!(=)}, mutable structures are compared by
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
