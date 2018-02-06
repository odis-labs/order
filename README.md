# Compare (Unreleased)

Functionality for comparison and ordering of OCaml values.

This library defines interfaces for equality and ordering comparisons.
Types can implement `Equal` or `Ordered` interfaces to include specialized
comparison operations.

The following features are provided:

- New `ordering` type to replace integer-based ordering.
- Extended `Ordered` and `Equal` interfaces for custom types.
- Public comparison operations specialized to integers.
- Convenience `Magic` module for polymorphic comparisons.
- Equality and ordering functions for common data types.
- New physical equality operator `is` and deprecated `==`.

Consult the [online documentation](http://odis.io/compare/Compare) for more details.

## Examples

In this example we want to sort a list of accounts by the total sum of transactions and than by the holder name. A custom comparator function is defined used with `List.sort`.

```ocaml
open Compare

(* Function composition *)
let (<<) f g = fun x -> f (g x)

type account = {
  holder : string;
  transactions : float list;
}

(* Field getters. *)
let holder account = account.holder
let transactions account = account.transactions

(* Sum all transactions *)
let sum = List.fold_left (+.) 0.0

(* Compare accounts by wealth and then by holder name. *)
let by_wealth : account comparator =
  let open Comparator in
  lexical [by (descending (sum << transactions)) float;
           by holder string]

let holders_by_wealth =
  [{ holder = "Alice"; transactions = [100.00; -25.00; -2.99; 30.00] };
   { holder = "Bob";   transactions = [999.02; -233.00; -400.00; -300.00] };
   { holder = "Craig"; transactions = [129.8; -0.70; 110.03; 350.00] };
   { holder = "David"; transactions = [234.0; 4534.00; -42.00; 900.00; -5000.00] }]
  |> List.sort (Comparator.to_integral by_wealth) (* Use integer-based comparator *)
  |> List.map holder

let () =
  (* Use specialized string list equality function. *)
  let equal = Equality.(list string) in
  assert (equal holders_by_wealth ["David"; "Craig"; "Alice"; "Bob"])
```

---

Custom data types can implement the base `Equal` or `Ordered` interfaces and get specialised comparison functions for free.

```ocaml
module Person = struct
  type t = {
    name : string;
    age : int;
  }
  
  let say_hello t =
    printf "Hello, %s!" t.name

  (* Base definition for ordering by age. *)
  module By_age = Ordered.Make(struct
    type nonrec t = t

    let compare t1 t2 =
      Comparator.int t1.age t2.age
  end)
end

(* This will generate the following extended module:

module Person : sig
  type t = {
    name : string;
    age : int;
  }
  
  val say_hello : t -> unit

  module By_age : sig 
    val compare : t comparator        
    val equal : t equality
    val not_equal : t equality
    val ( = ) : t equality
    val ( <> ) : t equality
    val ( < ) : t -> t -> bool        
    val ( > ) : t -> t -> bool
    val ( <= ) : t -> t -> bool
    val ( >= ) : t -> t -> bool
    val min : t -> t -> t
    val max : t -> t -> t
    val comparing : ('a -> t) -> 'a comparator
    val between : min:t -> max:t -> t -> bool
    val clamp : min:t -> max:t -> t -> t
  end
end

The generated specialized operations can now be used: *)

let alice = Person.{ name = "Alice"; age = 23 }
let bob   = Person.{ name = "Bob";   age = 28 }
let craig = Person.{ name = "Craig"; age = 43 }

let () =
  let open Person in
  assert By_age.(bob > alice);
  assert By_age.(max bob craig = craig);
  assert By_age.(bob |> between ~min:alice ~max:craig)
```


## Installation

The current stable version of the package can be installed with [OPAM](http://opam.ocaml.org):

```
opam install compare
```

The development version can be installed by pinning the `master` branch:

```
opam pin add compare https://github.com/rizo/compare.git
```

## Documentation

The documentation is generated from the source interfaces and can be consulted both online and offline:

- [Online API Reference](http://odis.io/compare/Compare)
- Offline API Reference: `odig doc compare`


## License

This library is distributed under the ISC license. See The `LICENSE` for more details.

