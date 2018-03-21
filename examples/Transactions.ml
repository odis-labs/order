open Order

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
  invert (lexical [by (sum << transactions) float;
                   by holder string])

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

