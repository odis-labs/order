
open Order


module type Person = sig
  type t

  include Equal.Extension with type t := t
end


module Person = struct
  type t = {
    name : string;
    age : int;
  }

  (* Equality by name. *)
  include Equal.Extend(struct
    type nonrec t = t

    let equal t1 t2 =
      Equality.string t1.name t2.name
  end)

  (* Ordering by age. *)
  module By_age = Ordered.Extend(struct
    type nonrec t = t

    let compare t1 t2 =
      Comparator.int t1.age t2.age
  end)
end


let alice = Person.{ name = "Alicea"; age = 23 }
let bob   = Person.{ name = "Bob";    age = 28 }
let craig = Person.{ name = "Craig";  age = 43 }


let () =
  assert Person.By_age.(bob > alice);
  assert Person.By_age.(max bob craig == craig);
  assert Person.By_age.(bob |> between ~min:alice ~max:craig)

