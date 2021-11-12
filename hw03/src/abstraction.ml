(*

FPSE Assignment 3

Name                  : Kyle Wu
List of Collaborators : Shaunak Shah

Please make a good faith effort at listing people you discussed any 
problems with here, as per the course academic integrity policy.  

*)

(*
  One thing which is ubiquitous in many
  object-oriented languages is the abilty to program against
  an interface, and not an underlying class or type,
  enabling dependency injection, easier unit testing,
  and encapsulation of implementation details.

  We've seen in OCaml how to use polymorphic functions
  (those involving types like 'a) to operate on any kind of
  data, but this lets us know _nothing_ about the object.

  We've also seen how to hide parts of our implementation
  by not including certain details about types in our mli files
  or module signatures, and not including functions which are
  only meant to be called by some more limited exposed API.

  But how can we write functions which operate on types which
  have a specific interface, rather than "any type"? Imperative
  languages would often use traits or interfaces for this, or simply
  dynamic dispatch and class hierarchies. What's idiomatic in OCaml?
*)

open Core

(*
  Turns out, module types and functors do the trick of specifying
  exactly what can be relied upon, and naming our dependencies.

  Better than that, we aren't restricted to specifying only one
  "class" and its interfaces, we can ask for any number and kind
  of functions operating over multiple types, and produce an
  entire module which makes use of those behaviors.

  Using functors in this way decouples our code from the implementation
  of functionality it depends on, and from the representation of objects
  which it uses, allowing a very powerful kind of dependency injection.

  Let's start with a simple case and just consider
  a requirement that some type can be compared for equality:
  (you've seen this type before!)
*)
module type Eq = sig
  (*
    Think of this as the constrained type.
    Because we won't know what it actually is,
    we'll only be allowed to use it with the
    operations defined in this module.
  *)
  type t

  (*
    This is the only operation we're allowing
    on values of type `t` (for now). This serves as a way
    to allow us to do _some_ things with `t`, but still
    hide away how they work exactly.
  *)
  val ( = ) : t -> t -> bool
end

(*
  Similarly, next we'll define an interface
  for types which are not only equatable, but orderable:
*)
module type Ord = sig
  (*
    We would like to enforce that instances of `Ord` also
    include the operations from `Eq`. We can do this using the include statement. 
    `include` in OCaml modules is not dissimilar to C's `#include` in that
    you can think of it as pasting the contents of the specified module here
    verbatim.

    In this context, it effectively makes `Ord` an _extension_ of `Eq`. Note that
    using `open` here would not have the same effect, since it would only make
    the contents of `Eq` visible in this scope, but not copy them into it
    completely.
  *)
  include Eq

  val ( < ) : t -> t -> bool
end

(*
  Let us improve our previous assignment's dictionary implementation 
  so that any orderable type can be used as a key.

  Note that you can implement the dictionary any way you like this time,
  just don't make a wrapper around OCaml's `Map`.

*)
module type Dict = sig
  (*
    Like our original definition of dicts, we want the types of values
    to be unconstrained, so they will be the type parameter.
  *)
  type 'a t

  (*
    Rather than strings, this dictionary
    will have keys of some arbitrary orderable type,
    so this type `key` will represent it. 
  *)
  type key

  val empty : 'a t

  (*
    added for sake of invariant check
    true if underlying data structure is properly formatted
    else false
  *)
  val proper_format : 'a t -> bool

  val lookup : key -> 'a t -> 'a option

  val map : f:('a -> 'b) -> 'a t -> 'b t
  
  (* as before, insert adds a mapping and overrides if already present. *)
  val insert : key -> 'a -> 'a t -> 'a t

  (*
    `remove` returns Some(v) if v was associated with the provided key, 
    or None if there is no such key in the dictionary.

    Does not change the dict if the key was not present.
  *)
  val remove : key -> 'a t -> 'a t * 'a option

  (*
    Keeps values from the second argument, when there are duplicate keys.
  *)
  val merge : 'a t -> 'a t -> 'a t
end

(*
  Exercise 1:

  Provide an implementation of `Dict`, making use of an
  unknown module providing the `Ord` interface.

  Feel free to use last assignments' data types to form
  the basis of your implementation, if you like.
  Note you can't use Core.Map "under the covers" here,
  you need to give your own implementation.
*)
module Dict (Key: Ord): (Dict with type key = Key.t) = struct

  (* 
    Simple definition of a binary tree with values at each branching node
    Will be used as the underlying data structure of our Dict
  *)
  type 'a btree =
    | Leaf
    | Branch of 'a btree * 'a * 'a btree

  (*
    Our Key.t from the given module providing the `Ord` interface
    Will be used as the key within our dicts key value pair
    Has Key.( = ) and Key.( < ) defined
  *)
  type key = Key.t

  (*
    Our dictionary data structure
    a btree of 
      key = key (Key.t) 
      value = 'a
  *)
  type 'a t = (key * 'a) btree

  (*
    returns an empty dictionary
  *)
  let empty: 'a t = Leaf

  let list_of_btree (tree: 'a t): key list =
    let rec inorder_traversal (tree: 'a t) (ret_list: key list): key list =
        match tree with
        | Leaf -> ret_list
        | Branch (l, v, r) -> (* Even though it is inorder, we actually need to do reverse order so we can use :: instead of @ *)
            let go_right = inorder_traversal r ret_list in
            let key, _ = v in
            let append = key :: go_right in
            inorder_traversal l append in 
        inorder_traversal tree []
  
  let proper_format (tree: 'a t): bool = 
    let tree_list = list_of_btree tree in
    if List.length tree_list > 0 then
        let rec comparator (prev_val: key) (list: key list): bool =
            match list with
            | [] -> true
            | value :: rem_list ->
                if Key.( < ) prev_val value then comparator value rem_list
                else false 
        in
        let value = List.hd tree_list in
        let rem_list = List.tl tree_list in
        match (value, rem_list) with
        | (Some v, Some l) -> comparator v l
        | (Some _, None) -> true
        | (None, _) -> true (* will never be reached, but would be true *)
    else true
  
  let rec lookup (k: key) (d: 'a t): 'a option =
    match d with
    | Leaf -> None
    | Branch (left, (k1, v1), right) ->
      if Key.( = ) k k1 then Some ( v1 )  (* invariant check: after inserting a k, v pair, a lookup on k should return v *)
      else if Key.( < ) k k1 then lookup k left
      else lookup k right

  let rec map ~(f:('a -> 'b)) (d: 'a t): 'b t =
    match d with
    | Leaf -> Leaf
    | Branch (left, (k1, v1), right) ->
        Branch (map left ~f, (k1, f v1), map right ~f)

  let rec insert (k: key) (v: 'a) (d: 'a t): 'a t =
    if not @@ proper_format d then failwith "not ordered tree - insert"
    else
    match d with
    | Leaf -> Branch (Leaf, (k, v), Leaf)
    | Branch (left, (k1, v1), right) ->
      if Key.( = ) k k1 then Branch (left, (k, v), right)  (* invariant check: tree should be ordered after an insert *)
      else if Key.( < ) k k1 then Branch (insert k v left, (k1, v1), right)
      else Branch (left, (k1, v1), insert k v right)

  (*
    Helper function allowing for a fold over our dict
  *)
  let rec fold_least (d: 'a t) ~(init: 'accum) ~(f: key -> 'a -> 'accum -> 'accum): 'accum =
    match d with
    | Leaf -> init
    | Branch (left, (key, value), right) ->
        let fold_left = fold_least left ~init ~f in
        let accumulated = f key value fold_left in
        fold_least right ~init:accumulated ~f

  (*
      Helper function that returns the key value pair with the largest key value in a given dict
  *)
  let max_key_and_val (d: 'a t): key * 'a =
    match d with
    | Leaf -> failwith "empty"
    | Branch (_, (k, v), _) ->
      fold_least d ~init:(k, v) ~f:(fun next_key next_val (k, v) -> 
      if Key.( < ) k next_key then (next_key, next_val) else (k, v))

  let remove (k: key) (d: 'a t): 'a t * 'a option =
    let value = lookup k d in
    let rec traversal (k: key) (d: 'a t): 'a t =
      match d with
      | Leaf -> Leaf
      | Branch (left, (k1, v1), right) when Key.( < ) k k1 ->
          Branch (traversal k left, (k1, v1), right)
      | Branch (left, (k1, v1), right) when Key.( < ) k1 k ->
          Branch (left, (k1, v1), traversal k right)
      | Branch (Leaf, (_, _), right) -> right
      | Branch (left, (_, _), Leaf) -> left
      | Branch (left, (_, _), right) ->
          match max_key_and_val left with
          | max_key, value -> Branch (traversal max_key left, (max_key, value), right)
    in
    traversal k d, value (* invariant check: tree should be ordered after a remove *)

  let merge (d1: 'a t) (d2: 'a t): 'a t =
    fold_least d2 ~init:(d1) ~f:(fun key value merged_dict -> insert key value merged_dict)
end

(*
  Subtle point: "Over-encapsulation"

  The great advantage of programming against the `Ord`
  or `Dict` interface is precisely not having to know
  what the underlying type `t` is; this
  restricts your dependent code from becoming tightly coupled
  to any particular instantiation of the interface.

  However, the definition of the `Dict` interface
  makes no distinction between how "hidden" the `t` and `elt`
  types should be. This means that both are only presented
  as abstract types to any code outside the module... but
  we need to create a value of type `elt` in order
  to use any of the Dict's features! We've been so successful
  at hiding the implementation, that we can't even use it.

  Fortunately OCaml has a way to specifically un-hide
  an abstract type in a module signature for this reason.
  Consider the following definition of `Ord` for integers:
*)
module Int_ord : Ord with type t = int = struct
  type t = int

  let ( = ) i1 i2 = Int.equal i1 i2

  let ( < ) i1 i2 = Int.compare i1 i2 < 0
end

(*
  The module type `(Ord with type t = int)` guarantees that
  all of the contents of `Ord` are satisfied, and additionally
  provides the information that the type `t` is `int`.

*)

(*
  Exercise 2:

  Modify the signature given to `Dict` in your implementation
  so that they expose the `elt` and `key` types, solving the over-hiding
  problem so that the data structure can finally be used.

  Hint: remember that you can externally refer to a type ty defined within a module
  Mod by the notation "Mod.ty".  You will need that here.

  Put the improved definitions right here; since they are later in the file they
  will override the ones given above:
*)

(*
  Exercise 3(a):

  Given an integer `n`, use an `int Dict.t ref` to
  design a memoized definition of fibonacci
  which is recursive but does not take exponential time.

  That is, use a Dict to store intermediate answers so they
  do not need to be re-computed if requested again. This should
  work across calls to the top-level function.

  You will need to make use of side-effects/mutability to achieve this,
  and can introduce a global `Dict.t ref` variable to help.
*)

(*
  Helper function which adds two 'a option if both are Some
  else return -1
*)
let some_add (x: 'a option) (y: 'a option) =
  match x, y with
  | Some num1, Some num2 -> num1 + num2
  | _, _ -> (-1)
;;

(*
  Helper function that unwraps an int option
  -1 if None
*)
let unwrap_opt (x: 'a option) =
  match x with
  | Some x -> x
  | None -> (-1)
;;

module Memo_Dict = Dict (Int);;

let memo = ref (
      Memo_Dict.insert 1 1 @@ 
      Memo_Dict.insert 0 0 @@
      Memo_Dict.insert (-1) 2 Memo_Dict.empty) (* use pair with key: -1 data: next to be calculated *)
;;

let memoized_fib (n : int) : int =
  let fib_check = unwrap_opt @@ Memo_Dict.lookup n !memo in
  if fib_check = (-1) then 
    let rec memoize (n: int) (i: int) = (* invariant check: after a call to memoize n, memo n should be populated *)
      if i > n then ()
      else 
        let new_fib = 
          some_add (Memo_Dict.lookup (i - 1) !memo) (Memo_Dict.lookup (i - 2) !memo) 
        in
        memo := Memo_Dict.insert i new_fib !memo;
        memo := Memo_Dict.insert (-1) (i + 1) !memo; (* update next to be calculated *)
        memoize n (i + 1)
    in
    memoize n @@ unwrap_opt (Memo_Dict.lookup (-1) !memo);
    unwrap_opt @@ Memo_Dict.lookup n !memo
  else fib_check
;;

(*
  Exercise 3(b):
  Repeat the above but using the built-in functional dictionary, a Map.t ref
*)
module Memo_Dict' = Map.Make (Int);;

let memo' = ref (
  Memo_Dict'.add_exn ~key:1 ~data:1 @@ 
  Memo_Dict'.add_exn ~key:0 ~data:0 @@ 
  Memo_Dict'.add_exn ~key:(-1) ~data:2 Memo_Dict'.empty) (* use pair with key: -1 data: next to be calculated *)

let memoized_fib' (n : int) : int =
  let fib_check = unwrap_opt @@ Memo_Dict'.find !memo' n in
  if fib_check = (-1) then 
    let rec memoize (n: int) (i: int) = (* invariant check: after a call to memoize n, memo' n should be populated *)
      if i > n then ()
      else 
        let new_fib = 
          some_add (Memo_Dict'.find !memo' (i - 1)) (Memo_Dict'.find !memo' (i - 2)) 
        in
        memo' := Memo_Dict'.add_exn ~key:i ~data:new_fib !memo';
        memo' := Memo_Dict'.set ~key:(-1) ~data:(i + 1) !memo'; (* update next to be calculated *)
        memoize n (i + 1)
    in
    memoize n @@ unwrap_opt (Memo_Dict'.find !memo' (-1));
    unwrap_opt @@ Memo_Dict'.find !memo' n
  else fib_check
;;
(*
  Exercise 3(c):

  The above approaches are not optimal: if you are making 
  a mutable structure you should use one which supports
  fine-grained mutation instead of only global replacement
  as in the above.  So, refactor `memoized_fib` from above to use
  a Core.Hashtbl (Core's mutable map/dict) instead.  This implementation 
  will be much faster as it can update the memoized values in-place.
*)
module Memo_Dict'' = Hashtbl.Make (Int);;

let memo'' = Memo_Dict''.create ();;
let () = Memo_Dict''.add_exn memo'' ~key:0 ~data:0;;
let () = Memo_Dict''.add_exn memo'' ~key:1 ~data:1;;
let () = Memo_Dict''.add_exn memo'' ~key:(-1) ~data:2;; (* use pair with key: -1 data: next to be calculated *)

let memoized_fib'' (n : int) : int =
  let fib_check = unwrap_opt @@ Memo_Dict''.find memo'' n in
  if fib_check = (-1) then 
    let rec memoize (n: int) (i: int) = (* invariant check: after a call to memoize n, memo'' n should be populated *)
      if i > n then ()
      else 
        let new_fib = 
          some_add (Memo_Dict''.find memo'' (i - 1)) (Memo_Dict''.find memo'' (i - 2)) 
        in
        Memo_Dict''.add_exn ~key:i ~data:new_fib memo'';
        Memo_Dict''.set ~key:(-1) ~data:(i + 1) memo''; (* update next to be calculated *)
        memoize n (i + 1)
    in
    memoize n @@ unwrap_opt (Memo_Dict''.find memo'' (-1));
    unwrap_opt @@ Memo_Dict''.find memo'' n
  else fib_check
;;

(*
  Exercise 4:

  For this question we will develop a generic operator language parser / evaluator.
  We will make it generic by allowing us to "plug in" the underlying data type.

  Note we will simplify several things so this is primarily an OCaml abstraction
  exercise and not a parsing exercise.  We will have only `+` and `*` binary
  operations, will just parse a string rather than reading from a stream, and 
  will use postfix (aka RPN) notation for operators: "1 2 +" will return 3.
  
*)

(* Here is the module type for the underlying data.
   The key function is next, it reads a `t` off of the front of the string,
   and returns the remainder of the string as well as the `t` element.

   Here are some clarifications on how next works.
   1. whitespace (space, tab, newline) is a separator, the `t` value ends at that point.
   2. `next` is only reponsible for reading a `t` off the front of the string
   3. It should obey the "maximal munch" principle, read in as many characters as possible
     whilst still making a `t`.  So for example on input `"12"` for `t = int` read in `12`, not `1`.
     On "12@" next will return `12` plus remainder "@", `next` is not responsible for checking 
     for other errors.
   4. `next` will return `None` if there is no `t` to read from the front of the string.  So for example is the string starts with a non-digit or `-` for the case of integers.
*)

module type Data = sig
  type t

  val from_string : string -> t

  val to_string : t -> string

  val next : string -> (string * t) option

  val plus : t -> t -> t

  val times : t -> t -> t
end

(* The Evaluator for this simple language is then a functor of this type. 
   If the input cannot be parsed, `eval` will return `Error "string"`, otherwise it will
   return `Some(t-value)`. 
   
   Clarifications:
   1. If an illegal character is encountered, the evaluator will return `Error "illegal character"`.
   2. If there are too few or too many operators (as in "1 2 + +" or "1 2") return `Error "unmatched"
   3. Note that operators need not be space-separated, e.g. "1 2 3++" returns `6`.
   4. '+' and '*' are the characters corresponding to the `plus` and `times` binary operations. 
     There are no other operations supported, all others are illegal characters.
   *)

module type Eval = functor (Data : Data) -> sig
  type t = Data.t

  val eval : string -> (t , string) result
end

(* a. Write the evaluator functor matching this signature.  *)

module Eval : Eval = functor (Data : Data) -> struct
  type t = Data.t

  (* list ref acting as our stack for calculator *)
  let stack = ref []

  (* 
    helper function for eval, figures out what scenario has occured when next returns None
  *)
  let error_check (s: string) =
    if String.length s = 0 && List.length !stack = 1 then Ok ('f')
    else if String.length s = 0 then Error "unmatched"
    else
      let first_char = String.get s 0 in
      if (Char.( = ) first_char '+' || Char.( = ) first_char '*') && List.length !stack >= 2 then Ok (first_char)
      else if Char.( = ) first_char '+' || Char.( = ) first_char '*' then Error "unmatched"
      else Error "illegal character"

  (*
    helper function for operate, removes the operator character from the string and returns the new string
  *)
  let remove_first_char (s: string): string =
    match String.to_list s with
    | _ :: xs -> String.of_char_list xs
    | _ -> failwith "removing from empty string"

  (*
    helper function for eval, when given an operator character, 
    performs operation on stack and returns rest of string for continued evaluation
  *)
  let operate (op: char) (s: string): string =
    let v1 = List.hd_exn !stack in
    stack := List.tl_exn !stack;
    let v2 = List.hd_exn !stack in
    stack := List.tl_exn !stack;
    match op with
    | '+' -> 
      stack := List.cons (Data.plus v1 v2) !stack;
      String.lstrip @@ remove_first_char s
    | '*' ->
      stack := List.cons (Data.times v1 v2) !stack;
      String.lstrip @@ remove_first_char s
    | _ -> failwith "invalid operating char passed"
 
  let eval (s: string): (t , string) result =
    let rec eval_loop (s: string) =
      match Data.next s with
      | Some (new_str, next_t) ->
        stack := List.cons next_t !stack;
        eval_loop new_str
      | None -> 
        match error_check @@ String.lstrip s with
        | Ok op ->
          if Char.( = ) op 'f' then Ok (List.hd_exn !stack)
          else eval_loop @@ operate op s;
        | Error msg -> Error msg
    in
    stack := [];
    eval_loop s
end



(* b. Make Int_Data and Rat_Data modules for parsing integers and rationals. 

    - Integers may optionally be signed, so -4 is an integer 
    - Rationals are written as "3/4", integers separated by "/".  They also may be signed
with a signed prefix (only in numerator, "3/-4" is not a rational).
    *)

(*
  helper function for next in both Int_Data and Rat_Data
  given a char list with either a '-' or digit at the front,
  returns the maximal munch int and the remaining string
*)
let get_num (char_l: char list): (string * int) =
  let neg_check (cl: char list): (char list * int) =
    if Char.( = ) (List.hd_exn cl) '-' then (List.tl_exn cl, (-1))
    else (cl, 1)
  in
  let rec parse_num (s: string) (cl: char list) =
    match cl with
    | [] -> ([], int_of_string s)
    | c :: tl ->
      if Char.is_digit c then parse_num (s ^ Char.escaped c) tl
      else (cl, int_of_string s)
  in
  let new_list, negator = neg_check char_l in
  let ret_cl, ret_val = parse_num "" new_list in
  (String.lstrip @@ String.of_char_list ret_cl, negator * ret_val)
;;

let some_wrap ((s: string), data) = Some (s, data);;

module Int_Data = struct
  type t = int

  let from_string (s: string): t =
    int_of_string s

  let to_string (data: t): string =
    Int.to_string data

  let next (s: string): (string * t) option =
    let trim_str = String.lstrip s in
    if String.length trim_str = 0 then None
    else
      let first_char = String.get trim_str 0 in
      if ((not (Char.is_digit first_char )) && Char.( <> ) first_char '-') then None
      else if Char.( = ) first_char '-' && 
        ((not (String.length trim_str >= 2)) || (not (Char.is_digit @@ String.get trim_str 1))) then None
      else
        String.to_list trim_str
        |> get_num
        |> some_wrap 

  let plus (v1: t) (v2: t): t = v1 + v2
  
  let times (v1: t) (v2: t): t = v1 * v2
end

module Rat_Data = struct
  type t = (int * int)

  let from_string (s: string): t =
    match String.split s ~on:'/' with
    | numerator :: denominator :: [] ->
      (int_of_string numerator), (int_of_string denominator)
    | _ -> failwith "invalid string in from_string"

  let to_string ((n, d): t): string =
    Int.to_string n ^ "/" ^ Int.to_string d

  (*
    helper function for next, very similar to get num but handles error cases:
    1. Denominator = 0
    2. Has negative
    returns "illegal denominator", -1 when error case
  *)
  let get_denominator (char_l: char list): (string * int) =
    let rec parse_denom (s: string) (cl: char list) =
      match cl with
      | [] -> ([], int_of_string s)
      | c :: tl ->
        if Char.is_digit c then parse_denom (s ^ Char.escaped c) tl
        else if Char.( = ) c '-' then ([], -1)
        else (cl, int_of_string s)
    in
    let new_list, denominator = parse_denom "" char_l in
    if denominator = 0 || denominator = -1 then ("illegal denominator", -1)
    else (String.lstrip @@ String.of_char_list new_list, denominator)

  (*
    helper function which checks if the first char in a char list is '/'
    if so, returns true, remaining char list
    else false, empty list
  *)
  let has_divisor (char_l: char list): bool * char list =
    match char_l with
    | '/' :: tl -> true, tl
    | _ -> false, []

  (*
    gcd function to be used in simplify and least common multiple
  *)
  let rec gcd x y = if y = 0 then x else gcd y (x mod y) 

  (*
    helper function which returns a given t in its most simplified form
  *)
  let simplify (n, d: t): t =
    let divisor = gcd (Int.abs n) (Int.abs d) in
    (n / divisor, d / divisor)

  let next (s: string): (string * t) option =
    let trim_str = String.lstrip s in
    if String.length trim_str = 0 then None
    else
      let first_char = String.get trim_str 0 in
      if ((not (Char.is_digit first_char)) && Char.( <> ) first_char '-') then None
      else if Char.( = ) first_char '-' &&
        ((not (String.length trim_str >= 4)) || (not (Char.is_digit @@ String.get trim_str 1))) then None
      else
        let num_chars = String.to_list trim_str in
        let check_string, numerator = get_num num_chars in
        let has_dash, denom_chars = has_divisor @@ String.to_list check_string in
        if not has_dash || List.length denom_chars = 0 then None
        else let ret_str, denominator = get_denominator denom_chars in
        if denominator = -1 then None
        else some_wrap (String.lstrip ret_str, simplify (numerator, denominator))

  (*
    helper function which returns the lcm of two numbers
  *)
  let least_common_multiple x y = x * y / gcd x y
  
  let plus (v1: t) (v2: t): t =
    let n1, d1 = v1 in
    let n2, d2 = v2 in
    let lcm = least_common_multiple d1 d2 in
    simplify (n1 * (lcm / d1) + n2 * (lcm / d2), lcm)

  let times (v1: t) (v2: t): t =
    let n1, d1 = v1 in
    let n2, d2 = v2 in
    simplify (n1 * n2, d1 * d2)
  end
  
(* With this we may now create evaluators for integers and rationals. *)



module Int_Eval = Eval(Int_Data)

module Rat_Eval = Eval(Rat_Data)

(*

At this point you should look at the types and see if you "hid too much"
as per the Dict example above; you might need some `with` declarations
here to solve the over-hiding problem.

*)

(*
Int_Eval.eval "2 3 +" should now return `OK(5).  Make sure to 
write a good array of such tests in `tests.ml` to make sure your
implementation is working.
*)