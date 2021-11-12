(* Part I

   This file specifies the interface for your code and must not 
   be edited (it will also not be included in your zip submission). 
   Your actual implementation should go in the file
   `treedict.ml`  which satisfies this interface appropriately.

   Mutation operations of OCaml are not allowed, or required.
   
*)

(*
    Section 1: Trees
*)

(*
    What follows is a simple definition of a binary tree,
    with values at each branching node.

    There are many interesting operations which can be performed
    on such tree structures, which will be explored.
*)

type 'a btree =
  | Leaf
  | Branch of 'a btree * 'a * 'a btree 


(*
    Given a btree, compute the number of total values it contains.
    Each Branch constructor counts for 1.
*)
val size : 'a btree -> int

(*
    Given a btree, produce a btree which is the mirror image:
    flip all left and right nodes. 
*)
val flip : 'a btree -> 'a btree

(*
    Given a btree, flatten it into a list which retains the btree's ordering (the 'inorder' traversal).
    This should take O(n) time; recall that each (::)) is O(1) and each (@) is O(n).
*)
val list_of_btree : 'a btree -> 'a list

(*
    Given a comparison function for its elements, check whether a btree is ordered, such
    that all left subtree elements are less than the branch value,
    and all right subtree elements are greater.

    `compare a b` should return:
        -1  if  a < b
         0  if  a = b
        +1  if  a > b

    That is, for comparison operators <op>, 
    `compare a b <op> 0` iff `a <op> b`.

    See `Core.Int.compare`, `Core.String.compare` etc. for examples.
*)
val is_ordered_tree : 'a btree -> compare:('a -> 'a -> int) ->  bool

(*
    Section 2: Trees as Maps
*)

(*
    Represent a dictionary as a binary search tree of pairs, where
    - the first element of the pair is a string key (keys are always strings)
    - the second element of the pair is the associated value.

    As this is not representing a multimap, no operations
    here should preserve multiple values with the same key.
*)
type 'a dict = (string * 'a) btree

(*
    Write a well-formedness checker which verifies 
      - there are no duplicate keys 
      - The underlying tree is in fact a binary search tree: left keys are less than right keys
    Note that all the operations on 'a dict should both take and
    return only well-formed dictionaries.
*)

val is_well_formed_dict : 'a dict -> bool

(*
    Given a string and a dictionary, look up the associated value, if any.
*)
val lookup : string -> 'a dict -> 'a option

(*
    Given a dict and some transforming operation, apply the operation
    to each value within the dictionary to produce a new dict, 
    but keeping the keys constant.  Note that the mapping function f
    can use the key in its calculation so we also pass it as an argument.
*)
val map : 'a dict -> f:(string ->'a -> 'b) -> 'b dict


(*
    Define a fold_least operation over dictionaries. It is "least" because it will
    process the elements starting with the lexicographically least key.

    For example, for dictionary which is informally { "a" |-> 5, "b" |-> 7},
    the result will be the computation of f "b" 7 (f "a" 5 init).

*)
val fold_least : 'a dict -> init:'accum -> f:(string -> 'a -> 'accum -> 'accum) -> 'accum


(* 
    Show the usefulness of folding by finding the maximal value in a dict using
    fold_least and no `let rec`.  compare is the comparison operation as used above
    and in lecture.
*)

val max_value : 'a dict -> compare:('a -> 'a -> int) -> 'a

(*
    Given a string key and a value, insert the pair into the dictionary,
    overwriting any existing value attached to the key in the dict if present.
*)
val insert : 'a dict -> string -> 'a -> 'a dict

(*
    Given two dictionaries, merge them into one, preserving the ordering property.
    If both contain a value associated to a key, retain the second dict's value in the result.

    You may use the functions defined earlier in this .mli file but no new recursive functions. 
      (partial credit will be given if you need to use recursion)
*)
val merge : 'a dict -> 'a dict -> 'a dict

(*
    Given two dictionaries, merge them into one, preserving the ordering property.
    Compute the new values for each dictionary key using the supplied merger 
    function, feeding it Some or None depending on the presence of each key in the first and second dict.

*)
val merge_with : 'a dict -> 'b dict ->  merger:('a option -> 'b option -> 'c) -> 'c dict
