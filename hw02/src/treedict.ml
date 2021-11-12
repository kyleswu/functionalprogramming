(*

FPSE Assignment 2

Name                  : Kyle Wu
List of Collaborators :

Please make a good faith effort at listing people you discussed any 
problems with here, as per the course academic integrity policy.  
CAs/Prof need not be listed!

See file treedict.mli for the specification of Part I of the assignment,
and histo.ml for Part II.  Recall from lecture that .mli files are module 
signatures aka module types and you will need to provide implementations of all the functions listed there in this file. 

Your Part I answers go here, and the Part II answers go in the histo.ml file.

Hint: to start out you will want to copy over the .mli file and make dummy headers for each function similar to what we gave you for Assignment 1.  Recall that the syntax is slightly different in .ml and .mli  
declarations, e.g. `val` in .mli is `let` in .ml, etc.

Note that .ml files need to include all `type` declarations in .mli files.

*)

open Core

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
let rec size (tree: 'a btree): int =
    match tree with
    | Leaf -> 0
    | Branch (l, _, r) -> 1 + size l + size r 
;;

(*
    Given a btree, produce a btree which is the mirror image:
    flip all left and right nodes. 
*)
let rec flip (tree: 'a btree): 'a btree =
    match tree with
    | Leaf -> Leaf
    | Branch (l, v, r) -> Branch (flip r, v, flip l)
    ;;

(*
    Given a btree, flatten it into a list which retains the btree's ordering (the 'inorder' traversal).
    This should take O(n) time; recall that each (::)) is O(1) and each (@) is O(n).
*)
let list_of_btree (tree: 'a btree): 'a list =
    let rec inorder_traversal (tree: 'a btree) (ret_list: 'a list): 'a list =
        match tree with
        | Leaf -> ret_list
        | Branch (l, v, r) -> (* Even though it is inorder, we actually need to do reverse order so we can use :: instead of @ *)
            let go_right = inorder_traversal r ret_list in
            let append = v :: go_right in
            inorder_traversal l append in 
        inorder_traversal tree []
    ;;

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
let is_ordered_tree (tree: 'a btree) ~(compare : 'a -> 'a -> int): bool = 
    let tree_list = list_of_btree tree in
    if List.length tree_list > 0 then
        let rec comparator (prev_val: 'a) (list: 'a list) ~(compare: 'a -> 'a -> int): bool =
            match list with
            | [] -> true
            | value :: rem_list ->
                if compare prev_val value <= 0 then comparator value rem_list ~compare
                else false in
        let value = List.hd tree_list in
        let rem_list = List.tl tree_list in
        match (value, rem_list) with
        | (Some v, Some l) -> comparator v l ~compare
        | (Some _, None) -> true
        | (None, _) -> true (* will never be reached, but would be true *)
    else true
    ;;

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

let is_well_formed_dict (d: 'a dict): bool =
    let compare_string_pair ((s1, _): (string * 'a)) ((s2, _): (string * 'a)): int =
        if String.(<) s1 s2 then -1 else 1 in
    is_ordered_tree d ~compare:(compare_string_pair)
    ;;

(*
    Given a string and a dictionary, look up the associated value, if any.
*)
let rec lookup (s: string) (d: 'a dict): 'a option =
    match d with
    | Leaf -> None
    | Branch (dict_left, (s1, v1), dict_right) ->
        if String.(=) s s1 then Some(v1) 
        else if String.(<) s s1 then lookup s dict_left
        else lookup s dict_right
    ;;

(*
    Given a dict and some transforming operation, apply the operation
    to each value within the dictionary to produce a new dict, 
    but keeping the keys constant.  Note that the mapping function f
    can use the key in its calculation so we also pass it as an argument.
*)
let rec map (d: 'a dict) ~(f:(string ->'a -> 'b)): 'b dict =
    match d with
    | Leaf -> Leaf
    | Branch (dict_left, (s1, v1), dict_right) ->
        Branch (map dict_left ~f, (s1, f s1 v1), map dict_right ~f)
    ;;


(*
    Define a fold_least operation over dictionaries. It is "least" because it will
    process the elements starting with the lexicographically least key.

    For example, for dictionary which is informally { "a" |-> 5, "b" |-> 7},
    the result will be the computation of f "b" 7 (f "a" 5 init).

*)
let rec fold_least (d: 'a dict) ~(init: 'accum) ~(f: string -> 'a -> 'accum -> 'accum): 'accum =
    match d with
    | Leaf -> init
    | Branch (left, (str, value), right) ->
        let fold_left = fold_least left ~init ~f in
        let accumulated = f str value fold_left in
        fold_least right ~init:accumulated ~f
    ;;


(* 
    Show the usefulness of folding by finding the maximal value in a dict using
    fold_least and no `let rec`.  compare is the comparison operation as used above
    and in lecture.
*)
let max_value (d: 'a dict) ~(compare: ('a -> 'a -> int)): 'a =
    let greater_val (value1: 'a) (value2: 'a): 'a =
        if compare value1 value2 > 0 then value1 else value2 in
    match d with
    | Leaf -> failwith "empty"
    | Branch (_, (_, value), _) ->
        fold_least d ~init:value ~f:(fun _ v max -> greater_val v max)
    ;;

(*
    Given a string key and a value, insert the pair into the dictionary,
    overwriting any existing value attached to the key in the dict if present.
*)
let rec insert (d: 'a dict) (key: string) (value: 'a): 'a dict =
    match d with
    | Leaf -> Branch (Leaf, (key, value), Leaf)
    | Branch (left, (k, v), right) ->
        if String.(=) key k then Branch (left, (key, value), right)
        else if String.(<) key k then Branch ((insert left key value), (k, v), right)
        else Branch (left, (k, v), (insert right key value))
    ;;

(*
    Given two dictionaries, merge them into one, preserving the ordering property.
    If both contain a value associated to a key, retain the second dict's value in the result.

    You may use the functions defined earlier in this .mli file but no new recursive functions. 
      (partial credit will be given if you need to use recursion)
*)
let merge (d1: 'a dict) (d2: 'a dict): 'a dict =
    fold_least d2 ~init:(d1) ~f:(fun key value merged_dict -> insert merged_dict key value)
    ;;

(*
    Given two dictionaries, merge them into one, preserving the ordering property.
    Compute the new values for each dictionary key using the supplied merger 
    function, feeding it Some or None depending on the presence of each key in the first and second dict.

*)
let merge_with (d1: 'a dict) (d2: 'b dict) ~(merger: 'a option -> 'b option -> 'c): 'c dict =
    let merge_val (key: string): 'c = 
        merger (lookup key d1) (lookup key d2) in
    fold_least d2 
    ~init:(fold_least d1 ~init:Leaf ~f:(fun key _ init -> insert init key (merge_val key))) 
    ~f:(fun key _ init -> insert init key (merge_val key))
    ;;