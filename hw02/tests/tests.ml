
(* open Core *)
open OUnit2
module T = Treedict
(* module H = Histo *)
let test_flip _ =
  assert_equal T.(Leaf) @@ T.(flip Leaf);
  assert_equal T.(
    Branch (Branch (Leaf, 2.0, Leaf), 1.0, Leaf)
  ) @@ T.flip T.(
    Branch (Leaf, 1.0, Branch (Leaf, 2.0, Leaf))
  );
  assert_equal T.(
    Branch (Branch (Leaf, 2.0, Branch (Leaf, 3.0, Leaf)), 1.0, Leaf)
  ) @@ T.flip T.(
    Branch (Leaf, 1.0, Branch (Branch (Leaf, 3.0, Leaf), 2.0, Leaf))
  )

let test_size _ =
  assert_equal 0 @@ T.size T.(Leaf);
  assert_equal 3 @@ T.size T.(
    Branch (Branch (Leaf, 0.0, Leaf), 1.0, Branch (Leaf, 2.0, Leaf))
  )

let test_list_of_btree _ =
  assert_equal [] @@ T.list_of_btree T.(Leaf);
  assert_equal [1;2;3] @@ T.list_of_btree T.(
    Branch (Branch (Leaf, 1, Leaf), 2, Branch (Leaf, 3, Leaf)));
  assert_equal [1;2;3] @@ T.list_of_btree T.(
    Branch (Branch (Branch (Leaf, 1, Leaf), 2, Leaf), 3, Leaf));
  assert_equal [1;2;3] @@ T.list_of_btree T.(
    Branch (Leaf, 1, Branch (Branch (Leaf, 2, Leaf), 3, Leaf)))

let test_is_ordered _ =
  assert_equal true @@ T.is_ordered_tree ~compare:Int.compare T.(Leaf);
  assert_equal true @@ T.is_ordered_tree ~compare:Int.compare T.(
    (Branch (Leaf, 1, Branch (Branch (Leaf, 2, Leaf), 3, Leaf)))
  );
  assert_equal false @@ T.is_ordered_tree ~compare:String.compare T.(
    Branch (Branch (Leaf, "b", Leaf), "a", Leaf)
  )
;;

let test_well_formed _ =
  assert_equal true @@ T.is_well_formed_dict T.(Leaf);
  assert_equal true @@ T.is_well_formed_dict T.(
    Branch (Branch (Leaf, ("a", 9), Branch (Leaf, ("b", 1), Leaf)), ("c", 6), Leaf)
  );
  assert_equal false @@ T.is_well_formed_dict T.(
    Branch (Branch (Leaf, ("b", 9), Branch (Leaf, ("a", 1), Leaf)), ("c", 6), Leaf)
  )
;;

let test_lookup _ =
  assert_equal true @@ T.is_well_formed_dict T.(Branch (Branch (Leaf, ("a", 8), Leaf), ("b", 5), Branch (Leaf, ("c", 1), Leaf)));
  assert_equal (Some 8) @@ T.lookup "a" T.(
    Branch (Branch (Leaf, ("a", 8), Leaf), ("b", 5), Branch (Leaf, ("c", 1), Leaf))
  );
  assert_equal (Some 5) @@ T.lookup "b" T.(
    Branch (Branch (Leaf, ("a", 8), Leaf), ("b", 5), Branch (Leaf, ("c", 1), Leaf))
  );
  assert_equal (Some 1) @@ T.lookup "c" T.(
    Branch (Branch (Leaf, ("a", 8), Leaf), ("b", 5), Branch (Leaf, ("c", 1), Leaf))
  );
  assert_equal None @@ T.lookup "z" T.(
    Branch (Branch (Leaf, ("a", 8), Leaf), ("b", 5), Branch (Leaf, ("c", 1), Leaf))
  );
  assert_equal None @@ T.lookup "a" T.(Leaf)
;;

let test_map _ =
  assert_equal true @@ T.is_well_formed_dict T.(Branch (Branch (Leaf, ("a", 8), Leaf), ("b", 5), Branch (Leaf, ("c", 1), Leaf)));
  assert_equal T.(
    Branch (Branch (Leaf, ("a", 9), Leaf), ("b", 6), Branch (Leaf, ("c", 2), Leaf))
  ) @@ T.map T.(Branch (Branch (Leaf, ("a", 8), Leaf), ("b", 5), Branch (Leaf, ("c", 1), Leaf))) ~f:(fun _ value -> value + 1);
  assert_equal T.(
    Branch (Branch (Leaf, ("a", "a"), Leaf), ("b", "b"), Branch (Leaf, ("c", "c"), Leaf))
  ) @@ T.map T.(Branch (Branch (Leaf, ("a", 9), Leaf), ("b", 5), Branch (Leaf, ("c", 1), Leaf))) ~f:(fun str _ -> str)
;;

let test_fold _ =
  assert_equal true @@ T.is_well_formed_dict T.(Branch (
    Branch (Branch (Leaf, ("a", 5), Leaf), ("b", 8), Branch (Leaf, ("c", 3), Leaf)), 
    ("d", 5), 
    Branch (Branch (Leaf, ("e", 2), Leaf), ("f", 1), Branch (Leaf, ("g", 9), Leaf)))
    );
  assert_equal 7 @@ T.fold_least T.(Branch (
    Branch (Branch (Leaf, ("a", 5), Leaf), ("b", 8), Branch (Leaf, ("c", 3), Leaf)), 
    ("d", 5), 
    Branch (Branch (Leaf, ("e", 2), Leaf), ("f", 1), Branch (Leaf, ("g", 9), Leaf)))
    ) ~init:(0) ~f:(fun _ _ init -> init + 1)
;;

let test_max_value _ =
  assert_equal true @@ T.is_well_formed_dict T.(Branch (
    Branch (Branch (Leaf, ("a", 5), Leaf), ("b", 8), Branch (Leaf, ("c", 3), Leaf)), 
    ("d", 5), 
    Branch (Branch (Leaf, ("e", 2), Leaf), ("f", 1), Branch (Leaf, ("g", 9), Leaf)))
    );
  assert_equal 9 @@ T.max_value T.(Branch (
    Branch (Branch (Leaf, ("a", 5), Leaf), ("b", 8), Branch (Leaf, ("c", 3), Leaf)), 
    ("d", 5), 
    Branch (Branch (Leaf, ("e", 2), Leaf), ("f", 1), Branch (Leaf, ("g", 9), Leaf)))
    ) ~compare:(Int.compare);
  assert_equal 10 @@ T.max_value T.(Branch (
    Branch (Branch (Leaf, ("a", 5), Leaf), ("b", 8), Branch (Leaf, ("c", 3), Leaf)), 
    ("d", 10), 
    Branch (Branch (Leaf, ("e", 2), Leaf), ("f", 1), Branch (Leaf, ("g", 9), Leaf)))
    ) ~compare:(Int.compare);
  assert_equal 16 @@ T.max_value T.(Branch (
    Branch (Branch (Leaf, ("a", 16), Leaf), ("b", 8), Branch (Leaf, ("c", 3), Leaf)), 
    ("d", 10), 
    Branch (Branch (Leaf, ("e", 2), Leaf), ("f", 1), Branch (Leaf, ("g", 9), Leaf)))
    ) ~compare:(Int.compare);
;;

let test_insert _ =
  assert_equal true @@ T.is_well_formed_dict T.(Branch (
    Branch (Branch (Leaf, ("b", 5), Leaf), ("d", 8), Branch (Leaf, ("f", 3), Leaf)), 
    ("h", 5), 
    Branch (Branch (Leaf, ("j", 2), Leaf), ("l", 1), Branch (Leaf, ("n", 9), Leaf)))
    );
  assert_equal T.(Branch (
    Branch (Branch (Leaf, ("b", 5), Branch (Leaf, ("c", 9), Leaf)), ("d", 8), Branch (Leaf, ("f", 3), Leaf)), 
    ("h", 5), 
    Branch (Branch (Leaf, ("j", 2), Leaf), ("l", 1), Branch (Leaf, ("n", 9), Leaf)))
    ) @@ T.insert T.(Branch (
    Branch (Branch (Leaf, ("b", 5), Leaf), ("d", 8), Branch (Leaf, ("f", 3), Leaf)), 
    ("h", 5), 
    Branch (Branch (Leaf, ("j", 2), Leaf), ("l", 1), Branch (Leaf, ("n", 9), Leaf)))
    ) "c" 9;
  assert_equal T.(Branch (
    Branch (Branch (Leaf, ("b", 5), Leaf), ("d", 8), Branch (Leaf, ("f", 3), Leaf)), 
    ("h", 5), 
    Branch (Branch (Leaf, ("j", 2), Branch (Leaf, ("k", 9), Leaf)), ("l", 1), Branch (Leaf, ("n", 9), Leaf)))
    ) @@ T.insert T.(Branch (
    Branch (Branch (Leaf, ("b", 5), Leaf), ("d", 8), Branch (Leaf, ("f", 3), Leaf)), 
    ("h", 5), 
    Branch (Branch (Leaf, ("j", 2), Leaf), ("l", 1), Branch (Leaf, ("n", 9), Leaf)))
    ) "k" 9;
  assert_equal T.(Branch (
    Branch (Branch (Leaf, ("b", 5), Leaf), ("d", 8), Branch (Leaf, ("f", 10), Leaf)), 
    ("h", 5), 
    Branch (Branch (Leaf, ("j", 2), Leaf), ("l", 1), Branch (Leaf, ("n", 9), Leaf)))
    ) @@ T.insert T.(Branch (
    Branch (Branch (Leaf, ("b", 5), Leaf), ("d", 8), Branch (Leaf, ("f", 3), Leaf)), 
    ("h", 5), 
    Branch (Branch (Leaf, ("j", 2), Leaf), ("l", 1), Branch (Leaf, ("n", 9), Leaf)))
    ) "f" 10
;;

let test_merge _ =
  assert_equal true @@ T.is_well_formed_dict T.(Branch (
  Branch (Leaf, ("a", 1), Leaf), 
  ("d", 2), 
  Branch (Leaf, ("f", 3), Leaf))
  );
  assert_equal true @@ T.is_well_formed_dict T.(Branch (
  Branch (Leaf, ("a", 6), Leaf), 
  ("b", 2), 
  Branch (Branch (Leaf, ("c", 2), Leaf), ("e", 3), Leaf))
  );
  assert_equal true @@ T.is_well_formed_dict T.(Branch (
  Branch (Leaf, ("a", 6), Branch (Leaf, ("b", 2), Branch (Leaf, ("c", 2), Leaf))), 
  ("d", 2), 
  Branch (Branch (Leaf, ("e", 3), Leaf), ("f", 3), Leaf))
  );
  assert_equal T.(Branch 
  (Branch (Leaf, ("a", 6), Branch (Leaf, ("b", 2), Branch (Leaf, ("c", 2), Leaf))), 
  ("d", 2), 
  Branch (Branch (Leaf, ("e", 3), Leaf), ("f", 3), Leaf)))
  @@ T.merge (T.(Branch 
  (Branch (Leaf, ("a", 1), Leaf), 
  ("d", 2), 
  Branch (Leaf, ("f", 3), Leaf)))) 
  (T.(Branch 
  (Branch (Leaf, ("a", 6), Leaf), 
  ("b", 2), 
  Branch (Branch (Leaf, ("c", 2), Leaf), ("e", 3), Leaf))))
;;

let test_merge_with _ =
  assert_equal true @@ T.is_well_formed_dict T.(Branch (
  Branch (Leaf, ("a", 1), Leaf), 
  ("d", 2), 
  Branch (Leaf, ("f", 3), Leaf))
  );
  assert_equal true @@ T.is_well_formed_dict T.(Branch (
  Branch (Leaf, ("a", "y"), Leaf), 
  ("b", "a"), 
  Branch (Branch (Leaf, ("c", "y"), Leaf), ("e", "a"), Leaf))
  );
  assert_equal true @@ T.is_well_formed_dict T.(Branch (Leaf, ("a", 2),
  Branch (Branch (Leaf, ("b", 1), Branch (Leaf, ("c", 1), Leaf)), ("d", 1),
   Branch (Branch (Leaf, ("e", 1), Leaf), ("f", 1), Leaf)))
  );
  assert_equal T.(Branch (Leaf, ("a", 2),
  Branch (Branch (Leaf, ("b", 1), Branch (Leaf, ("c", 1), Leaf)), ("d", 1),
   Branch (Branch (Leaf, ("e", 1), Leaf), ("f", 1), Leaf))))
  @@ T.merge_with (T.(Branch 
  (Branch (Leaf, ("a", 1), Leaf), 
  ("d", 2), 
  Branch (Leaf, ("f", 3), Leaf)))) 
  (T.(Branch 
  (Branch (Leaf, ("a", "y"), Leaf), 
  ("b", "a"), 
  Branch (Branch (Leaf, ("c", "y"), Leaf), ("e", "a"), Leaf))))
  ~merger:(fun a b -> 
    match a, b with
    | Some _, None -> 1
    | None, Some _ -> 1
    | Some _, Some _ -> 2
    | None, None -> 0
  )
;;

let section1_tests =
  "Section 1" >: test_list [
    "Flip" >:: test_flip;
    "Size"  >:: test_size;
    "List"  >:: test_list_of_btree;
    "Order" >:: test_is_ordered;
    "Well Formed" >:: test_well_formed;
    "Lookup" >:: test_lookup;
    "Map" >:: test_map;
    "Fold" >:: test_fold;
    "Max Value" >:: test_max_value;
    "Insert" >:: test_insert;
    "Merge" >:: test_merge;
    "Merge With" >:: test_merge_with;
  ]

let series =
  "Assignment2 Tests" >::: [
    section1_tests;
  ]

let () = 
  run_test_tt_main series

