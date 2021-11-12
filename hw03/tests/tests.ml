(*
  Part II: Tests
 
  In this part, you will need to create and run your own tests.  Tests should
  cover both common cases and edge cases.  In previous assignments, we only
  asked for a specified number of additional tests, but in this assignment we
  will be grading based on code coverage.
 
  Aim for complete code coverage on all functions, and we will check 
  by running the bisect tool on your code.  For that reason, you need 
  to add the following line in the dune file for your library:
      
      (preprocess (pps bisect_ppx))
 
  or else your tests will not run in the autograder.

 Additionally, you will need to write a special suite of tests here which
 verifies some invariants.  See the assignment for details.
 
*)

open Core;;
open OUnit2;;

module Int_Dict = Abstraction.Dict (Int);;

let final_dict = 
  Int_Dict.insert 2 30 @@ 
  Int_Dict.insert 3 20 @@ 
  Int_Dict.insert 1 10 Int_Dict.empty
;;

let test_lookup _ =
  assert_equal (Some 10) @@ Int_Dict.lookup 1 final_dict;
  assert_equal (Some 20) @@ Int_Dict.lookup 3 final_dict;
  assert_equal (Some 30) @@ Int_Dict.lookup 2 final_dict;
  assert_equal (None) @@ Int_Dict.lookup 10 final_dict
;;

let test_map _ = 
  let test_dict =
    Int_Dict.insert 2 "abcdefghijklmnopqrstuvwxyzabcd" @@ 
    Int_Dict.insert 3 "abcdefghijklmnopqrst" @@ 
    Int_Dict.insert 1 "abcdefghij" Int_Dict.empty
  in
  assert_equal final_dict @@ Int_Dict.map ~f:(fun s -> String.length s) test_dict
;;

let test_insert _ =
  let test_dict =
    Int_Dict.insert 2 30 @@ 
    Int_Dict.insert 3 0 @@ 
    Int_Dict.insert 1 10 Int_Dict.empty
  in
  assert_equal final_dict @@ Int_Dict.insert 3 20 test_dict
;;

let test_remove _ =
  let test_dict1 =
    Int_Dict.insert 4 40 @@
    Int_Dict.insert 2 30 @@ 
    Int_Dict.insert 3 20 @@ 
    Int_Dict.insert 1 10 Int_Dict.empty
  in
  let test_dict2 =
    Int_Dict.insert 4 40 @@
    Int_Dict.insert 2 30 @@ 
    Int_Dict.insert 5 20 @@ 
    Int_Dict.insert 1 10 Int_Dict.empty
  in
  let test_dict2' =
    Int_Dict.insert 4 40 @@
    Int_Dict.insert 2 30 @@  
    Int_Dict.insert 1 10 Int_Dict.empty
  in
  let test_dict3 =
    Int_Dict.insert 4 20 @@
    Int_Dict.insert 1 40 @@
    Int_Dict.insert 3 40 @@
    Int_Dict.insert 6 40 @@
    Int_Dict.insert 2 30 @@  
    Int_Dict.insert 5 10 Int_Dict.empty
  in
  let test_dict3' =
  Int_Dict.insert 4 20 @@
    Int_Dict.insert 3 40 @@
    Int_Dict.insert 1 40 @@
    Int_Dict.insert 6 40 @@ 
    Int_Dict.insert 5 10 Int_Dict.empty
  in
  let test_dict4 =
    Int_Dict.insert 3 20 @@
    Int_Dict.insert 5 40 @@
    Int_Dict.insert 4 40 @@
    Int_Dict.insert 6 40 @@
    Int_Dict.insert 1 30 @@  
    Int_Dict.insert 2 10 Int_Dict.empty
  in
  let test_dict4' =
    Int_Dict.insert 3 20 @@
    Int_Dict.insert 5 40 @@
    Int_Dict.insert 4 40 @@
    Int_Dict.insert 1 30 @@  
    Int_Dict.insert 2 10 Int_Dict.empty
  in
  assert_equal (final_dict, Some(40)) @@ Int_Dict.remove 4 test_dict1;
  assert_equal (test_dict2', Some(20)) @@ Int_Dict.remove 5 test_dict2;
  assert_equal (test_dict3', Some(30)) @@ Int_Dict.remove 2 test_dict3;
  assert_equal (test_dict4', Some(40)) @@ Int_Dict.remove 6 test_dict4;
  assert_equal (final_dict, None) @@ Int_Dict.remove 4 final_dict
;;

let test_merge _ =
  let test_dict1 = 
    Int_Dict.insert 3 0 @@ 
    Int_Dict.insert 1 10 Int_Dict.empty
  in
  let test_dict2 =
    Int_Dict.insert 2 30 @@ 
    Int_Dict.insert 3 20 Int_Dict.empty
  in
  assert_equal final_dict @@ Int_Dict.merge test_dict1 test_dict2
;;

let test_fib _ =
  assert_equal 0 @@ Abstraction.memoized_fib 0;
  assert_equal 1 @@ Abstraction.memoized_fib 1;
  assert_equal 1 @@ Abstraction.memoized_fib 2;
  assert_equal 2 @@ Abstraction.memoized_fib 3;
  assert_equal 3 @@ Abstraction.memoized_fib 4;
  assert_equal 139583862445 @@ Abstraction.memoized_fib 55
;;

let test_fib' _ =
  assert_equal 0 @@ Abstraction.memoized_fib' 0;
  assert_equal 1 @@ Abstraction.memoized_fib' 1;
  assert_equal 1 @@ Abstraction.memoized_fib' 2;
  assert_equal 2 @@ Abstraction.memoized_fib' 3;
  assert_equal 3 @@ Abstraction.memoized_fib' 4;
  assert_equal 139583862445 @@ Abstraction.memoized_fib' 55
;;

let test_fib'' _ =
  assert_equal 0 @@ Abstraction.memoized_fib'' 0;
  assert_equal 1 @@ Abstraction.memoized_fib'' 1;
  assert_equal 1 @@ Abstraction.memoized_fib'' 2;
  assert_equal 2 @@ Abstraction.memoized_fib'' 3;
  assert_equal 3 @@ Abstraction.memoized_fib'' 4;
  assert_equal 139583862445 @@ Abstraction.memoized_fib'' 55
;;

let unwrap_ok data =
  match data with
  | Ok d -> d
  | Error msg -> failwith msg
;;

let unwrap_some data =
  match data with
  | Some x -> x
  | None -> failwith "unwrap some error"

let test_int_from_string _ =
  assert_equal 5 @@ Abstraction.Int_Data.from_string "5";
  assert_equal (-5) @@ Abstraction.Int_Data.from_string "-5"
;;

let test_int_to_string _ =
  assert_equal "5" @@ Abstraction.Int_Data.to_string 5;
  assert_equal "-5" @@ Abstraction.Int_Data.to_string (-5)
;;

let test_int_next _ =
  assert_equal None @@ Abstraction.Int_Data.next "-";
  assert_equal None @@ Abstraction.Int_Data.next "-a";
  assert_equal ("", (-1)) @@ unwrap_some (Abstraction.Int_Data.next "-1");
  assert_equal ("", 12) @@ unwrap_some (Abstraction.Int_Data.next "12");
  assert_equal ("", 5) @@ unwrap_some (Abstraction.Int_Data.next "5");
  assert_equal ("3 +", 2) @@ unwrap_some (Abstraction.Int_Data.next "2 3 +");
  assert_equal ("+", 3) @@ unwrap_some (Abstraction.Int_Data.next "3 +");
  assert_equal None @@ Abstraction.Int_Data.next "+";
  assert_equal ("12 + 3 0 + *", 2) @@ unwrap_some(Abstraction.Int_Data.next "2 12 + 3 0 + *");
  assert_equal ("+ 3 0 + *", 12) @@ unwrap_some(Abstraction.Int_Data.next "12 + 3 0 + *");
  assert_equal None @@ Abstraction.Int_Data.next "+ 3 0 + *";
  assert_equal ("0 + *", 3) @@ unwrap_some(Abstraction.Int_Data.next "3 0 + *");;

let test_int_plus _ =
  assert_equal 5 @@ Abstraction.Int_Data.plus 2 3;
  assert_equal (-5) @@ Abstraction.Int_Data.plus (-2) (-3)
;;

let test_int_times _ =
  assert_equal 6 @@ Abstraction.Int_Data.times 2 3;
  assert_equal (-6) @@ Abstraction.Int_Data.times 2 (-3);
  assert_equal 6 @@ Abstraction.Int_Data.times (-2) (-3)
;;

let test_int_eval _ =
  assert_equal 5 (unwrap_ok (Abstraction.Int_Eval.eval "5"));
  assert_equal 5 (unwrap_ok (Abstraction.Int_Eval.eval "2 3 +"));
  assert_equal 10 (unwrap_ok (Abstraction.Int_Eval.eval "2 3 + 2 + 3 +"));
  assert_equal 42 (unwrap_ok (Abstraction.Int_Eval.eval "2 12 + 3 0 + *"));
  assert_equal 0 (unwrap_ok (Abstraction.Int_Eval.eval "1-1+"))
  let _ = match Abstraction.Int_Eval.eval "" with 
  | Error msg -> assert_equal msg "unmatched" 
  | _ -> assert_equal 1 (-1)
  in
  let _ = match Abstraction.Int_Eval.eval "1 +" with 
  | Error msg -> assert_equal msg "unmatched" 
  | _ -> assert_equal 1 (-1)
  in
  let _ = match Abstraction.Int_Eval.eval "1 *" with 
  | Error msg -> assert_equal msg "unmatched" 
  | _ -> assert_equal 1 (-1)
  in
  let _ = match Abstraction.Int_Eval.eval "a" with 
  | Error msg -> assert_equal msg "illegal character" 
  | _ -> assert_equal 1 (-1)
  in
  let _ = match Abstraction.Int_Eval.eval "" with 
  | Error msg -> assert_equal msg "unmatched" 
  | _ -> assert_equal 1 (-1)
  in
  assert_equal (-4) (unwrap_ok (Abstraction.Int_Eval.eval "-2-2+"));
;;

let test_rat_from_string _ =
  assert_equal (3,2) @@ Abstraction.Rat_Data.from_string "3/2";
  assert_equal (-3,2) @@ Abstraction.Rat_Data.from_string "-3/2";
;;

let test_rat_to_string _ =
  assert_equal "3/2" @@ Abstraction.Rat_Data.to_string (3, 2);
  assert_equal "-3/2" @@ Abstraction.Rat_Data.to_string(-3, 2)
;;

let test_rat_next _ =
  assert_equal ("", (-1, 1)) @@ unwrap_some (Abstraction.Rat_Data.next "-1/1");
  assert_equal ("", (1, 1)) @@ unwrap_some (Abstraction.Rat_Data.next "2/2");
  assert_equal None @@ Abstraction.Rat_Data.next "1/-1";
  assert_equal None @@ Abstraction.Rat_Data.next "1/0";
  assert_equal None @@ Abstraction.Rat_Data.next "10";
  assert_equal None @@ Abstraction.Rat_Data.next "-1";
  assert_equal None @@ Abstraction.Rat_Data.next "1/";
  assert_equal None @@ Abstraction.Rat_Data.next "-a/12";
;;

let test_rat_plus _ =
  assert_equal (7, 3) @@ Abstraction.Rat_Data.plus (7, 6) (7, 6);
  assert_equal (-7, 3) @@ Abstraction.Rat_Data.plus (-7, 6) (-7, 6);
  assert_equal (5, 2) @@ Abstraction.Rat_Data.plus (2, 1) (1, 2);
;;

let test_rat_times _ =
  assert_equal (7, 6) @@ Abstraction.Rat_Data.times (14, 2) (1, 6);;

let test_rat_eval _ =
  assert_equal (5, 2) (unwrap_ok (Abstraction.Rat_Eval.eval "5/2"));
  assert_equal (5, 2) (unwrap_ok (Abstraction.Rat_Eval.eval "2/1 1/2 +"));
  assert_equal (5, 1) (unwrap_ok (Abstraction.Rat_Eval.eval "2/1 1/2 + 2/1 + 1/2 +"));
  let _ = match Abstraction.Rat_Eval.eval "1" with 
  | Error msg -> assert_equal msg "illegal character" 
  | _ -> assert_equal 1 (-1)
  in
  assert_equal (42, 1) (unwrap_ok (Abstraction.Rat_Eval.eval "2/1 12/1 + 3/1 0/1 + *"));
;;

let invariant_insert k v d =
  assert_equal true @@ Int_Dict.proper_format @@ Int_Dict.insert k v d
;;

let test_dict_insert_invariant _ =
  let test_dict1 =
    Int_Dict.insert 5 10 Int_Dict.empty
  in
  invariant_insert 7 30 test_dict1;
  invariant_insert 2 0 test_dict1;
  let test_dict2 =
    Int_Dict.insert 7 30 @@ 
    Int_Dict.insert 2 0 @@ 
    Int_Dict.insert 5 10 Int_Dict.empty
  in
  invariant_insert 1 20 test_dict2;
  invariant_insert 4 20 test_dict2;
  invariant_insert 6 20 test_dict2;
  invariant_insert 8 20 test_dict2
;;

let invariant_remove k d =
  let d, _ = Int_Dict.remove k d in
  assert_equal true @@ Int_Dict.proper_format d
;;

let test_dict_remove_invariant _ =
  let test_dict =
    Int_Dict.insert 1 20 @@
    Int_Dict.insert 4 20 @@
    Int_Dict.insert 6 20 @@
    Int_Dict.insert 8 20 @@
    Int_Dict.insert 7 30 @@ 
    Int_Dict.insert 2 0 @@ 
    Int_Dict.insert 5 10 Int_Dict.empty
  in
  invariant_remove 1 test_dict;
  invariant_remove 2 test_dict;
  invariant_remove 4 test_dict;
  invariant_remove 5 test_dict;
  invariant_remove 6 test_dict;
  invariant_remove 7 test_dict;
  invariant_remove 8 test_dict;
;;

let invariant_lookup k v d =
  assert_equal v @@ unwrap_some @@ Int_Dict.lookup k @@ Int_Dict.insert k v d
;;

let test_dict_lookup_invariant _ =
  invariant_lookup 1 1 Int_Dict.empty;
  let test_dict = Int_Dict.insert 2 2 Int_Dict.empty in
  invariant_lookup 1 1 test_dict;
  invariant_lookup 3 3 test_dict
;;

let invariant_memoed n =
  let _ = Abstraction.memoized_fib n in
  assert_equal true @@ Option.is_some @@ Abstraction.Memo_Dict.lookup n !Abstraction.memo
;;

let test_memo_invariant _ =
  invariant_memoed 1;
  invariant_memoed 5;
  invariant_memoed 100
;;

let invariant_memoed' n =
  let _ = Abstraction.memoized_fib' n in
  assert_equal true @@ Option.is_some @@ Abstraction.Memo_Dict'.find !Abstraction.memo' n
;;

let test_memo'_invariant _ =
  invariant_memoed' 1;
  invariant_memoed' 5;
  invariant_memoed' 100
;;

let invariant_memoed'' n =
  let _ = Abstraction.memoized_fib'' n in
  assert_equal true @@ Option.is_some @@ Abstraction.Memo_Dict''.find Abstraction.memo'' n
;;

let test_memo''_invariant _ =
  invariant_memoed'' 1;
  invariant_memoed'' 5;
  invariant_memoed'' 100
;;



let part1_tests =
  "Dict Tests" >: test_list [
    "Lookup" >:: test_lookup;
    "Map" >:: test_map;
    "Insert" >:: test_insert;
    "Remove" >:: test_remove;
    "Merge" >:: test_merge;
    "Fib" >:: test_fib;
    "Fib'" >:: test_fib';
    "Fib''" >:: test_fib'';
  ]

let part2_tests =
  "Eval Tests" >: test_list [
    "Int From String" >:: test_int_from_string;
    "Int To String" >:: test_int_to_string;
    "Int Next" >:: test_int_next;
    "Int Plus" >:: test_int_plus;
    "Int Times" >:: test_int_times;
    "Int Eval" >:: test_int_eval;
    "Rat From String" >:: test_rat_from_string;
    "Rat To String" >:: test_rat_to_string;
    "Rat Next" >:: test_rat_next;
    "Rat Plus" >:: test_rat_plus;
    "Rat Times" >:: test_rat_times;
    "Rat Eval" >:: test_rat_eval;
  ]

let invariant_check =
  "Invariant Checking" >: test_list [
    "Invariant Insert" >:: test_dict_insert_invariant;
    "Invariant Remove" >:: test_dict_remove_invariant;
    "Invariant Lookup" >:: test_dict_lookup_invariant;
    "Invariant Memo Fib" >:: test_memo_invariant;
    "Invariant Memo' Fib" >:: test_memo'_invariant;
    "Invariant Memo'' Fib" >:: test_memo''_invariant;
  ]

let series =
  "Assignment3 Tests" >::: [
    part1_tests;
    part2_tests;
    invariant_check;
  ]

let () =
  run_test_tt_main series