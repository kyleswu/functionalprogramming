open Core
open OUnit2
open Submission

(* This file contains a few tests but not necessarily complete coverage.  You are
   encouraged to think of more tests if needed for the corner cases. 
   We will cover the details of the test syntax but with simple copy/paste it should 
   not be hard to add new tests of your own without knowing the details.
   1) Write a new let which performs the test, e.g. let test_fibonacci_2 = ...
   2) Add that let-named entity to one of the test suite lists such as section1_tests
      by adding e.g. 
       "Fibonacci 2"       >:: test_fibonacci_2;
   Thats it!

   Recall that you need to type "dune test" to your shell to run the test suite.

*)

let test_summate _ = 
  assert_equal (summate 3) 6;
  assert_equal (summate 10) 55;
  assert_equal (summate 0) 0;
  ;;

let test_lcm _ = 
  assert_equal (lcm 3 9) 9;
  assert_equal (lcm 9 12) 36;
  assert_equal (lcm 1 1) 1;
  assert_equal (lcm 7 13) 91;
  ;;

let test_fibonacci _ =
  assert_equal (fibonacci 0) 0;
  assert_equal (fibonacci 10) 55;
  assert_equal (fibonacci 50) 12586269025;
  ;;

let part1_section1_tests =
  "Part 1 Section 1" >: test_list [
    "Summate"   >:: test_summate;
    "LCM"       >:: test_lcm;
    "Fibonacci" >:: test_fibonacci;
  ]

let test_iota1 _ =
  assert_equal (iota1 5) [5; 4; 3; 2; 1];
  assert_equal (iota1 1) [1];
  assert_equal (iota1 0) [];
  ;;

let test_iota2 _ =
  assert_equal (iota2 6) [1; 2; 3; 4; 5; 6];
  assert_equal (iota2 1) [1];
  assert_equal (iota2 0) [];
  ;;

let test_factors _ =
  assert_equal (factors 10) [1; 2; 5; 10];
  assert_equal (factors 12) [1; 2; 3; 4; 6; 12];
  assert_equal (factors 1) [1];
  assert_equal (factors 7) [1; 7];
  ;;

let test_is_ordered _ =
  assert_equal (is_ordered ["a"; "b"; "c"]) true;
  assert_equal (is_ordered ["b"; "a"; "c"]) false;
  assert_equal (is_ordered []) true;
  ;;

let test_insert_string _ = 
  assert_equal (insert_string "FPSE" ["I"; "Love"]) ["FPSE"; "I"; "Love"];
  assert_equal (insert_string "baa" ["0"; "111"; "abb"; "abc"; "bcd"]) ["0"; "111"; "abb"; "abc"; "baa"; "bcd"];
  assert_equal (insert_string "a" []) ["a"];
  assert_equal (insert_string "z" ["a"]) ["a"; "z"];
  ;;

let test_insert_string_exn _ = 
  let f _ = insert_string_exn "Oops" ["Not"; "In"; "Order"; "!"] 
    in assert_raises (Invalid_argument "List not sorted") f;
  ;;

let test_insertion_sort _ = 
  assert_equal (insertion_sort ["bus"; "cat"; "apple"; "green"; "email"]) ["apple"; "bus"; "cat"; "email"; "green"];
  assert_equal (insertion_sort []) [];
  assert_equal (insertion_sort ["a"]) ["a"];
  ;;

let test_remove_max _ = 
  assert_equal (remove_max ["hi"; "remove me!"; "don't remove me"]) @@ Ok ("remove me!", ["hi"; "don't remove me"]);
  assert_equal (remove_max ["remove me!"; "hi"; "remove me!"; "don't remove me"]) @@ Ok ("remove me!", ["hi"; "remove me!"; "don't remove me"]);
  assert_equal (remove_max ["hi"; "don't remove me"; "remove me!"]) @@ Ok ("remove me!", ["hi"; "don't remove me"]);
  ;;

let test_max_sort _ = 
  assert_equal (max_sort ["bus"; "cat"; "apple"; "green"; "email"]) ["apple"; "bus"; "cat"; "email"; "green"];
  assert_equal (max_sort ["a"]) ["a"];
  ;;

let part1_section2_tests =
  "Part 1 Section 2" >: test_list [
    "Iota1"             >:: test_iota1;
    "Iota2"             >:: test_iota2;
    "Factors"           >:: test_factors;
    "Insert String"     >:: test_insert_string;
    "Insert String Exn" >:: test_insert_string_exn;
    "Insertion Sort"    >:: test_insertion_sort;
    "Remove Max"        >:: test_remove_max;
    "Max Sort"          >:: test_max_sort;
  ]

let test_student_record1: student_record =
  [
    Ok (Some 10.0);
    Ok (Some 10.0);
    Ok (None);
  ]

let test_student_record2: student_record =
  [
    Error "No such item.";
    Ok (Some 100.0);
    Ok (Some 100.0);
  ]

let test_mean_of_record _ =
  assert_equal (mean_of_record test_student_record1) (Some 10.0)
  ;;
  
let test_mean_of_record' _ =
  assert_equal (mean_of_record' test_student_record2) (100.0, ["No such item."])
  ;;


let part2_section1_tests = 
  "Part 2 Section 1" >: test_list [
    "Mean of Record"       >:: test_mean_of_record;
    "Mean of Record (alt)" >:: test_mean_of_record';
  ]

(* We leave this section to you *)
let test_iota1' _ =
  assert_equal (iota1' 5) [5; 4; 3; 2; 1];
  assert_equal (iota1' 1) [1];
  assert_equal (iota1' 0) [];
  ;;

let test_iota2' _ =
  assert_equal (iota2' 6) [1; 2; 3; 4; 5; 6];
  assert_equal (iota2' 1) [1];
  assert_equal (iota2' 0) [];
  ;;

let test_factors' _ =
  assert_equal (factors' 10) [1; 2; 5; 10];
  assert_equal (factors' 12) [1; 2; 3; 4; 6; 12];
  assert_equal (factors' 1) [1];
  assert_equal (factors' 7) [1; 7];
  ;;

let test_insert_string' _ = 
  assert_equal (insert_string' "FPSE" ["I"; "Love"]) ["FPSE"; "I"; "Love"];
  assert_equal (insert_string' "baa" ["0"; "111"; "abb"; "abc"; "bcd"]) ["0"; "111"; "abb"; "abc"; "baa"; "bcd"];
  assert_equal (insert_string' "a" []) ["a"];
  assert_equal (insert_string' "z" ["a"]) ["a"; "z"];
  ;;

let part2_section2_tests = 
  "Part 2 Section 2" >: test_list [
    "Iota1'"             >:: test_iota1';
    "Iota2'"             >:: test_iota2';
    "Factors'"           >:: test_factors';
    "Insert String'"     >:: test_insert_string';
  ]

let test_run_length_encode _ =
  assert_equal (run_length_encode [1; 1; 2; 3; 3; 3; 4; 1; 1])
    [(1, 2); (2, 1); (3, 3); (4, 1); (1, 2)]
  ;;

let test_run_length_decode _ =
  assert_equal (run_length_decode [(1, 2); (2, 1); (3, 3); (4, 1); (1, 2)])
    [1; 1; 2; 3; 3; 3; 4; 1; 1]
  ;;

let test_smallest_divisible_by _ =
  assert_equal (smallest_divisible_by [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]) 2520
  ;;

let test_is_pangram _ = 
  assert_equal (is_pangram "the quick brown fox jumps over the lazy dog") true;
  assert_equal (is_pangram "the quick brown fox slides under the lazy dog") false


let part2_section3_tests =
  "Part 2 Section 3" >: test_list [
    "Run-length encode"     >:: test_run_length_encode;
    "Run-length decode"     >:: test_run_length_decode;
    "Smallest divisible by" >:: test_smallest_divisible_by;
    "Is Pangram"            >:: test_is_pangram;
  ]


(* Uncomment part 2 tests when you finish implementing them *)
let series =
  "Assignment1 Tests" >::: [
    part1_section1_tests;
    part1_section2_tests;
    part2_section1_tests;
    part2_section2_tests;
    part2_section3_tests;
  ]


let () = run_test_tt_main series
