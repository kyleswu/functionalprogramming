open Core;;
open OUnit2;;

let test_chunks _ =
  assert_equal [ [1;2;3]; [2;3;4]; [3;4;5] ] @@ Lib.chunks 3 [1; 2; 3; 4; 5];
  assert_equal [ ["a";"b"]; ["b";"c"]; ["c";"d"]; ["d";"e"] ] @@ Lib.chunks 2 ["a"; "b"; "c"; "d"; "e"];
  assert_equal [] @@ Lib.chunks 3 [1; 2]
;;

let test_split_last _ =
  assert_equal (3, [1;2]) @@ Lib.split_last [1;2;3];
  assert_equal (3, []) @@ Lib.split_last [3]
;;

let test_sample _ =
  let b = Bag.create () in
  assert_equal None @@ Lib.sample (module Lib.Test_random) b;
  let (_: 'a Bag.Elt.t) = Bag.add b 1 in
  assert_equal 1 @@ Option.value_exn (Lib.sample (module Lib.Test_random) b);
  let (_: 'a Bag.Elt.t) = Bag.add b 2 in
  assert_equal 1 @@ Option.value_exn (Lib.sample (module Lib.Test_random) b);
  let (_: 'a Bag.Elt.t) = Bag.add b 3 in
  assert_equal 2 @@ Option.value_exn (Lib.sample (module Lib.Test_random) b);
  let (_: 'a Bag.Elt.t) = Bag.add b 4 in
  assert_equal 1 @@ Option.value_exn (Lib.sample (module Lib.Test_random) b)
;;

module Int_ngram = Lib.N_grams (Lib.Test_random) (Int);;

let d1 = Int_ngram.ngrams 2 [1; 2; 3; 4; 4; 4; 2; 2; 3; 1];;

let d2 = Int_ngram.ngrams 3 [1; 2; 3; 4; 4; 4; 2; 2; 3; 1];;

let d3 = Int_ngram.ngrams 1 [1; 2; 3; 4; 4; 4; 2; 2; 3; 1];;

let d4 = Int_ngram.ngrams 3 [1; 1; 2];;

let get_bag (dist: Int_ngram.distribution) (tok_list: int list): int list =
  Bag.to_list @@ Option.value_exn (Int_ngram.Token_list_map.find dist tok_list)
;;

let test_ngrams _ =
  assert_equal [2] @@ get_bag d1 [1];
  assert_equal [3; 2; 3] @@ get_bag d1 [2];
  assert_equal [1; 4] @@ get_bag d1 [3];
  assert_equal [2; 4; 4] @@ get_bag d1 [4];

  assert_equal [3] @@ get_bag d2 [1; 2];
  assert_equal [1; 4] @@ get_bag d2 [2; 3];
  assert_equal [4] @@ get_bag d2 [3; 4];
  assert_equal [2; 4] @@ get_bag d2 [4; 4];
  assert_equal [2] @@ get_bag d2 [4; 2];
  assert_equal [3] @@ get_bag d2 [2; 2];

  assert_equal [1; 3; 2; 2; 4; 4; 4; 3; 2; 1] @@ get_bag d3 [];

  assert_equal [2] @@ get_bag d4 [1; 1]
;;

let test_sample_sequence _ =
  assert_equal [1; 2; 2; 2; 2] @@ Int_ngram.sample_sequence d1 ~max_length:5 ~initial_ngram:[1];
  assert_equal [] @@ Int_ngram.sample_sequence d1 ~max_length:0 ~initial_ngram:[1];
  
  assert_equal [2; 3; 4; 4; 4; 4; 4] @@ Int_ngram.sample_sequence d2 ~max_length:7 ~initial_ngram:[2; 3];
  assert_equal [2] @@ Int_ngram.sample_sequence d2 ~max_length:1 ~initial_ngram:[2; 3];

  assert_equal [1; 1; 1; 1; 1] @@ Int_ngram.sample_sequence d3 ~max_length:5 ~initial_ngram:[];

  assert_equal [1; 1; 2] @@ Int_ngram.sample_sequence d4 ~max_length:99 ~initial_ngram:[1; 1]
;;

let test_sanitize _ =
  assert_equal "abyz01289" @@ Option.value_exn (Lib.sanitize "!a@b Y Z0 1@(2()8)))))9");
  assert_equal None @@ Lib.sanitize "!)(#*$)(#*$)!(#*$)  *#$)(*#)(@$"
;;

let is_valid_sanitize (s: string): bool =
  String.to_list s
  |> List.for_all ~f:(fun c -> 
    (Char.(>=) c 'a' && Char.(<=) c 'z') ||
    (Char.(>=) c '0' && Char.(<=) c '9'))

let quickcheck_sanitize _ =
  let check_sanitize sanitize_fun = 
    Quickcheck.test 
      (String.quickcheck_generator)
      ~f:(fun s -> 
        match sanitize_fun s with
        | Some s -> assert_equal true @@ is_valid_sanitize @@ Option.value_exn(sanitize_fun s)
        | None -> assert_equal 1 1
      )
  in
  check_sanitize Lib.sanitize
;;

let test_rand_initial _ =
  assert_equal ["return"; "this"] @@ Lib.rand_initial (module Lib.Test_random) ["hello"; "my"; "return"; "this"; "kyle"] 3;
  assert_equal ["return"] @@ Lib.rand_initial (module Lib.Test_random) ["return"; "my"; "name"; "is"; "kyle"] 2
;;

let test_get_initial_n_minus_one _ =
  assert_equal (["this"; "is"; "the"; "front"], ["this"; "is"; "the"; "back"]) 
    @@ Lib.get_initial_n_minus_one ["this"; "is"; "the"; "front"; "this"; "is"; "the"; "back"] 5
;;

let test_map1 = Lib.create_freq_map [ ["a"; "b"]; ["b"; "a"]; ["a"; "b"]; ["a"; "c"]; ["a"; "c"]; ["b"; "a"]; ["z"; "z"] ];;

let test_create_freq_map _ =
  assert_equal 2 @@ Option.value_exn (Lib.Freq_map.find test_map1 ["a"; "b"]);
  assert_equal 2 @@ Option.value_exn (Lib.Freq_map.find test_map1 ["a"; "c"]);
  assert_equal 2 @@ Option.value_exn (Lib.Freq_map.find test_map1 ["b"; "a"]);
  assert_equal 1 @@ Option.value_exn (Lib.Freq_map.find test_map1 ["z"; "z"])
;;

let test_find_freq_list _ =
  assert_equal [] @@ Lib.find_freq_list 0 test_map1;
  assert_equal [{Lib.ngram = ["a"; "b"]; frequency = 2}] @@ Lib.find_freq_list 1 test_map1;
  assert_equal [{Lib.ngram = ["a"; "b"]; frequency = 2};
                {Lib.ngram = ["a"; "c"]; frequency = 2}] @@ Lib.find_freq_list 2 test_map1;
  assert_equal [{Lib.ngram = ["a"; "b"]; frequency = 2};
                {Lib.ngram = ["a"; "c"]; frequency = 2};
                {Lib.ngram = ["b"; "a"]; frequency = 2}] @@ Lib.find_freq_list 3 test_map1;
  assert_equal [{Lib.ngram = ["a"; "b"]; frequency = 2};
                {Lib.ngram = ["a"; "c"]; frequency = 2};
                {Lib.ngram = ["b"; "a"]; frequency = 2};
                {Lib.ngram = ["z"; "z"]; frequency = 1}] @@ Lib.find_freq_list 4 test_map1
;;

let part1_tests =
  "Part1 Tests" >: test_list [
    "Chunks" >:: test_chunks;
    "Split Last" >:: test_split_last;
    "Sample" >:: test_sample;
    "Ngrams" >:: test_ngrams;
    "Sample Sequence" >:: test_sample_sequence;
    "Sanitize" >:: test_sanitize;
  ]

let part2_tests =
  "Part2 Tests" >: test_list [
    "Random Initial" >:: test_rand_initial;
    "Get Initial N - 1" >:: test_get_initial_n_minus_one;
    "Create Freq Map" >:: test_create_freq_map;
    "Find Freq List" >:: test_find_freq_list;
    "Quickcheck Sanitize" >:: quickcheck_sanitize;
  ]

let series =
  "Assignment4 Tests" >::: [
    part1_tests;
    part2_tests;
  ]

let () =
  run_test_tt_main series