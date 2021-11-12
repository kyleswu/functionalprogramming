
open Core

(*
  In this assignment we will implement a simple n-gram model.
  We will also give you some template code to give you a basis
  for an elegant and general library for modeling n-grams.

  Part I consists of these library routines, and Part II
  will be to develop a command-line tool to build and use an 
  n-gram model which makes use of your library.

  Using the n-gram model of sequences and probabilities, we'll take in
  some sequence of items, and then use it as a basis to generate more similar
  sequences (for example, sentences of words, lists of numbers, etc.),
  or evaluate the likelihood of seeing particular sequences.

  First we need some helper functions and definitions.

*)

(* Note, Part I consists of the following 6 Exercises *)

(*
  Exercise 1:

  Given a list of some type, and a positive integer `n`,
  produce a list of contiguous subsequences of length `n`
  of the original list. If `n` is greater than the length
  of the input, return an empty list.

  E.G.
    chunks 3 [1; 2; 3; 4; 5] =
      [ [1;2;3]; [2;3;4]; [3;4;5] ]

    chunks 2 ["a"; "b"; "c"; "d"; "e"] =
      [ ["a";"b"]; ["b";"c"]; ["c";"d"]; ["d";"e"] ]

    chunks 3 [1; 2] = []
*)
let chunks (n: int) (l: 'a list): 'a list list =
  if n > List.length l then []
  else 
    let rec chunkify (n: int) (l: 'a list) (ret: 'a list list) =
      if n > List.length l then ret
      else match l with
      | _ :: remaining -> 
        let new_chunk = [List.hd_exn @@ List.groupi ~break:(fun i _ _ -> i = n) l] in
        chunkify n remaining (ret @ new_chunk)
      | [] -> failwith "should never happen" (* will never happen for bisect coverage *)
    in
    chunkify n l []
(*
  Exercise 2:

  Given a non-empty list of some type, return a pair
  of the last element, and a list of all previous elements.
  This should take O(n) time.

  E.G.
    split_last [1;2;3] = (3, [1;2])
*)
let split_last (l: 'a list): 'a * 'a list =
  match List.last l, List.drop_last l with
  | Some elt, Some list -> elt, list
  | _ , _ -> failwith "should be given non-empty list" (* will never happen for bisect coverage *)
(*
  Exercise 3:

  Here we will make a generic method for making map keys which are 
  lists of some underlying element. 

  Given a data type module Elt which can be used as the key for a map
  (Map.Key module type in Core), fill in the following functor to
  make a map key data type module for a *list* of Elt's.

*)
module List_key (Elt: Map.Key): (Map.Key with type t = Elt.t list) = struct
  type t = Elt.t list [@@deriving compare, sexp]
end

(*
  We will need randomness for this code, which can make things hard to test.
  To make it repeatable, we will abstract away the randomizer as another
  parameter, of module type `Randomness`.
  
  Note that the standard `Base.Random` module is compatible with this 
  module type, but you could also provide alternative definitions which 
  are deterministic, give more debug info, guarantee a certain sequence of numbers, log to stderr, etc.
*)
module type Randomness = sig
  (*
    Given a maximum integer value, return
    a pseudorandom integer from 0 (inclusive) to this value (exclusive).
  *)
  val int : int -> int
end

module Test_random = struct
  let int (n: int) = if n mod 2 = 0 then 0 else n - 1; 
end
(*
  Exercise 4:

  Given a multiset aka bag, select one element
  from it with uniform probability (i.e. so that elements which
  appear multiple times should have a higher chance to be picked).
  Or, if the bag is empty, return None.

  See the (weighted) reservoir sampling algorithms
  for a simple potential approach to this.
  
  Use `Core.Bag` for your bag. This operation should not be destructive 
  or mutate the bag, it should just extract a random element.
  Be aware that Core.Bag is a mutable data structure, in general.
  Also be aware of the `'a Bag.Elt.t` type, which signifies a particular
  element within the Bag (distinguishing it from other, possibly equal 
  elements).

  Several Bag functions take and return `'a Bag.Elt.t` values so look
  at the documentation for `Bag.Elt` to see how to e.g. extract the underlying value.

  Note that `Bag.choose` does *not* satisfy this definition;  it simply 
  picks the first element always.

  Note we use a shorthand notation for first-class modules not 
  covered in lecture: the parameter R here is a first-class module 
  which has already been unpacked from value-space to module-space.
  Similarly, sample can be invoked as e.g. `sample (module Random) ...`

*)
let sample (module R: Randomness) (b: 'a Bag.t): 'a option =
  if Bag.is_empty b then None
  else
    let l = Bag.to_list b in
    let length = List.length l in
    let rec pick_random (index: int) (reservoir: 'a): 'a option =
      if index = length then Some reservoir
      else if R.int (index + 1) < 1 then pick_random (index + 1) @@ List.nth_exn l index
      else pick_random (index + 1) reservoir
    in
    let init_reservoir = List.nth_exn l 0 in
    pick_random 1 init_reservoir
  


(* Exercise 5:

  Fill out the skeleton of the module N_grams below to make a common 
  probabilistic model of sequences, the n-gram model, also called the 
  Markov model.

  The general intuition is simple: if we want to be able to predict
  what comes next in a sequence of items, we can probably do so on 
  the basis of the elements which preceeded it. Moreover, we can 
  probably ignore parts of the sequence which came _far_ before the 
  element we want to predict, and focus our attention on the immediately 
  previous couple of items.

  Consider sentences of words in english text, a very common type of 
  sequence to apply this approach to. If we are given that the word we 
  want to predict came after:
  
  "take this boat for a spin out on the" ???

  Then we could say that "water" is more likely than "town" to follow.
  If we have less context, say only 2 words:

  "on the" ???

  We will naturally make a poorer approximation of the true distribution,
  but it may be sufficient for some purposes anyway, and will be easier 
  to estimate.  How can we estimate the actual distribution of words 
  efficiently, then?
  
  We will need to take in some observed sequence of words or tokens, 
  called a _corpus_.  Let's say we want to keep 2 words of context when 
  predicting what comes next, based on the provided corpus. Then we can 
  just keep track of every 3-tuple of consecutive words in the input,
  and count how often they appear.

  For example, say we observe the triples

  ("take", "this", "boat"), ("this", "boat", "for"), ... ("on", "the", "water").

  Then, if we index these properly, we can predict what should follow 
  ("on", "the") by just sampling randomly from among all the tuples which 
  started with that prefix, and using the last element of the tuple as 
  our prediction.  Naturally, words which appear more frequently in the 
  context specified should then be given more weight, and words which do 
  not appear in our corpus after the given sequence will not be chosen 
  at all, so our prediction should be a reasonable estimate for the
  empirical distribution.

  If we instead count 5-tuples rather than 3-tuples, we can make better
  predictions with the greater context, which will then more closely 
  match the true sequence properties. However, we will also be able to 
  observe fewer unique 5-tuples overall than 3-tuples, which will mean 
  we need greater amounts of data to properly use a larger n-gram size.


  Feel free to read these useful resources to better understand n-grams:
  - https://blog.xrds.acm.org/2017/10/introduction-n-grams-need/
  - https://web.stanford.edu/~jurafsky/slp3/slides/LM_4.pdf
  - https://medium.com/mti-technology/n-gram-language-model-b7c2fc322799

  
  First define a module which holds our main functionality
  specific to a particular orderable type we'll call `Token`. These
  tokens could be words (strings) of course, but could also be numbers, or
  DNA base pairs, etc.

  We also need randomness here, so we will abstract over it as well.
*)
module N_grams (Random: Randomness) (Token: Map.Key) = struct

  (*
    Define a module which is a Map satisfying the signature provided,
    so that sequences of tokens can be mapped to values.
  *)
  module Token_list_map : (Map.S with type Key.t = Token.t list) = Map.Make(List_key(Token))


  (*
    Based on how n-grams work, we will represent a probability distribution
    as mapping from prefixes of size `n`, to tokens which followed this prefix
    in our training corpus. The more times any particular token follows a 
    prefix, the more likely it is to follow it again.

    Don't change this type; it is a map from token lists to bags of tokens.
  *)
  type distribution = (Token.t Bag.t) Token_list_map.t

  (*
    Given a positive integer `n` and a list of tokens,
    add each token to a new distribution as an element of the set
    corresponding to the (n-1)-gram which preceeds it.

    e.g. (informally diagramming the map/bag of a `distribution`)

      ngrams 2 [1; 2; 3; 4; 4; 4; 2; 2; 3; 1] =
        { 
          [1] -> {2}; 
          [2] -> {3; 2; 3};
          [3] -> {4; 1};
          [4] -> {4; 4; 2};
            |        |
            |        \------- ...was followed by each of these elements
            \-- this sequence...
        }

      ngrams 3 [1; 2; 3; 4; 4; 4; 2; 2; 3; 1] =
        {
          [1; 2] -> {3};
          [2; 3] -> {4; 1};
          [3; 4] -> {4};
          [4; 4] -> {4; 2};
          [4; 2] -> {2};
            |        |
            |        \------- ...was followed by each of these elements
            \-- this sequence...
        }

      ngrams 1 [1; 2; 3; 4; 4; 4; 2; 2; 3; 1] =
        {
          [] -> {1; 2; 3; 4; 4; 4; 2; 2; 3; 1};
          |        |
          |        \------- ...was followed by each of these elements
          \-- this sequence...
        }
  *)
  let ngrams (n: int) (l: Token.t list): distribution =
    let processed_list = List.map ~f:(fun l -> split_last l) @@ chunks n l in
    List.fold processed_list ~init:Token_list_map.empty ~f:(fun init (value, key) -> 
      match Token_list_map.find init key with
      | Some bag -> 
        let (_ : 'a Bag.Elt.t) = Bag.add bag value in
        init
      | None -> 
        let new_bag = Bag.create () in
        let (_ : 'a Bag.Elt.t) = Bag.add new_bag value in
        Token_list_map.add_exn init ~key: key ~data: new_bag
      )


  (*

  Now, we can use the output of `ngrams` to create new, randomly 
    sampled sequences.

    The arguments it expects are as follows:
    - an output from a call to `ngrams n` (above) representing a distribution
    - an integer for the maximum length of sequence to generate
    - a list of tokens (of length `n-1`) to kick off the random generation 
      and sequence (consider it a 'seed' to look up in the distribution)

    It will then produce a sequence which is distributed according
    to the n-gram model it is given, terminating if either the resulting
    sequence reaches the maximum length, or when there are no observed
    n-grams which could follow.

  *)

  let sample_sequence (dist: distribution) ~(max_length: int) ~(initial_ngram: Token.t list): Token.t list =
    if max_length < List.length initial_ngram then
      List.take initial_ngram max_length
    else
    let rec add_next_token (ngram: Token.t list) (ret_list: Token.t list): Token.t list =
      if List.length ret_list = max_length then ret_list
      else 
        match Token_list_map.find dist ngram with
        | None -> ret_list
        | Some bag ->
          let sample_val = sample (module Random) bag in
          let next_token = [Option.value_exn sample_val] in
          if List.length ngram = 0 then add_next_token ngram (ret_list @ next_token)
          else
          let next_ngram = List.tl_exn ngram @ next_token in
          add_next_token next_ngram (ret_list @ next_token)
    in
    add_next_token initial_ngram initial_ngram
end (* of module N_grams *)

(*
  Exercise 6:

  Given a string, perform basic sanitization/normalization
  by taking the following steps:

  - remove all characters not in the range [a-zA-Z0-9]
  - convert all characters [A-Z] to lowercase

  if the resulting string is empty, return None.

*)
let sanitize (s: string): string option =
  let cleanup (s: string): string =
    String.lowercase s
    |> String.to_list
    |> List.filter ~f:(fun c -> 
      (Char.(>=) c 'a' && Char.(<=) c 'z') ||
      (Char.(>=) c '0' && Char.(<=) c '9'))
    |> String.of_char_list
  in
  let ret_string = cleanup s in
  if String.is_empty ret_string then None
  else Some ret_string

(* See ngrams.ml for part II.

For any auxiliary functions needed in part II, put them here so they can
be unit tested.  

*)

let rand_initial (module R: Randomness) (sl: string list) (n: int): string list =
  let chunked_list = chunks n sl in
  let rand_ngram = split_last @@ List.nth_exn chunked_list (R.int @@ List.length chunked_list) in
  let _, init_ngram = rand_ngram in
init_ngram

let get_initial_n_minus_one (sl: string list) (n: int): (string list * string list) =
  let front, n_minus_one = List.split_n sl (List.length sl - n + 1) in
  front, n_minus_one

module Freq_map = Map.Make (struct
  type t = string list [@@deriving compare, sexp]
end)

let create_freq_map (chunks: string list list) : int Freq_map.t =
  let map = Freq_map.empty in
  List.fold chunks ~init:map ~f:(fun m chunk ->
    match Freq_map.find m chunk with
    | Some freq -> Freq_map.set m ~key:chunk ~data:(freq + 1)
    | None -> Freq_map.add_exn m ~key:chunk ~data:1
    )

type entry_pair = { ngram: string list; frequency: int } [@@deriving yojson]

type entry_list = entry_pair list [@@deriving yojson] 

let find_freq_list (size: int) (map: int Freq_map.t) : entry_list =
  Freq_map.to_alist ~key_order:`Increasing map
  |> fun list -> List.sort list ~compare:(fun (key1, val1) (key2, val2) ->
    let cmp_val = compare val2 val1 in
    if cmp_val <> 0 then cmp_val
    else List.compare (fun s1 s2 -> String.compare s1 s2) key1 key2)
  |> fun list -> List.take list size
  |> fun list -> List.fold list ~init:[] ~f:(fun e_list (ngram, frequency) -> { ngram; frequency } :: e_list)
  |> List.rev
