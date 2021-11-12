(*

FPSE Assignment 1
 
Name                  : Kyle Wu kwu45@jh.edu
List of Collaborators :

Please make a good faith effort at listing people you discussed any
problems with here, as per the course academic integrity policy.  
CAs/Prof need not be listed!

Fill in the function definitions below replacing the

  unimplemented ()

with your code.  Feel free to add "rec" to any function listed to make
it recursive. In some cases, you will find it helpful to define
auxillary functions, feel free to.

You must not use any mutation operations of OCaml for any of these
questions (which we have not taught yet in any case): no arrays,
for- or while-loops, references, etc.

*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(* You are required to use the Core libraries, don't remove the following line.
   If the editor is not recognizing Core (red squiggle under it for example),
   run a "dune build" from the shell -- the first time you build it will create some
   .merlin files which tells the editor where the libraries are.
*)
open Core

(* Here is a simple function which gets passed unit, (), as argument
   and raises an exception.  It is the initial implementation below. *)

let unimplemented () =
	failwith "unimplemented"
	
(*
	Part I Section 1: simple numeric recursions.
	
	All functions must be total for the specified domain;
	overflow is excluded from this restriction but should be avoided.
	
*)

(*
	Given a non-negative integer `n`, compute `0+1+2+ ... +n` using recursion
	(don't use the closed-form solution, do the actual addition).
*)
let rec summate (n: int): int =
	if n = 0 then 0
	else n + summate(n - 1)
	;;

(*
	Given non-negative integers `n` and `m`, compute their least common multiple.
*)
let lcm (n: int) (m: int): int =
	let int_mod (x: int) (y: int): int =
		x mod y
	in
	let rec gcd (a: int) (b: int): int =
		if a = 0 then b
		else if b = 0 then a
		else if a > b then gcd b (int_mod a b) 
		else if b >= a then gcd a (int_mod b a)
		else 0 (* never reached *)
	in
	m * n / gcd m n
	;;

(*
	Given a non-negative integer `n`, compute the n-th fibonacci number.
	Give an implementation that does not take exponential time; the naive 
	version from lecture is exponential	since it has two recursive calls for each call.
*)
let fibonacci (n: int): int =
	let rec fib (i: int) (prev: int) (curr: int): int =
		if n = i then prev
		else fib (i + 1) curr (prev + curr) 
	in
	fib 0 0 1
	;;

(*
	Part I Section 2: building lists. The List module functions may NOT be used (yet).
*)
	
(*
	Given a non-negative integer `n`, produce a list [n; n-1; ...; 2; 1].
*)
let rec iota1 (n: int): int list =
	if n = 0 then []
	else n :: iota1(n - 1)
	;;
(*
	Given a non-negative integer `n`, produce a list [1; 2; ...; n-1; n],
	without taking O(n^2) time.
*)
let iota2 (n: int): int list =
	let rec iotaUp (i: int): int list = 
		if i > n then []
		else i :: iotaUp(i + 1)
	in
	iotaUp 1
	;;

(*
	Given a positive integer `n`, produce the list of integers in the range (0, n]
	which it is divisible by, in ascending order.
*)
let factors (n: int): int list =
	let int_mod (x: int) (y: int): int =
		x mod y
	in
	let rec factor_list (i: int): int list =
		if i > n then []
		else if int_mod n i = 0 then i :: factor_list(i + 1)
		else factor_list(i + 1)
	in
	factor_list 1
	;;

(*
	Part I Section 3: strings, lists, and sorting.  The List module functions 
	cannot be used.	String comparisons operations such as String.(<=) can be used, but
	no other String module functions.
*)

(*
	Given a list of strings, check to see if it is ordered, i.e. whether earlier 
	elements are less than or equal to later elements.
*)
let rec is_ordered (ls: string list): bool =
	match ls with
	| [] -> true
	| x :: [] -> true
	| x :: y :: xs -> if String.(>) x y then false else is_ordered (y :: xs)
;;

(*
	Given a string and an ordered list of strings, insert the string into the list so
	that the list remains ordered.  Return the list with the string inserted.

	Note this is an example of a *functional data structure*, instead of mutating
	you return a fresh copy with the element added.
*)
let rec insert_string (s: string) (l: string list): string list =
	match l with
	| [] -> s :: []
	| x :: xs -> if String.(<) s x then s :: x :: xs else x :: insert_string s xs
;;

(*
	Define a variation on the previous function which before inserting the element verifies 
	that the input list is indeed sorted.  Use the built-in `Base` function invalid_arg
	which will raise an exception
	Note that invalid_arg is a function that raises the Invalid_argument exception

	Important: Your invalid_arg function must take this exact string (without quotes):
	"List not sorted" 

	e.g. invalid_arg "List not sorted"

	This is because OUnit2 tests check not only for exception type but also the message.
	If you don't have the same error message as us, the autograder will fail you. 
*)
let insert_string_exn (s: string) (l: string list): string list =
	if is_ordered l then insert_string s l
	else invalid_arg "List not sorted"
;;

(*
	Define a function to sort a list of strings by a functional version of the 
	insertion sort method: repeatedly invoke insert_string to add elements one by one 
	to an initially empty list.
*)
let rec insertion_sort (l: string list): string list =
	match l with 
	| [] -> []
	| x :: xs -> insert_string x (insertion_sort xs)
;;


(*
	Define a function to remove the lexicographically maximum string in a list of strings.
	Return
	 Error("empty list") if the input list is empty (and has no max)
	 OK(s,s_list) for s the maximum string and s_list the list with s removed, 
	  if the list is not empty.
*)
let remove_max (l: string list): (string * string list, string) result = (* removing first duplicate - campuswire said arbitrary and could do either *)
	let rec find_max (ls: string list) (max: string): string =
		match ls with
		| [] -> max
		| x :: xs -> if String.(>) x max then find_max xs x else find_max xs max
	in
	let rec max_destroyer (lss: string list) (front: string list) (max: string): (string * string list, string) result =
		match lss with
		| [] -> Error("empty list") (* never reached *)
		| x :: xs -> if String.(=) x max then Ok (max, (front @ xs)) else max_destroyer xs (front @ [x]) max
	in
	match l with
	| [] -> Error("empty list")
	| x :: xs -> max_destroyer l [] (find_max l x)
;;


(*
	Write a sort routine by repeated invocations of remove_max to pull out the largest
	elements one-by-one.  You should never need to invoke `remove_max` on an empty
	list, and you can thus `assert false` (an invariant failure) if the `Error`
	case is ever returned from `remove_max`.  
	This problem shows how we can manually encode the exceptional condition in `
	remove_max` with `Ok`/`Error` but convert it to an actual side effect here
	(the `assert false` will raise an exception if hit).
*)
let max_sort (l: string list): string list =
	let rec append_max (ls: string list) (ret_ls: string list): string list =  
		match remove_max ls with
		| Error s -> assert false
		| Ok (x, xs) -> match xs with 
			| [] -> x :: ret_ls
			| y :: xs -> append_max (y :: xs) (x :: ret_ls)
	in
	append_max l []
;;

(* ***********************************************************************
    END PART I
************************************************************************ *)		

(* Part II Section 1: More error handling *)

(*
	Consider a student record as a list of Results of scores.
	Each entry will be Ok if the entry was retrieved, or an Error otherwise.
	The scores may be None, if the student did not complete the assignment.
	
	Given a student record, compute the mean of the existing scores, or None.
	In the case that one of the entries is an Error, throw an exception.
*)
type student_record = (float option, string) result list

let mean_of_record (es: student_record): float option =
	let rec record_to_list (ls: float list) (sr: student_record) : float list =
		match sr with
		| Error s :: xs-> failwith s
		| [] -> ls
		| Ok x :: xs -> match x with
			| None -> record_to_list ls xs
			| Some x -> record_to_list (x :: ls) xs
	in
	match record_to_list [] es with
	| [] -> None
	| x::xs -> Some(Float.(/) (List.fold ~f:(+.) ~init:0. (record_to_list [] es)) (Float.of_int @@ List.length @@ record_to_list [] es))
;;

(*
	Given a student record as before, compute the mean of the scores.
	Assume that the scores do exist, and throw an exception otherwise.
	Collect each of the Error branches into a list of errors and return this too.
*)
let mean_of_record' (es: student_record): float * string list =
	let rec record_to_list (ls: float list) (string_list: string list) (sr: student_record): (float list * string list) =
		match sr with
		| Error s :: xs-> record_to_list ls (s :: string_list) xs
		| [] -> (ls, string_list)
		| Ok x :: xs -> match x with
			| None -> failwith "none"
			| Some x -> record_to_list (x :: ls) string_list xs
	in
	match record_to_list [] [] es with
	| (scores, errors) -> match scores with
		| [] -> failwith "none"
		| x :: xs -> (Float.(/) (List.fold ~f:(+.) ~init:0. (scores)) (Float.of_int @@ List.length scores), errors)
;;

(*
	Part II Section 2: for selected functions in Part I, provide
	a reimplementation of your previous code by refactoring
	the definition to use combinators provided by the List module.
	
	Care should be taken to use a concise, elegant combination
	of these provided functions to best express the task.
	
	These new implementations should not be explicitly recursive.
	Note that the autograder is not aware if you cheated and used recursion;
	we will manually inspect your code and give you negative points if 
	you used recursion.

*)

let iota1' (n: int): int list = 
	List.init n ~f:(fun x -> n - x)	
;;

let iota2' (n: int): int list =
	List.init n ~f:(fun x -> x + 1)
;;

let factors' (n: int): int list =		
	iota2' n |> List.filter ~f:(fun x -> if n % x = 0 then true else false) 
;;

let insert_string' (s: string) (l: string list): string list =
	List.merge [s] l ~compare:(String.compare)
;;

(*
	Part II  Section 3: novel problems using List and String modules
	
	Use the List combinators, pipelining, etc. when possible
	and whenever it improves the readability of the solution.
*)

(*
	Given a list of integers, produce a list which pairs repeated integers
	with the number of times they repeated.
	E.G.
		run_length_encode [1; 1; 25; 4; 4; 4; -3; 1; 1] =
			[ (1, 2); (25, 1); (4, 3); (-3, 1); (1, 2) ]
*)
let run_length_encode (ns: int list): (int * int) list = 
	let count_array (ls: int list): int list list =
		let not_equal (x: int) (y: int): bool = x <> y in
		ls |> List.group ~break:(not_equal)
	in
	let hd_no_option (l: int list): int = 
		match List.hd l with
		| Some x -> x
		| None -> failwith "this will never happen"
	in
	List.map ~f:(fun x -> (hd_no_option x, List.length x)) (count_array ns)
;;

(*
	Given a list of values paired with repetition counts,
	produce the list which contains each value repeated the
	correct number of times.
	I.E.
		invert the previous function's transformation.
*)
let run_length_decode (ps: (int * int) list): int list = 
	let rec repeater (ls: int list) (x: int) (n: int): int list = 
		if n = 0 then ls
		else repeater (x :: ls) x (n - 1)
	in
	let rec decoder (decoded_list: int list) (encoded_list: (int * int) list) : int list =
		match encoded_list with
		| [] -> decoded_list
		| (x, y) :: xs -> decoder (decoded_list @ (repeater [] x y)) xs
	in
	decoder [] ps
;;

(*
	Given a list of positive integers, compute the smallest integer which is evenly
	divisible by each of the integers in the list.
*)
let smallest_divisible_by (ns: int list): int = 
	List.fold ~f:(lcm) ~init: 1 ns
;;
	
(* 
	Given a string, check to see if the underlying sentence is a pangram, namely
	that it uses all 26 letters of the alphabet at least once.  Ignore all characters 
	outside of the alphabet and also ignore case.  See Base.String for helpful
	filtering and conversion functions.  Use pipes `|>` to make an elegant solution!
*)

let is_pangram (s: string): bool = 
	String.lowercase s 
	|> String.to_list 
	|> List.filter ~f:Char.is_alpha 
	|> List.dedup_and_sort ~compare:Char.compare 
	|> List.length |> (=) 26
;;