
(*
    Part II: Core.Sys, Stdio, and making a useful utility
*)

(*
    You will be developing a simple command-line application which will compute a
    histogram of word occurrences in a whole directory tree of files.
    
    Given a directory path on the command line (or the current directory if none is given), you should:

    - traverse the directory recursively to find all text files

    - compute a histogram of word uses over all the files: sum up all
    of the occurrences over all files and sort the resulting dictionary
    from most to least common occurrence.

    - report the data to stdout in JSON format, as a JSON array of objects of the form:

        [ { "word": <word>,
            "count": <number>
          }, 
          { "word": <word>,
            "count": <number>
          } ... ]


    We need to define exactly what a "word" in a text file is; here is the 
    definition we will be using.

    * Words only include alphanumerical characters as well as `-` 
       (for "re-build" for example to be considered a word)
    * Words are case-insensitive and lower case is the canonical form
    * spaces, tabs, and newlines are word separators
    * Non-alphanumeric characters besides "-" are filtered from words.  
    So, "it's" just turns into "its".
    
    Also we need to define what is a text file.  For simplicity we
    will just use the extension on the file name and consider only
    the following extensions as text files:
      .txt, .text, .md

    Libraries for you to use:

    Any of the default set of libraries for the course may be used.
    Core.Sys and Stdio provide basics for filesystem usage and accessing argv.

    You are of course welcome to use your Part I dictionary.

    Yojson provides JSON manipulation for output.  Also, we recommend 
    `ppx_deriving_yojson` as a way to convert your own record types into 
    JSON and then strings directly.  The assignment contains links to the docs.

    Note: don't use any other opam libraries beyond the official opam libraries
    on the FPSE Coding page.

    `jq` is a command-line JSON tool which could come in handy for debugging.
*)

(*
	We are providing you with some template code below.

	OCaml executables work by simply evaluating each top-level expression in the file,
	similar to a scripting language conceptually.  So all the let () = code will run.

	Feel free to modify the code below as much as you want.
	Should you need or want more of the standard set of libraries
	than just `Core` and `Stdio` (and your own implementation of Part I), 
	you will need to modify the dune file to specify this.

    Note that if you are using `ppx_deriving_yojson` you will both need
    to list the yojson library and also add
    (preprocess
    (pps ppx_deriving_yojson))
    to the executable build in the dune file to enable the ppx macros

*)

open Core

(* function to return the given path, or if none give current directory *)
let target_dir = 
  match Sys.get_argv () |> Array.to_list with
  | _ :: target :: _ -> target
  | _ :: [] -> Sys.getcwd ()
  | [] -> Sys.getcwd ()
  ;;

(* returns boolean, true if valid text file else false *)
let valid_file (file: string): bool =
  String.is_suffix file ~suffix:(".txt") || 
  String.is_suffix file ~suffix:(".text") || 
  String.is_suffix file ~suffix:(".md")
  ;;

(* function which recursively traverses the directory and its sub-directories, 
  returns a string list containing the paths to all valid text files *)
let get_path_elt (path: string): string list =    
  let rec traverse (out_list: string list) = function
    | file :: remaining when Sys.is_directory_exn file ->
      Sys.ls_dir file
      |> List.map ~f:(Filename.concat file)
      |> List.append remaining
      |> traverse out_list
    | file :: remaining when valid_file file -> traverse (file :: out_list) remaining
    | _ :: remaining -> traverse out_list remaining
    | [] -> out_list
  in
  traverse [] [path]
  ;;

(* returns boolean, true if alphanumeric else false *)
let is_alphanumeric (c: char): bool =
  (Char.(>=) c 'a' && Char.(<=) c 'z') ||
  (Char.(>=) c '0' && Char.(<=) c '9') ||
  Char.(=) c '-'
  ;;

(* given a valid text file, read in all the text to a string,
  set all letters to lowercase, split into list based on spaces tabs and newlines
  filter out all non alphanumeric characters from strings
  filter out all empty strings from list*)  
let read_and_filter_words (file:string): string list = 
  Stdio.In_channel.read_all file
  |> String.lowercase
  |> String.split_on_chars ~on:[' '; '\t'; '\n'; '\r']
  |> List.map ~f:(fun s -> String.of_char_list 
    @@ List.filter (String.to_list s) ~f:(fun c -> is_alphanumeric c))
  |> List.filter ~f:(fun s -> if String.(=) s "" then false else true)
  ;; 

(* helper function which takes a single file and handles it, adding its words and increasing the count in the histo *)
let single_file_render (file: string) (histo: int Treedict.dict): int Treedict.dict =
  let (word_list: string list) = read_and_filter_words file in
  let rec add_to_histo (word_list: string list) (histo: int Treedict.dict): int Treedict.dict =
    match word_list with
    | [] -> histo
    | word :: remaining ->
      match Treedict.lookup word histo with
      | Some value -> add_to_histo remaining @@ Treedict.insert histo word (value + 1)
      | None -> add_to_histo remaining @@ Treedict.insert histo word 1
  in
  add_to_histo word_list histo
  ;;

(* function which takes the list of all valid text files and processes data to histo *)
let rec compute_histo (histo: int Treedict.dict) (files: string list): int Treedict.dict =
  match files with
  | [] -> histo
  | file :: remaining -> compute_histo (single_file_render file histo) remaining
  ;;

(* pair for our json *)
type entry_pair = { word: string; count: int } [@@deriving yojson]

(* pair list for our json *)
type entry_list = entry_pair list [@@deriving yojson]

(* compare function for our pairs *)
let compare_histo_pair ((k1, v1): string * int) ((k2, v2): string * int) = 
  if v1 = v2 then (- String.compare k1 k2)
  else compare v1 v2
  ;;

(* properly formats the histo data and then converts it into the proper entry_list format *)
let histo_to_histo_list (histo: int Treedict.dict): entry_list =
  Treedict.list_of_btree histo
  |> List.sort ~compare:(fun (key1, value1) (key2, value2) -> compare_histo_pair (key1, value1) (key2, value2))
  |> List.fold ~init:[] ~f:(fun init (word, count) -> { word; count } :: init)
  ;;

(* print function for our resulting entry_list *)
let print_string (hl: entry_list) =
  Stdio.printf "%s\n" @@ Yojson.Safe.to_string (entry_list_to_yojson hl)

let () =
  target_dir
  |> get_path_elt
  |> compute_histo Leaf
  |> histo_to_histo_list
  |> print_string