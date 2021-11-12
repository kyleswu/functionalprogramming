(*
  Part II:

  Implement an executable `ngrams.exe` which can use n-gram models in 
  several ways.   It should expect to be called with the following 
  arguments, with bracketed ones optional:

    $ ngrams.exe N CORPUS-FILE [--sample SAMPLE-LENGTH [INITIAL-WORDS...]] [--most-frequent N-MOST-FREQUENT]

  
  Functionality should be as follows:

  - Load the file specified by `CORPUS-FILE` and split its contents into a
    sequence of strings based on whitespace. Treat newlines and spaces, etc. equally.

  - Sanitize each of the strings in this sequence according to the `sanitize` 
    function, removing all strings which end up as `None` in this way, to 
    produce a new sequence.

  - Initialize an n-gram distribution using `N` and the sanitized 
    sequence of words.

  
  If the option `--sample SAMPLE-LENGTH` is provided:

    To stdout, output a sequence of `SAMPLE-LENGTH` words randomly sampled 
    from the n-gram model.  Print them out separated by single spaces. 
    
    To begin the sequence, use the `INITIAL-WORDS` arguments
    provided after `--sample` to seed the sequence, or if none are provided, 
    choose a random starting n-gram to begin. You may assume that the words
    provided as `INITIAL-WORDS` are already sanitized, and that there are 
    at least (`N` - 1) of them.

  If the option `--most-frequent N-MOST-FREQUENT` is provided:
  
    To stdout, output a sorted JSON-formatted array of length `N-MOST-FREQUENT`
    containing information about the most common n-grams seen in the `CORPUS-FILE`, like so:

    [
      { 
        "ngram": ["array", "of", "strings", ...],
        "frequency": <number of times witnessed>
      },
      ...
    ]

  You may assume that only one of `--sample` or `--most-frequent` will be
  supplied at a time, and that at least one will be given.

  To output JSON, you may again use yojson and `ppx_deriving_yojson`,
  and to parse command line arguments, we recommend looking into
  the `Core.Command` module.
  See Real World OCaml Chapter 14 https://dev.realworldocaml.org/command-line-parsing.html
  for some examples of Core.Command in action.
*)

open Core

let read_and_filter_words (file: string): string list =
  Stdio.In_channel.read_all file
  |> String.split_on_chars ~on:[' '; '\t'; '\n'; '\r']
  |> List.filter_map ~f:(fun s -> Lib.sanitize s)

let print_yojson_freq (hl: Lib.entry_list) =
  Stdio.printf "%s\n" @@ Yojson.Safe.to_string (Lib.entry_list_to_yojson hl)

let print_sample_sequence (front: string list) (sampled_sequence: string list) (size: int) =
  let print_list = List.take (front @ sampled_sequence) size in
  List.iter ~f:(printf "%s ") print_list

module Sl_ngram = Lib.N_grams (Random) (String);;

let command =
  Command.basic
    ~summary:""
    Command.Let_syntax.(
    let%map_open
      n = anon ("n" %: int)
    and filename = anon ("filename" %: string)
    and n_most_freq = flag "--most-frequent" (optional int)
      ~doc:"find N-MOST-FREQUENT ngrams"
    and sample_input = flag "--sample" escape
      ~doc:"sample with SAMPLE-LENGTH"
    in
    fun () -> 
      let input_word_list = read_and_filter_words filename in
      let chunked_list = Lib.chunks n input_word_list in
      match n_most_freq, sample_input with
      | Some freq, None -> 
        chunked_list
        |> Lib.create_freq_map
        |> Lib.find_freq_list freq
        |> print_yojson_freq
      | None, Some sample_input ->
        let sample_length = int_of_string @@ List.hd_exn sample_input in
        let initial_words = List.tl_exn sample_input in
        let n_gram_dist =  Sl_ngram.ngrams n input_word_list in
        if List.length initial_words = 0 then
          let rand_init = Lib.rand_initial (module Random) input_word_list n in
          let sample_seq = Sl_ngram.sample_sequence n_gram_dist ~max_length:sample_length ~initial_ngram:rand_init in
          print_sample_sequence [] sample_seq sample_length
        else 
          let front, input_init = Lib.get_initial_n_minus_one initial_words n in
          let sample_seq = Sl_ngram.sample_sequence n_gram_dist ~max_length:sample_length ~initial_ngram:input_init in
          print_sample_sequence front sample_seq sample_length
      | _, _ -> failwith "should never happen since flags are mutually exclusive and one must be given")

let () =
  Command.run command