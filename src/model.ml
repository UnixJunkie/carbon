
module CLI = Minicli.CLI
module LO = Line_oriented
module S = BatString

open Printf

type parser_state = Chain of int
                  | Branch of int
                  | Ring of int

(* model the states by which the parser goes through while reading a SMILES string *)

let main () =
  let argc, args = CLI.init () in
  if argc = 1 then
    (eprintf "usage:\n  \
              %s\n  \
              [-i <filename>]: SMILES input file\n  \
              [-o <filenams>: (reconstructed) SMILES output file\n"
       Sys.argv.(0);
     exit 1);
  let input_fn = CLI.get_string ["-i"] args in
  let output_fn = CLI.get_string ["-o"] args in
  CLI.finalize();
  LO.with_infile_outfile input_fn output_fn (fun input output ->
      try
        while true do
          let line = input_line input in
          let smi, name = S.split line ~by:"\t" in
          let tokens = Carbon.tokenize smi in
          let smi' = Carbon.string_of_smiles tokens in
          fprintf output "%s\t%s\n" smi' name
        done
      with End_of_file -> ()
    )

let () = main ()
