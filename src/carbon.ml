
(* SMILES tokens *)

module L = BatList
module S = BatString

open Printf

type aliphatic_orga = B
                    | C
                    | N
                    | O
                    | S
                    | P
                    | F
                    | Cl
                    | Br
                    | I

exception Invalid_aliphatic_orga of string

let aliphatic_orga_of_string = function
  | "B"   -> B
  | "C"   -> C
  | "N"   -> N
  | "O"   -> O
  | "S"   -> S
  | "P"   -> P
  | "F"   -> F
  | "Cl"  -> Cl
  | "Br"  -> Br
  | "I"   -> I
  | other -> raise (Invalid_aliphatic_orga other)

let parse_aliphatic_orga s =
  let n = S.length s in
  if n = 0 then
    (None, "")
  else if n = 1 then
    try (Some (aliphatic_orga_of_string s), "")
    with Invalid_aliphatic_orga _ -> (None, s)
  else (* n > 1 *)
    begin
      if S.get s 1 = 'r' && S.get s 0 = 'B' then
        (Some Br, S.sub s 2 (n - 2))
      else if S.get s 1 = 'l' && S.get s 0 = 'C' then
        (Some Cl, S.sub s 2 (n - 2))
      else
        try (Some (aliphatic_orga_of_string (S.sub s 0 1)), S.sub s 1 (n - 1))
        with Invalid_aliphatic_orga _ -> (None, s)
    end

let string_of_aliphatic_orga = function
  | B  -> "B"
  | C  -> "C"
  | N  -> "N"
  | O  -> "O"
  | S  -> "S"
  | P  -> "P"
  | F  -> "F"
  | Cl -> "Cl"
  | Br -> "Br"
  | I  -> "I"

type aromatic_orga = Aro_B
                   | Aro_C
                   | Aro_N
                   | Aro_O
                   | Aro_S
                   | Aro_P

exception Invalid_aromatic_orga of string

let aromatic_orga_of_string = function
  | "b" -> Aro_B
  | "c" -> Aro_C
  | "n" -> Aro_N
  | "o" -> Aro_O
  | "s" -> Aro_S
  | "p" -> Aro_P
  | other -> raise (Invalid_aromatic_orga other)

let parse_aromatic_orga s =
  let n = S.length s in
  if n = 0 then
    (None, "")
  else (* n > 0 *)
    try (Some (aromatic_orga_of_string (S.sub s 0 1)), S.sub s 1 (n - 1))
    with Invalid_aromatic_orga _ -> (None, s)

let string_of_aromatic_orga = function
  | Aro_B -> "b"
  | Aro_C -> "c"
  | Aro_N -> "n"
  | Aro_O -> "o"
  | Aro_S -> "s"
  | Aro_P -> "p"

type atom = Bracket_atom of string
          | Aliphatic_orga of aliphatic_orga
          | Aromatic_orga of aromatic_orga
          | Star

let string_of_atom = function
  | Bracket_atom ba -> ba
  | Aliphatic_orga ali -> string_of_aliphatic_orga ali
  | Aromatic_orga aro -> string_of_aromatic_orga aro
  | Star -> "*"

type bond = Single
          | Double
          | Triple
          | Quadruple
          | Aromatic
          | Up
          | Down

exception Invalid_bond of string

let bond_of_string = function
  | "-"  -> Single
  | "="  -> Double
  | "#"  -> Triple
  | "$"  -> Quadruple
  | ":"  -> Aromatic
  | "/"  -> Up
  | "\\" -> Down
  | other -> raise (Invalid_bond other)

let parse_bond s =
  let n = S.length s in
  if n = 0 then
    (None, "")
  else (* n > 0 *)
    try (Some (bond_of_string (S.sub s 0 1)), S.sub s 1 (n - 1))
    with Invalid_bond _ -> (None, s)

type smi_token = Atom of atom
               | Bond of bond
               | Ring_open_close of int
               | Branch_open
               | Branch_close

let parse_atoms_and_bonds str: smi_token list =
  let rec loop acc s =
    if S.length s = 0 then
      L.rev acc
    else
      begin match parse_aliphatic_orga s with
        | (Some a, rest) -> loop (Atom (Aliphatic_orga a) :: acc) rest
        | (None, _) ->
          begin match parse_bond s with
            | (Some b, rest) -> loop ((Bond b) :: acc) rest
            | (None, _) ->
              begin match parse_aromatic_orga s with
                | (Some a, rest) -> loop (Atom (Aromatic_orga a) :: acc) rest
                | (None, _) -> failwith ("parse_atoms_and_bonds: cannot parse: " ^ s)
              end
          end
      end
  in
  loop [] str

let string_of_bond = function
  | Single    -> "-"
  | Double    -> "="
  | Triple    -> "#"
  | Quadruple -> "$"
  | Aromatic  -> ":"
  | Up        -> "/"
  | Down      -> "\\"

type smiles = smi_token list

let string_of_ring_closure c =
  if c > 99 then (* forbid triple digits ring closure *)
    failwith (sprintf "Carbon.string_of_ring_closure: %d > 99" c)
  else if c > 9 then
    (* a double digit ring closure in SMILES must be prefixed by % *)
    sprintf "%%%d" c
  else
    sprintf "%d" c

let string_of_smiles tokens =
  String.concat ""
    (L.map (function
         | Atom a -> string_of_atom a
         | Bond b -> string_of_bond b
         | Ring_open_close i -> string_of_ring_closure i
         | Branch_open -> "("
         | Branch_close -> ")"
       ) tokens
    )

let parens_regexp = Str.regexp "[()]"
let bracket_atom_regexp = Str.regexp "\\[[^]]+\\]"
let double_digits_regexp = Str.regexp "%[0-9][0-9]"
let single_digit_regexp = Str.regexp "[0-9]"

let parse_double_digit_ring_closure s =
  try Scanf.sscanf s "%%%d" (fun x -> assert(x >= 10); x)
  with _exn ->
    failwith ("Carbon.parse_double_digit_ring_closure: cannot parse: " ^ s)

let parse_single_digit_ring_closure s =
  try Scanf.sscanf s "%d" (fun x -> assert(x <= 9); x)
  with _exn ->
    failwith ("Carbon.parse_single_digit_ring_closure: cannot parse: " ^ s)

let tokenize (s: string): smi_token list =
  let res =
    L.map Str.(function
        | Delim "(" -> [[[[Branch_open]]]]
        | Delim ")" -> [[[[Branch_close]]]]
        | Delim _ -> assert(false) (* parens_regexp would be wrong then *)
        | Text a ->
          L.map Str.(function
              | Delim bracket_atom_str -> [[[Atom (Bracket_atom bracket_atom_str)]]]
              | Text b ->
                L.map Str.(function
                    | Delim double_digit_rc ->
                      [[Ring_open_close (parse_double_digit_ring_closure double_digit_rc)]]
                    | Text c ->
                      L.map Str.(function
                          | Delim single_digit_rc ->
                            [Ring_open_close
                               (parse_single_digit_ring_closure single_digit_rc)]
                          | Text atom_bonds ->
                            parse_atoms_and_bonds atom_bonds
                        )
                        (Str.bounded_full_split single_digit_regexp c 1024)
                  )
                  (Str.bounded_full_split double_digits_regexp b 1024)
            )
            (Str.bounded_full_split bracket_atom_regexp a 1024)
      )
      (* WARNING: bug if more than 1024 tokens in the string ! *)
      (Str.bounded_full_split parens_regexp s 1024) in
  L.flatten (L.flatten (L.flatten (L.flatten res)))
