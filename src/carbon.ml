
(* SMILES tokens *)

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

let string_of_bond = function
  | Single    -> "-"
  | Double    -> "="
  | Triple    -> "#"
  | Quadruple -> "$"
  | Aromatic  -> ":"
  | Up        -> "/"
  | Down      -> "\\"

type ringbond = Ring_opening of int
              | Ring_closure of int

type branch = Branch_opening
            | Branch_closure

type dot = Dot
