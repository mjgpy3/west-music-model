type NaturalPitch =
  | C
  | D
  | E
  | F
  | G
  | A
  | B

type AccidentalPitch =
  | Sharp of NaturalPitch
  | Flat of NaturalPitch

type Note =
  | Accidental of AccidentalPitch
  | Natural of NaturalPitch

type Direction = | Up | Down

let semitone direction note = match direction, note with
  | Up, Natural C -> Sharp C |> Accidental
  | Up, Accidental (Sharp C) -> Natural D
  | Up, Natural D -> Sharp D |> Accidental
  | Up, Accidental (Sharp D) -> Natural E
  | Up, Natural E -> Natural F
  | Up, Natural F -> Sharp F |> Accidental
  | Up, Accidental (Sharp F) -> Natural G
  | Up, Natural G -> Sharp G |> Accidental
  | Up, Accidental (Sharp G) -> Natural A
  | Up, Natural A -> Sharp A |> Accidental
  | Up, Accidental (Sharp A) -> Natural B
  | Up, Natural B -> Natural C

  | Down, Accidental (Sharp C) -> Natural C
  | Down, Natural D -> Accidental (Sharp C)
  | Down, Accidental (Sharp D) -> Natural D
  | Down, Natural E -> Accidental (Sharp D)
  | Down, Natural F -> Natural E
  | Down, Accidental (Sharp F) -> Natural F
  | Down, Natural G -> Accidental (Sharp F)
  | Down, Accidental (Sharp G) -> Natural G
  | Down, Natural A -> Accidental (Sharp G)
  | Down, Accidental (Sharp A) -> Natural A
  | Down, Natural B -> Accidental (Sharp A)
  | Down, Natural C -> Natural B
  | n -> failwith "Not a real note %A" n

let rec transpose direction amount note = match direction, amount with
  | _, 0 -> note
  | Up, n -> semitone Up note |> transpose Up (n - 1)
  | Down, n -> semitone Down note |> transpose Up (n - 1)

let tone direction = transpose direction 2

[<EntryPoint>]
let main args =
  printfn "Arguments passed to function : %A" args
  0
