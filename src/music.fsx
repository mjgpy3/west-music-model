type NaturalPitch =
  | C of int
  | D of int
  | E of int
  | F of int
  | G of int
  | A of int
  | B of int

type AccidentalPitch =
  | Sharp of NaturalPitch
  | Flat of NaturalPitch

type Note =
  | Accidental of AccidentalPitch
  | Natural of NaturalPitch

type Direction = | Up | Down

let semitone direction note = match direction, note with
  | Up, Natural (C n) -> C n |> Sharp |> Accidental
  | Up, Accidental (Sharp (C n)) -> D n |> Natural
  | Up, Natural (D n) -> D n |> Sharp |> Accidental
  | Up, Accidental (Sharp (D n)) -> E n |> Natural
  | Up, Natural (E n) -> F n |> Natural
  | Up, Natural (F n) -> F n |> Sharp |> Accidental
  | Up, Accidental (Sharp (F n)) -> G n |> Natural
  | Up, Natural (G n) -> G n |> Sharp |> Accidental
  | Up, Accidental (Sharp (G n)) -> A n |> Natural
  | Up, Natural (A n) -> A n |> Sharp |> Accidental
  | Up, Accidental (Sharp (A n)) -> B n |> Natural
  | Up, Natural (B n) -> n + 1 |> C |> Natural

  | Down, Accidental (Sharp (C n)) -> C n |> Natural
  | Down, Natural (D n) -> C n |> Sharp |> Accidental
  | Down, Accidental (Sharp (D n)) -> D n |> Natural
  | Down, Natural (E n) -> D n |> Sharp |> Accidental
  | Down, Natural (F n) -> E n |> Natural
  | Down, Accidental (Sharp (F n)) -> F n |> Natural
  | Down, Natural (G n) -> F n |> Sharp |> Accidental
  | Down, Accidental (Sharp (G n)) -> G n |> Natural
  | Down, Natural (A n) ->  G n |> Sharp |> Accidental
  | Down, Accidental (Sharp (A n)) -> A n |> Natural
  | Down, Natural (B n) -> A n |> Sharp |> Accidental
  | Down, Natural (C n) -> n - 1 |> B |> Natural
  | n -> failwithf "Not a real note %A" n

let rec transpose direction amount note = match direction, amount with
  | _, 0 -> note
  | Up, n -> semitone Up note |> transpose Up (n - 1)
  | Down, n -> semitone Down note |> transpose Up (n - 1)

let tone direction = transpose direction 2

[<EntryPoint>]
let main args =
  printfn "%A" <| Seq.map (tone Up) [A 0 |> Natural; B 0 |> Natural]
  0
