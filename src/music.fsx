type NaturalPitch =
  | C
  | D
  | E
  | F
  | G
  | A
  | B

type AccidentialPitch =
  | Sharp of NaturalPitch
  | Flat of NaturalPitch

[<EntryPoint>]
let main args =
  printfn "Arguments passed to function : %A" args
  0
