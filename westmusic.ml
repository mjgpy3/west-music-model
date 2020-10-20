type note_name =
  | C
  | CSharpDFlat
  | D
  | DSharpEFlat
  | E
  | F
  | FSharpGFlat
  | G
  | GSharpAFlat
  | A
  | ASharpBFlat
  | B;;

let next_higher_note_name = function
  | C -> CSharpDFlat
  | CSharpDFlat -> D
  | D -> DSharpEFlat
  | DSharpEFlat -> E
  | E -> F
  | F -> FSharpGFlat
  | FSharpGFlat -> G
  | G -> GSharpAFlat
  | GSharpAFlat -> A
  | A -> ASharpBFlat
  | ASharpBFlat -> B
  | B -> C;;

let next_lower_note_name = function
  | CSharpDFlat -> C
  | D -> CSharpDFlat
  | DSharpEFlat -> D
  | E -> DSharpEFlat
  | F -> E
  | FSharpGFlat -> F
  | G -> FSharpGFlat
  | GSharpAFlat -> G
  | A -> GSharpAFlat
  | ASharpBFlat -> A
  | B -> ASharpBFlat
  | C -> B;;

type nat = | Zero | Succ of nat;;

let n1 = Succ Zero;;
let n2 = Succ n1;;
let n3 = Succ n2;;
let n4 = Succ n3;;
let n5 = Succ n4;;
let n6 = Succ n5;;
let n7 = Succ n6;;
let n8 = Succ n7;;
let n9 = Succ n8;;
let n10 = Succ n9;;
let n11 = Succ n10;;
let n12 = Succ n11;;

let rec n_times v f = function
  | Zero -> v
  | Succ k -> n_times (f v) f k;;

type note = { name : note_name; octave: int };;

type pitch_direction = | Higher | Lower;;

let (<<<) f g x = f (g x);;

let semitone dir ({ name; octave } as note) =
  match name, dir with
    | B, Higher -> { name=next_higher_note_name name; octave=octave+1 }
    | _, Higher -> { note with name=next_higher_note_name name }
    | C, Lower -> { name=next_lower_note_name name; octave=octave-1 }
    | _, Lower -> { note with name=next_lower_note_name name };;

let tone dir = semitone dir <<< semitone dir;;

let transpose_note dir note = n_times note (semitone dir);;

let scale_from_offsets offsets tonic =
  List.map (transpose_note Higher tonic) offsets;;

let natural_major_offset =
  [ Zero; n2; n4; n5; n7; n9; n11; n12 ];;

let natural_major_scale_from_tonic =
  scale_from_offsets natural_major_offset;;

let natural_minor_offset =
  [ Zero; n2; n3; n5; n7; n8; n10; n12 ];;

let natural_minor_scale_from_tonic =
  scale_from_offsets natural_minor_offset;;

let blues_offset =
  [Zero; n3; n5; n6; n7; n10; n12];;

let blues_scale_from_tonic =
  scale_from_offsets natural_minor_offset;;

let () = print_endline "Hello, World!"
