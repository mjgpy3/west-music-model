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

type note = { name : note_name; octave: int };;

type chord =
  | Notes of note list;;

type pitch_direction = | Higher | Lower;;

let semitone ({ name; octave } as note) dir =
  match name, dir with
    | B, Higher -> { name=next_higher_note_name name; octave=octave+1 }
    | _, Higher -> { note with name=next_higher_note_name name }
    | C, Lower -> { name=next_lower_note_name name; octave=octave-1 }
    | _, Lower -> { note with name=next_lower_note_name name };;

let rec transpose note dir = function
  | Zero -> note
  | Succ k -> transpose (semitone note dir) dir k;;

let () = print_endline "Hello, World!"
