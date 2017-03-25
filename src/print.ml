type color
  = Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan

type attribute
  = Clear
  | Color of color

let color_code = function
  | Red -> "31"
  | Green -> "32"
  | Yellow -> "33"
  | Blue -> "34"
  | Magenta -> "35"
  | Cyan -> "36"

let code = function
  | Clear -> "0"
  | Color c -> color_code c

let string_of_attr attr =
  "\027[" ^ code attr ^ "m"

let style attributes text =
  String.concat "" (List.map string_of_attr attributes) ^ text ^ string_of_attr Clear

(* shortcuts *)
let red = style [Color Red]
let green = style [Color Green]
let yellow = style [Color Yellow]
let blue = style [Color Blue]
let magenta = style [Color Magenta]
let cyan = style [Color Cyan]
