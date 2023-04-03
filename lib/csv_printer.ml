open! Core

type -!'a t =
  { header : string
  ; getter : 'a -> string
  }
[@@deriving fields]

let column header getter = { header; getter }
let sexpable header getter = { header; getter = Fn.compose Sexp.to_string_mach getter }

let to_csv columns values =
  List.map columns ~f:header
  :: List.map values ~f:(fun value ->
       List.map columns ~f:(fun column -> getter column value))
;;
