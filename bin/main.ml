open! Core

let command =
  Command.group ~summary:""
  [ "interface", Autobindgen.Interface.command; "hierarchy", Autobindgen.Ast.command ]
;;

let () = Command_unix.run command
