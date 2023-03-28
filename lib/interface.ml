open! Core

let check_kind expected =
  match%map_open.Ast.Parser field kind with
  | Some record_kind when Ast.Kind.equal record_kind expected -> ()
  | kind ->
    raise_s
      [%message "Unexpected kind" (kind : Ast.Kind.t option) (expected : Ast.Kind.t)]
;;

module Type = struct
  (* Parse more later *)
  type t = string [@@deriving sexp_of, compare]

  let parse { Ast.Type.qualType; desugaredQualType; typeAliasDeclId = _ } =
    Option.value desugaredQualType ~default:qualType
  ;;
end

let only_one_bool bools_and_values =
  List.map bools_and_values ~f:(fun (bool, value) ->
    Option.some_if (Option.value bool ~default:false) value)
  |> List.reduce_exn ~f:(fun a b ->
       match a, b with
       | Some x, None | None, Some x -> Some x
       | None, None -> None
       | Some _, Some _ -> raise_s [%message "conflict"])
;;

module Property = struct
  module Getter = struct
    type t = string [@@deriving sexp_of, compare]

    let parse =
      let parser =
        let%map_open.Ast.Parser name = field name
        and () = check_kind ObjCMethodDecl in
        Option.value_exn name
      in
      Ast.Parser.collect_exn parser ~type_:"Property.Getter"
    ;;
  end

  type t =
    { name : string
    ; atomic : bool
    ; access : [ `readonly | `readwrite ]
    ; copy : bool [@sexp.bool]
    ; nullability : bool [@sexp.bool]
    ; strength : [ `weak | `strong ] option [@sexp.option]
    ; assign : bool [@sexp.bool]
    ; unsafe_unretained : bool [@sexp.bool]
    ; null_resettable : bool [@sexp.bool]
    ; type_ : Type.t
    ; retain : bool [@sexp.bool]
    ; class_ : bool [@sexp.bool]
    ; getter : Getter.t option [@sexp.option]
    }
  [@@deriving sexp_of, compare]

  let parse =
    let parser =
      let%map_open.Ast.Parser name = field name
      and () = check_kind ObjCPropertyDecl
      and atomic = field atomic
      and readonly = field readonly
      and copy = field copy
      and nullability = field nullability
      and strong = field strong
      and type_ = field type_
      and assign = field assign
      and unsafe_unretained = field unsafe_unretained
      and readwrite = field readwrite
      and null_resettable = field null_resettable
      and retain = field retain
      and class_ = field class_
      and nonatomic = field nonatomic
      and weak = field weak
      and getter = field getter 
      and _ = field loc
      and _ = field range
      and _inner = field inner in
      { name = Option.value_exn name
      ; atomic = only_one_bool [ atomic, true; nonatomic, false ] |> Option.value_exn
      ; retain = Option.value retain ~default:false
      ; class_ = Option.value class_ ~default:false
      ; strength = only_one_bool [ strong, `strong; weak, `weak ]
      ; access =
          only_one_bool [ readonly, `readonly; readwrite, `readwrite ] |> Option.value_exn
      ; copy = Option.value copy ~default:false
      ; nullability = Option.value nullability ~default:false
      ; assign = Option.value assign ~default:false
      ; unsafe_unretained = Option.value unsafe_unretained ~default:false
      ; null_resettable = Option.value null_resettable ~default:false
      ; type_ = Option.value_exn type_ |> Type.parse
      ; getter = Option.map ~f:Getter.parse getter
      }
    in
    Ast.Parser.collect_exn parser ~type_:"Property"
  ;;
end

module Class = struct
  type t =
    { name : string
    ; super : string option [@sexp.option]
    ; file : string option [@sexp.option]
    ; protocols : String.Set.t
    ; contents : [ `Property of Property.t ] list
    }
  [@@deriving sexp_of, compare]

  let parse_super =
    let parser =
      let%map_open.Ast.Parser name = field name
      and kind = field kind in
      Option.iter kind ~f:(function
        | ObjCInterfaceDecl -> ()
        | kind -> raise_s [%message "Unexpected kind for super" (kind : Ast.Kind.t)]);
      name
    in
    Ast.Parser.collect_exn parser ~type_:"Class.super"
  ;;

  let parse_protocol =
    let parser =
      let%map_open.Ast.Parser name = field name
      and () = check_kind ObjCProtocolDecl in
      Option.value_exn name
    in
    Ast.Parser.collect_exn parser ~type_:"Class.protocols"
  ;;

  let implementation_parser =
    Ast.Parser.collect_exn (Ast.Parser.return ()) ~type_:"Class.implementation"
  ;;

  let parse =
    let parser =
      let%map_open.Ast.Parser name = field name
      and () = check_kind ObjCInterfaceDecl
      and _ = field isImplicit
      and _ = field range
      and _ = field mangledName
      and _ = field previousDecl
      and inner = field inner
      and loc = field loc
      and super = field super
      and implementation = field implementation
      and protocols = field protocols in
      Option.iter implementation ~f:implementation_parser;
      let contents =
        Option.value inner ~default:[]
        |> List.filter_map ~f:(fun t ->
             match t.kind with
             | Some
                 ( AvailabilityAttr
                 | VisibilityAttr
                 | ObjCRootClassAttr
                 | SwiftNameAttr
                 | SwiftAttrAttr
                 | SwiftPrivateAttr
                 | ObjCRequiresPropertyDefsAttr
                 | ArcWeakrefUnavailableAttr ) -> None
             | Some ObjCPropertyDecl -> `Property (Property.parse t) |> Some
             | Some ObjCIvarDecl -> (* TODO parse *) None
             | Some RecordDecl -> (* TODO parse, probably ns_enum *) None
             | Some ObjCMethodDecl -> (* TODO parse *) None
             | Some ObjCTypeParamDecl -> (* TODO parse *) None
             | Some ObjCExceptionAttr -> (* TODO parse *) None
             | kind ->
               raise_s [%message "Unknown kind" (kind : Ast.Kind.t option) (t : Ast.t)])
      in
      { name = Option.value_exn name
      ; super = Option.bind ~f:parse_super super
      ; file = Option.bind ~f:Ast.Loc.file loc
      ; protocols =
          Option.value protocols ~default:[]
          |> List.map ~f:parse_protocol
          |> String.Set.of_list
      ; contents
      }
    in
    Ast.Parser.collect_exn parser ~type_:"Class"
  ;;
end

let run (ast : Ast.t) =
  let classes =
    List.filter_map (Option.value ~default:[] ast.inner) ~f:(fun ast ->
      match ast.kind with
      | Some ObjCInterfaceDecl -> Class.parse ast |> Some
      | _ -> None)
  in
  (* TODO protocols *)
  classes |> [%sexp_of: Class.t list] |> print_s
;;

let command =
  Command.basic ~summary:""
  @@
  let%map_open.Command file = anon ("file" %: string) in
  fun () -> Ast.load_exn ~file |> run
;;
