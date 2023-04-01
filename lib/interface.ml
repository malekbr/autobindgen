open! Core

let check_kind ?(ignore_fields = []) expected =
  match%map_open.Ast.Parser
    let%map.Ast.Parser kind = field kind
    and () =
      List.map ignore_fields ~f:(fun (Ast.Packed_field.T x) -> field x |> map ~f:ignore)
      |> Ast.Parser.all_unit
    in
    kind
  with
  | Some record_kind when Ast.Kind.equal record_kind expected -> ()
  | kind ->
    raise_s
      [%message "Unexpected kind" (kind : Ast.Kind.t option) (expected : Ast.Kind.t)]
;;

module Type = struct
  module Specs = struct
    module Basic = struct
      type t =
        | Char
        | Short
        | Int
        | Long
        | Long_long
      [@@deriving sexp_of, compare]
    end

    module Signedness = struct
      type t =
        | Signed
        | Unsigned
      [@@deriving sexp_of, compare]
    end

    type t =
      | Void
      | Basic of Signedness.t option * Basic.t
      | Named of string
      | Const of t
      | Pointer of t
      | Nullability of
          { nullable : bool
          ; ty : t
          }
      | Struct of string
      | Enum of string
      | Instance_type
      | Type_params of
          { name : string
          ; parameters : t list
          }
      | Function_pointer of
          { return_type : t
          ; arguments : t list
          }
      | Block of
          { return_type : t
          ; arguments : t list
          }
      | Kind_of of t
      | Deprecated of
          { message : string
          ; start_version : string
          ; end_version : string option
          ; target : string
          ; type_ : t
          }
    [@@deriving sexp_of, compare, variants]
  end

  module Token = struct
    type t =
      | Signed
      | Unsigned
      | Char
      | Short
      | Int
      | Long
      | Void
      | Const
      | Star
      | Nonnull
      | Nullable
      | Struct
      | Enum
      | Instance_type
      | Open_paren
      | Close_paren
      | Open_bracket
      | Close_bracket
      | Caret
      | Comma
      | Kind_of
      | Deprecated of
          { message : string
          ; start_version : string
          ; end_version : string option
          ; target : string
          }
      | Api_available of
          { target : string
          ; version : string
          }
      | NS_swift_unavailable_from_async of string
      | NS_available of
          { macos : string option
          ; ios : string option
          }
      | Named of string
    [@@deriving sexp_of, compare]

    module Parser : sig
      val tokens : t list Angstrom.t
    end = struct
      open! Angstrom

      let quoted_string =
        let%map.Angstrom string =
          consumed
            (char '"'
             *> Angstrom.scan_string `Unescaped (fun state c ->
                  match state, c with
                  | `Escaped, _ -> Some `Unescaped
                  | `Unescaped, '"' -> None
                  | `Unescaped, '\\' -> Some `Escaped
                  | `Unescaped, _ -> Some `Unescaped)
             <* char '"')
        in
        Scanf.sscanf string "%S" Fn.id
      ;;

      let version = consumed (sep_by1 (char '.') (skip_many1 (satisfy Char.is_digit)))

      let underscore_version =
        consumed (sep_by1 (char '_') (skip_many1 (satisfy Char.is_digit)))
      ;;

      let call_separator = char ',' <* skip_many (char ' ')

      let deprecated_parser =
        let%map.Angstrom message =
          string "API_DEPRECATED(" *> quoted_string <* call_separator
        and target = take_while1 Char.is_alpha <* char '('
        and start_version = version <* call_separator
        and end_version =
          map version ~f:Option.return
          <|> string "API_TO_BE_DEPRECATED" *> return None
          <* string "))"
        in
        Deprecated { message; target; start_version; end_version }
      ;;

      let api_available_parser =
        let%map.Angstrom target =
          string "API_AVAILABLE(" *> take_while1 Char.is_alpha <* char '('
        and version = version <* string "))" in
        Api_available { target; version }
      ;;

      let ns_swift_unavailable_from_async_parser =
        let%map.Angstrom message =
          string "NS_SWIFT_UNAVAILABLE_FROM_ASYNC(" *> quoted_string <* char ')'
        in
        NS_swift_unavailable_from_async message
      ;;

      let ns_available =
        let%map.Angstrom macos =
          string "NS_AVAILABLE(" *> underscore_version <* call_separator
        and ios = underscore_version <* char ')' in
        NS_available { macos = Some macos; ios = Some ios }
      ;;

      let ns_available_mac =
        let%map.Angstrom macos =
          string "NS_AVAILABLE_MAC(" *> underscore_version <* char ')'
        in
        NS_available { macos = Some macos; ios = None }
      ;;

      let ns_available_ios =
        let%map.Angstrom ios =
          string "NS_AVAILABLE_MAC(" *> underscore_version <* char ')'
        in
        NS_available { macos = None; ios = Some ios }
      ;;

      let valid_mid_char c = Char.equal c '_' || Char.is_alphanum c

      let maybe_type_name =
        consumed
          (satisfy (fun c -> Char.equal c '_' || Char.is_alpha c)
           *> skip_while valid_mid_char)
      ;;

      let end_of_word =
        match%bind.Angstrom peek_char with
        | Some c when valid_mid_char c -> fail "Keyword can't fail with valid character"
        | _ -> return ()
      ;;

      let token =
        choice
        @@ List.map
             ~f:(fun (str, v) -> string str *> end_of_word *> return v)
             [ "signed", Signed
             ; "unsigned", Unsigned
             ; "char", Char
             ; "short", Short
             ; "int", Int
             ; "long", Long
             ; "const", Const
             ; "void", Void
             ; "_Nonnull", Nonnull
             ; "_Nullable", Nullable
             ; "struct", Struct
             ; "enum", Enum
             ; "instancetype", Instance_type
             ; "__kindof", Kind_of
             ]
        @ List.map
            ~f:(fun (str, v) -> string str *> return v)
            [ "*", Star
            ; "(", Open_paren
            ; ")", Close_paren
            ; "^", Caret
            ; ",", Comma
            ; "<", Open_bracket
            ; ">", Close_bracket
            ]
        @ [ deprecated_parser
          ; ns_swift_unavailable_from_async_parser
          ; api_available_parser
          ; ns_available
          ; ns_available_mac
          ; ns_available_ios
          ; map maybe_type_name ~f:(fun type_name -> Named type_name)
          ]
      ;;

      let tokens = sep_by1 (take_while Char.is_whitespace) token
    end

    let parse input =
      Angstrom.parse_string Parser.tokens input ~consume:All
      |> Result.map_error ~f:(fun error ->
           let parsed_so_far =
             Angstrom.parse_string Parser.tokens input ~consume:Prefix
           in
           Error.create_s
             [%message
               "Failed to parse"
                 (input : string)
                 (error : string)
                 (parsed_so_far : (t list, string) Result.t)])
      |> ok_exn
    ;;
  end

  (* Parse more later *)
  module Parsed = struct
    type t =
      { original : string
      ; tokens : Token.t list
      }
    [@@deriving sexp_of, compare]

    let parse original = { original; tokens = Token.parse original }
  end

  type t =
    { specified_type : Parsed.t
    ; desugared_type : Parsed.t option
    }
  [@@deriving sexp_of, compare]

  let parse { Ast.Type.qualType; desugaredQualType; typeAliasDeclId = _ } =
    { specified_type = Parsed.parse qualType
    ; desugared_type = Option.map ~f:Parsed.parse desugaredQualType
    }
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

let parse_just_kind ?ignore_fields kind_value value =
  let parser =
    let%map_open.Ast.Parser _ = field loc
    and _ = field range
    and () = check_kind ?ignore_fields kind_value in
    value
  in
  Ast.Parser.collect_exn
    parser
    ~type_:(sprintf "Just kind: %s" (Sexp.to_string [%sexp (kind_value : Ast.Kind.t)]))
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
    ; nullable : bool [@sexp.bool]
    ; strength : [ `weak | `strong ] option [@sexp.option]
    ; assign : bool [@sexp.bool]
    ; unsafe_unretained : bool [@sexp.bool]
    ; null_resettable : bool [@sexp.bool]
    ; type_ : Type.t
    ; retain : bool [@sexp.bool]
    ; class_ : bool [@sexp.bool]
    ; getter : Getter.t option [@sexp.option]
    ; control : Ast.Control.t option [@sexp.option]
    }
  [@@deriving sexp_of, compare]

  let parse =
    let parser =
      let%map_open.Ast.Parser name = field name
      and () = check_kind ObjCPropertyDecl
      and atomic = field atomic
      and readonly = field readonly
      and copy = bool_option_field copy
      and nullable = bool_option_field nullability
      and strong = field strong
      and type_ = field type_
      and assign = bool_option_field assign
      and unsafe_unretained = bool_option_field unsafe_unretained
      and readwrite = field readwrite
      and null_resettable = bool_option_field null_resettable
      and retain = bool_option_field retain
      and class_ = bool_option_field class_
      and nonatomic = field nonatomic
      and weak = field weak
      and getter = field getter
      and _ = field loc
      and _ = field range
      and control = field control
      and _inner = list_option_field inner in
      { name = Option.value_exn name
      ; atomic = only_one_bool [ atomic, true; nonatomic, false ] |> Option.value_exn
      ; retain
      ; class_
      ; strength = only_one_bool [ strong, `strong; weak, `weak ]
      ; access =
          only_one_bool [ readonly, `readonly; readwrite, `readwrite ] |> Option.value_exn
      ; copy
      ; nullable
      ; assign
      ; unsafe_unretained
      ; null_resettable
      ; type_ = Option.value_exn type_ |> Type.parse
      ; getter = Option.map ~f:Getter.parse getter
      ; control
      }
    in
    Ast.Parser.collect_exn parser ~type_:"Property"
  ;;
end

module Type_parameter = struct
  type t =
    { name : string
    ; type_ : Type.t
    ; variance : Ast.Variance.t option
    ; is_referenced : bool [@sexp.bool]
    ; bounded : bool [@sexp.bool]
    }
  [@@deriving sexp_of, compare]

  let parse =
    let parser =
      let%map_open.Ast.Parser name = field name
      and () = check_kind ObjCTypeParamDecl
      and _ = field loc
      and _ = field range
      and type_ = field type_
      and variance = field variance
      and is_referenced = field isReferenced
      and bounded = field bounded in
      { name = Option.value_exn name
      ; type_ = Option.value_exn type_ |> Type.parse
      ; variance
      ; is_referenced = Option.value is_referenced ~default:false
      ; bounded = Option.value bounded ~default:false
      }
    in
    Ast.Parser.collect_exn parser ~type_:"Method.Type_parameter"
  ;;
end

module Method = struct
  module Parameter = struct
    module Attribute = struct
      type t =
        [ `No_escape
        | `Unused
        | `Consumed
        ]
      [@@deriving sexp_of, compare]

      let parse (ast : Ast.t) =
        match ast.kind with
        | Some NoEscapeAttr -> parse_just_kind NoEscapeAttr `No_escape ast |> Some
        | Some UnusedAttr -> parse_just_kind UnusedAttr `Unused ast |> Some
        | Some NSConsumedAttr -> parse_just_kind NSConsumedAttr `Consumed ast |> Some
        | Some AvailabilityAttr -> None
        | kind ->
          raise_s
            [%message
              "Unrecognized method parameter attribute" (kind : Ast.Kind.t option)]
      ;;
    end

    type t =
      { name : string
      ; type_ : Type.t
      ; attributes : Attribute.t list [@sexp.list]
      }
    [@@deriving sexp_of, compare]

    let parse =
      let parser =
        let%map_open.Ast.Parser () = check_kind ParmVarDecl
        and _ = field loc
        and _ = field range
        and _ = field mangledName
        and inner = list_option_field inner
        and type_ = field type_
        and name = field name in
        let attributes = List.filter_map inner ~f:Attribute.parse in
        { name = Option.value_exn name
        ; type_ = Option.value_exn type_ |> Type.parse
        ; attributes
        }
      in
      Ast.Parser.collect_exn parser ~type_:"Method.Parameter"
    ;;
  end

  module Attribute = struct
    type t =
      [ `Initializer
      | `Deprecated
      | `Unavailable
      | `Format_arg
      | `Format
      | `Retained
      | `Not_retained
      | `Parameter of Parameter.t
      | `Type_parameter of Type_parameter.t
      ]
    [@@deriving sexp_of, compare]

    let parse ast =
      match ast.Ast.kind with
      | Some ObjCDesignatedInitializerAttr ->
        Some (parse_just_kind ObjCDesignatedInitializerAttr `Initializer ast)
      | Some DeprecatedAttr -> Some (parse_just_kind DeprecatedAttr `Deprecated ast)
      | Some UnavailableAttr -> Some (parse_just_kind UnavailableAttr `Unavailable ast)
      | Some FormatArgAttr -> Some (parse_just_kind FormatArgAttr `Format_arg ast)
      | Some FormatAttr -> Some (parse_just_kind FormatAttr `Format ast)
      | Some CFReturnsRetainedAttr ->
        Some (parse_just_kind CFReturnsRetainedAttr `Retained ast)
      | Some NSReturnsRetainedAttr ->
        Some (parse_just_kind NSReturnsRetainedAttr `Retained ast)
      | Some CFReturnsNotRetainedAttr ->
        Some (parse_just_kind CFReturnsNotRetainedAttr `Not_retained ast)
      | Some ParmVarDecl -> Some (`Parameter (Parameter.parse ast))
      | Some
          ( AvailabilityAttr
          | ObjCReturnsInnerPointerAttr
          | SwiftAttrAttr
          | SwiftErrorAttr
          | ObjCRequiresSuperAttr
          | SwiftAsyncAttr
          | SwiftAsyncNameAttr
          | SwiftNameAttr
          | IBActionAttr
          | IBOutletAttr
          | SentinelAttr
          | WarnUnusedResultAttr
          | SwiftPrivateAttr ) -> None
      | kind -> raise_s [%message "Unrecognized attribute" (kind : Ast.Kind.t option)]
    ;;
  end

  type t =
    { name : string
    ; return_type : Type.t
    ; instance : bool
    ; attributes : Attribute.t list
    ; implicit : bool option [@sexp.option]
    ; variadic : bool option [@sexp.option]
    }
  [@@deriving sexp_of, compare]

  let parse =
    let parser =
      let%map_open.Ast.Parser name = field name
      and () = check_kind ObjCMethodDecl
      and _ = field mangledName
      and _ = field loc
      and _ = field range
      and inner = list_option_field inner
      and return_type = field returnType
      and instance = field instance
      and implicit = field isImplicit
      and variadic = field variadic in
      { name = Option.value_exn name
      ; return_type = Option.value_exn return_type |> Type.parse
      ; instance = Option.value_exn instance
      ; attributes = List.filter_map inner ~f:Attribute.parse
      ; implicit
      ; variadic
      }
    in
    Ast.Parser.collect_exn parser ~type_:"Method"
  ;;
end

module Exception = struct
  type t = { inherited : bool } [@@deriving sexp_of, compare]

  let parse =
    let parser =
      let%map_open.Ast.Parser () = check_kind ObjCExceptionAttr
      and inherited = field inherited
      and _ = field range in
      { inherited = Option.value inherited ~default:false }
    in
    Ast.Parser.collect_exn parser ~type_:"Exception"
  ;;
end

module Class_protocol_content = struct
  type t =
    [ `Property of Property.t
    | `Method of Method.t
    | `Type_parameter of Type_parameter.t
    | `Exception of Exception.t
    ]
  [@@deriving sexp_of, compare]

  let parser =
    let%map_open.Ast.Parser inner = list_option_field inner in
    List.filter_map inner ~f:(fun t ->
      match t.kind with
      | Some
          ( AvailabilityAttr
          | VisibilityAttr
          | ObjCRootClassAttr
          | SwiftNameAttr
          | SwiftAttrAttr
          | ObjCExplicitProtocolImplAttr
          | SwiftPrivateAttr
          | ObjCRequiresPropertyDefsAttr
          | ArcWeakrefUnavailableAttr ) -> None
      | Some ObjCPropertyDecl -> `Property (Property.parse t) |> Some
      | Some ObjCMethodDecl -> `Method (Method.parse t) |> Some
      | Some ObjCIvarDecl -> (* TODO parse; this is probably private *) None
      | Some RecordDecl -> (* TODO parse; this is probably private *) None
      | Some ObjCTypeParamDecl -> `Type_parameter (Type_parameter.parse t) |> Some
      | Some ObjCExceptionAttr -> `Exception (Exception.parse t) |> Some
      | kind -> raise_s [%message "Unknown kind" (kind : Ast.Kind.t option) (t : Ast.t)])
  ;;
end

let parse_protocol_reference =
  let parser =
    let%map_open.Ast.Parser name = field name
    and () = check_kind ObjCProtocolDecl in
    Option.value_exn name
  in
  Ast.Parser.collect_exn parser ~type_:"protocol reference"
;;

let protocols_parser =
  let%map_open.Ast.Parser protocols = field protocols in
  Option.value protocols ~default:[]
  |> List.map ~f:parse_protocol_reference
  |> String.Set.of_list
;;

module Protocol = struct
  type t =
    { name : string
    ; file : string option [@sexp.option]
    ; protocols : String.Set.t
    ; contents : Class_protocol_content.t list
    }
  [@@deriving sexp_of, compare]

  let parse =
    let parser =
      let%map_open.Ast.Parser name = field name
      and () = check_kind ObjCProtocolDecl
      and _ = field isImplicit
      and _ = field range
      and _ = field mangledName
      and _ = field previousDecl
      and loc = field loc
      and protocols = protocols_parser
      and contents = Class_protocol_content.parser in
      { name = Option.value_exn name
      ; file = Option.bind ~f:Ast.Loc.file loc
      ; contents
      ; protocols
      }
    in
    Ast.Parser.collect_exn parser ~type_:"Protocol"
  ;;
end

module Class = struct
  type t =
    { name : string
    ; super : string option [@sexp.option]
    ; file : string option [@sexp.option]
    ; protocols : String.Set.t
    ; contents :
        [ `Property of Property.t
        | `Method of Method.t
        | `Type_parameter of Type_parameter.t
        | `Exception of Exception.t
        ]
        list
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
      and contents = Class_protocol_content.parser
      and loc = field loc
      and super = field super
      and implementation = field implementation
      and protocols = protocols_parser in
      Option.iter implementation ~f:implementation_parser;
      { name = Option.value_exn name
      ; super = Option.bind ~f:parse_super super
      ; file = Option.bind ~f:Ast.Loc.file loc
      ; protocols
      ; contents
      }
    in
    Ast.Parser.collect_exn parser ~type_:"Class"
  ;;
end

module Enum = struct
  module Value = struct
    type t =
      { name : string
      ; type_ : Type.t
      ; is_referenced : bool [@sexp.bool]
      }
    [@@deriving sexp_of, compare]

    let parse =
      let parser =
        let%map_open.Ast.Parser name = field name
        and () = check_kind EnumConstantDecl
        and _ = field loc
        and _ = field range
        and type_ = field type_
        and _ = field inner
        and is_referenced = bool_option_field isReferenced in
        { name = Option.value_exn name
        ; type_ = Option.value_exn type_ |> Type.parse
        ; is_referenced
        }
      in
      Ast.Parser.collect_exn parser ~type_:"Enum.Value"
    ;;
  end

  type t =
    { name : string
    ; file : string option [@sexp.option]
    ; contents : [ `Value of Value.t | `Flag_enum ] list
    ; fixed_underlying_type : Type.t option
    }
  [@@deriving sexp_of, compare]

  let parse =
    let parser =
      let%map_open.Ast.Parser name = field name
      and () = check_kind EnumDecl
      and loc = field loc
      and _ = field range
      and _ = field previousDecl
      and inner = list_option_field inner
      and fixedUnderlyingType = field fixedUnderlyingType in
      let contents =
        List.filter_map inner ~f:(fun t ->
          match t.kind with
          | Some EnumConstantDecl -> Some (`Value (Value.parse t))
          | Some
              ( EnumExtensibilityAttr
              | AvailabilityAttr
              | SwiftNameAttr
              | SwiftPrivateAttr
              | NSErrorDomainAttr ) -> None
          | Some FlagEnumAttr ->
            Some
              (parse_just_kind
                 ~ignore_fields:[ T Ast.Fields.inherited ]
                 FlagEnumAttr
                 `Flag_enum
                 t)
          | kind ->
            raise_s [%message "Unknown kind" (kind : Ast.Kind.t option) (t : Ast.t)])
      in
      let%map.Option name = name in
      { name
      ; file = Option.bind ~f:Ast.Loc.file loc
      ; contents
      ; fixed_underlying_type = Option.map ~f:Type.parse fixedUnderlyingType
      }
    in
    Ast.Parser.collect_exn parser ~type_:"Enum"
  ;;
end

let run (ast : Ast.t) =
  let classes =
    List.filter_map (Option.value ~default:[] ast.inner) ~f:(fun ast ->
      match ast.kind with
      | Some ObjCInterfaceDecl -> `Class (Class.parse ast) |> Some
      | Some ObjCProtocolDecl -> `Protocol (Protocol.parse ast) |> Some
      | Some EnumDecl -> Enum.parse ast |> Option.map ~f:(fun enum -> `Enum enum)
      | _ -> None)
  in
  (* TODO protocols *)
  classes
  |> [%sexp_of: [ `Class of Class.t | `Protocol of Protocol.t | `Enum of Enum.t ] list]
  |> print_s
;;

let command =
  Command.basic ~summary:""
  @@
  let%map_open.Command file = anon ("file" %: string) in
  fun () -> Ast.load_exn ~file |> run
;;
