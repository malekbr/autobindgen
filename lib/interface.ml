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
  module Availability = struct
    type t =
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
      | Api_unavailable of string list
    [@@deriving sexp_of, compare]
  end

  module Token = struct
    module Basic = struct
      type t =
        | Signed
        | Unsigned
        | Char
        | Short
        | Int
        | Long
      [@@deriving sexp_of, compare]
    end

    type t =
      | Basic of Basic.t
      | Void
      | Const
      | Star
      | Nonnull
      | Nullable
      | Null_unspecified
      | Nullable_result
      | Refined_for_swift
      | Returns_not_retained
      | Swift_ui_actor
      | NS_unavailable
      | TVOS_prohibited
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
      | Returns_inner_pointer
      | Availability of Availability.t
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
        Deprecated { message; target; start_version; end_version } |> Availability
      ;;

      let api_available_parser =
        let%map.Angstrom target =
          string "API_AVAILABLE(" *> take_while1 Char.is_alpha <* char '('
        and version = version <* string "))" in
        Api_available { target; version } |> Availability
      ;;

      let ns_swift_unavailable_from_async_parser =
        let%map.Angstrom message =
          string "NS_SWIFT_UNAVAILABLE_FROM_ASYNC(" *> quoted_string <* char ')'
        in
        NS_swift_unavailable_from_async message |> Availability
      ;;

      let ns_available =
        let%map.Angstrom macos =
          string "NS_AVAILABLE(" *> underscore_version <* call_separator
        and ios = underscore_version <* char ')' in
        NS_available { macos = Some macos; ios = Some ios } |> Availability
      ;;

      let ns_available_mac =
        let%map.Angstrom macos =
          string "NS_AVAILABLE_MAC(" *> underscore_version <* char ')'
        in
        NS_available { macos = Some macos; ios = None } |> Availability
      ;;

      let ns_available_ios =
        let%map.Angstrom ios =
          string "NS_AVAILABLE_MAC(" *> underscore_version <* char ')'
        in
        NS_available { macos = None; ios = Some ios } |> Availability
      ;;

      let api_unavailable =
        let%map.Angstrom apis =
          string "API_UNAVAILABLE("
          *> sep_by1
               call_separator
               (skip_while Char.is_whitespace *> take_while1 Char.is_alpha
                <* skip_while Char.is_whitespace)
          <* char ')'
        in
        Api_unavailable apis |> Availability
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
             [ "signed", Basic Signed
             ; "unsigned", Basic Unsigned
             ; "char", Basic Char
             ; "short", Basic Short
             ; "int", Basic Int
             ; "long", Basic Long
             ; "const", Const
             ; "void", Void
             ; "_Nonnull", Nonnull
             ; "_Nullable", Nullable
             ; "_Null_unspecified", Null_unspecified
             ; "_Nullable_result", Nullable_result
             ; "__TVOS_PROHIBITED", TVOS_prohibited
             ; "NS_REFINED_FOR_SWIFT", Refined_for_swift
             ; "NS_SWIFT_UI_ACTOR", Swift_ui_actor
             ; "NS_UNAVAILABLE", NS_unavailable
             ; "CF_RETURNS_NOT_RETAINED", Returns_not_retained
             ; "struct", Struct
             ; "enum", Enum
             ; "instancetype", Instance_type
             ; "NS_RETURNS_INNER_POINTER", Returns_inner_pointer
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
          ; api_unavailable
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
          { nullable : [ `Nullable | `Nonnull | `Unspecified ]
          ; ty : t
          }
      | Nullable_result of t
      | Refined_for_swift of t
      | Swift_ui_actor of t
      | NS_unavailable of t
      | Returns_not_retained of t
      | Struct of string
      | Enum of string
      | Instance_type
      | Type_params of
          { name : string
          ; parameters : t list
          }
      | Prohibited of [ `TVOS ] * t
      | Function_pointer of
          { return_type : t
          ; arguments : t list
          }
      | Block of
          { return_type : t
          ; arguments : t list
          }
      | Kind_of of t
      | Returns_inner_pointer of t
    [@@deriving sexp_of, compare, variants]
  end

  (* Parse more later *)
  module Parsed = struct
    type t =
      { original : string
      ; tokens : Token.t list
      ; specs : Specs.t
      ; availability : Availability.t option [@sexp.option]
      }
    [@@deriving sexp_of, compare, fields]

    let basic_of_last_token : Token.Basic.t -> Specs.Signedness.t option * Specs.Basic.t
      = function
      | Char -> None, Char
      | Short -> None, Short
      | Int -> None, Int
      | Long -> None, Long
      | Signed -> Some Signed, Int
      | Unsigned -> Some Unsigned, Int
    ;;

    let rec keep_parsing_basic
      (tokens_rev : Token.t list)
      ((signs, specs) : Specs.Signedness.t option * Specs.Basic.t)
      =
      match tokens_rev with
      | Basic prefix :: rest ->
        let raise_unexpected_basic_prefix () =
          raise_s
            [%message
              "Unexpected basic prefix"
                (prefix : Token.Basic.t)
                (signs : Specs.Signedness.t option)
                (specs : Specs.Basic.t)]
        in
        let result : Specs.Signedness.t option * Specs.Basic.t =
          match prefix with
          | Char | Int -> raise_unexpected_basic_prefix ()
          | Short ->
            (match specs with
             | Int -> signs, Short
             | _ -> raise_unexpected_basic_prefix ())
          | Long ->
            (match specs with
             | Int -> signs, Long
             | Long -> signs, Long_long
             | _ -> raise_unexpected_basic_prefix ())
          | Signed ->
            (match signs with
             | None -> Some Signed, specs
             | Some _ -> raise_unexpected_basic_prefix ())
          | Unsigned ->
            (match signs with
             | None -> Some Unsigned, specs
             | Some _ -> raise_unexpected_basic_prefix ())
        in
        keep_parsing_basic rest result
      | rest -> Specs.Basic (signs, specs), rest
    ;;

    let rec parse context ~(tokens_rev : Token.t list) =
      let unexpected_in_context () =
        raise_s
          [%message
            "Unexpected in context"
              (List.hd tokens_rev : Token.t option)
              (context
                : [ `Type of Specs.t
                  | `Building_args of Specs.t list
                  | `Building_type_params of Specs.t list
                  | `Apply_next of _
                  | `Built_args of Specs.t list
                  | `Built_type_params of Specs.t list
                  | `Function_or_block of Specs.t list
                  ]
                  list)]
      in
      (* Only call when needed *)
      let rec reduce context =
        match context with
        | `Type ty :: `Apply_next f :: rest -> `Type (f ty) :: rest |> reduce
        | `Type ty :: `Built_type_params parameters :: rest ->
          let name =
            match ty with
            | Specs.Named name -> name
            | ty ->
              raise_s
                [%message
                  "Only simple named types can have type paramaeters" (ty : Specs.t)]
          in
          `Type (Specs.type_params ~name ~parameters) :: rest |> reduce
        | `Type _
          :: ( `Building_args _
             | `Type _
             | `Building_type_params _
             | `Built_args _
             | `Function_or_block _ )
          :: _
        | `Type _ :: _ -> context
        | `Apply_next f :: `Apply_next g :: context ->
          `Apply_next (Fn.compose f g) :: context |> reduce
        | ( `Building_args _
          | `Apply_next _
          | `Building_type_params _
          | `Built_args _
          | `Function_or_block _
          | `Built_type_params _ )
          :: _
        | [] -> context
      in
      let modify_last_type ~f =
        match reduce context with
        | `Type ty :: rest -> `Type (f ty) :: rest
        | `Apply_next apply_next :: rest ->
          `Apply_next (fun (ty : Specs.t) -> apply_next ty |> f) :: rest
        | _ -> unexpected_in_context ()
      in
      let schedule_apply f = `Apply_next f :: context in
      let add_type ty =
        match context with
        | `Apply_next f :: rest -> `Type (f ty) :: rest
        | context -> `Type ty :: context
      in
      let make_function_apply_next ~creator rest =
        let context =
          match reduce context with
          | `Apply_next f :: `Function_or_block arguments :: context ->
            `Apply_next (fun return_type -> creator ~return_type ~arguments |> f)
            :: context
          | `Function_or_block arguments :: context ->
            `Apply_next (fun return_type -> creator ~return_type ~arguments) :: context
          | _ -> unexpected_in_context ()
        in
        parse context ~tokens_rev:rest
      in
      match tokens_rev with
      | [] ->
        (match reduce context with
         | [ `Type ty ] -> ty
         | _ -> unexpected_in_context ())
      | Void :: rest -> parse (add_type Void) ~tokens_rev:rest
      | Instance_type :: rest -> parse (add_type Instance_type) ~tokens_rev:rest
      | Named name :: Struct :: rest -> parse (add_type (Struct name)) ~tokens_rev:rest
      | Named name :: Enum :: rest -> parse (add_type (Enum name)) ~tokens_rev:rest
      | (Struct | Enum) :: _ -> unexpected_in_context ()
      | Named name :: rest -> parse (add_type (Named name)) ~tokens_rev:rest
      | Availability _ :: _ -> unexpected_in_context ()
      | Basic start :: rest ->
        let type_, rest = basic_of_last_token start |> keep_parsing_basic rest in
        parse (add_type type_) ~tokens_rev:rest
      | Const :: rest -> parse (modify_last_type ~f:Specs.const) ~tokens_rev:rest
      | Caret :: Open_paren :: rest -> make_function_apply_next ~creator:Specs.block rest
      | Caret :: _ -> unexpected_in_context ()
      | Star :: Open_paren :: rest ->
        make_function_apply_next ~creator:Specs.function_pointer rest
      | Star :: rest -> parse (schedule_apply Specs.pointer) ~tokens_rev:rest
      | Nonnull :: rest ->
        parse
          (schedule_apply (fun ty -> Specs.nullability ~nullable:`Nonnull ~ty))
          ~tokens_rev:rest
      | Nullable :: rest ->
        parse
          (schedule_apply (fun ty -> Specs.nullability ~nullable:`Nullable ~ty))
          ~tokens_rev:rest
      | Null_unspecified :: rest ->
        parse
          (schedule_apply (fun ty -> Specs.nullability ~nullable:`Unspecified ~ty))
          ~tokens_rev:rest
      | Nullable_result :: rest ->
        parse (schedule_apply Specs.nullable_result) ~tokens_rev:rest
      | Kind_of :: rest -> parse (modify_last_type ~f:Specs.kind_of) ~tokens_rev:rest
      | Swift_ui_actor :: rest ->
        parse (modify_last_type ~f:Specs.swift_ui_actor) ~tokens_rev:rest
      | Refined_for_swift :: rest ->
        parse (modify_last_type ~f:Specs.refined_for_swift) ~tokens_rev:rest
      | Returns_inner_pointer :: rest ->
        parse (modify_last_type ~f:Specs.returns_inner_pointer) ~tokens_rev:rest
      | Returns_not_retained :: rest ->
        parse (modify_last_type ~f:Specs.returns_not_retained) ~tokens_rev:rest
      | NS_unavailable :: rest ->
        parse (modify_last_type ~f:Specs.ns_unavailable) ~tokens_rev:rest
      | TVOS_prohibited :: rest ->
        parse (modify_last_type ~f:(Specs.prohibited `TVOS)) ~tokens_rev:rest
      | Comma :: rest ->
        let context =
          match reduce context with
          | `Type ty :: `Building_args args :: context ->
            `Building_args (ty :: args) :: context
          | `Type ty :: `Building_type_params args :: context ->
            `Building_type_params (ty :: args) :: context
          | _ -> unexpected_in_context ()
        in
        parse context ~tokens_rev:rest
      | Close_bracket :: rest ->
        parse (`Building_type_params [] :: context) ~tokens_rev:rest
      | Open_bracket :: rest ->
        (match reduce context with
         | `Type ty :: `Building_type_params params :: context ->
           parse (`Built_type_params (ty :: params) :: context) ~tokens_rev:rest
         | _ -> unexpected_in_context ())
      | Close_paren :: rest ->
        let context =
          match reduce context with
          | `Built_args args :: context -> `Function_or_block args :: context
          | context -> `Building_args [] :: context
        in
        parse context ~tokens_rev:rest
      | Open_paren :: rest ->
        (match reduce context with
         | `Type ty :: `Building_args args :: context ->
           parse (`Built_args (ty :: args) :: context) ~tokens_rev:rest
         | _ -> unexpected_in_context ())
    ;;

    let availability_and_specs_of_tokens = function
      | Token.Availability availability :: rest ->
        Some availability, parse [] ~tokens_rev:(List.rev rest)
      | tokens -> None, parse [] ~tokens_rev:(List.rev tokens)
    ;;

    let parse original =
      let tokens = Token.parse original in
      let availability, specs = availability_and_specs_of_tokens tokens in
      { original; tokens = Token.parse original; specs; availability }
    ;;
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

  let fold_types t ~init ~f = f init t.type_

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

  let fold_types t ~init ~f = f init t.type_
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

    let fold_types t ~init ~f = f init t.type_

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

    let fold_types (t : t) ~init ~f =
      match t with
      | `Initializer
      | `Deprecated
      | `Unavailable
      | `Format_arg
      | `Format
      | `Retained
      | `Not_retained -> init
      | `Parameter param -> Parameter.fold_types param ~init ~f
      | `Type_parameter param -> Type_parameter.fold_types param ~init ~f
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

  let fold_types t ~init ~f =
    let init = f init t.return_type in
    List.fold t.attributes ~init ~f:(fun init attr -> Attribute.fold_types ~init ~f attr)
  ;;

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

  let fold_types (_ : t) ~init ~f:_ = init
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

  let fold_types (t : t) ~init ~f =
    match t with
    | `Property property -> Property.fold_types ~init ~f property
    | `Method method_ -> Method.fold_types ~init ~f method_
    | `Type_parameter type_parameter -> Type_parameter.fold_types ~init ~f type_parameter
    | `Exception exn -> Exception.fold_types ~init ~f exn
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

  let fold_types t ~init ~f =
    List.fold t.contents ~init ~f:(fun init v ->
      Class_protocol_content.fold_types ~init ~f v)
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

  let fold_types t ~init ~f =
    List.fold t.contents ~init ~f:(fun init v ->
      Class_protocol_content.fold_types ~init ~f v)
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

  let fold_types t ~init ~f =
    let init = Option.fold t.fixed_underlying_type ~f ~init in
    List.fold t.contents ~init ~f:(fun init -> function
      | `Flag_enum -> init
      | `Value { name = _; type_; is_referenced = _ } -> f init type_)
  ;;

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

type t =
  | Class of Class.t
  | Protocol of Protocol.t
  | Enum of Enum.t
[@@deriving sexp_of, compare, variants]

let fold_types t ~init ~f =
  match t with
  | Class class_ -> Class.fold_types class_ ~init ~f
  | Protocol protocol -> Protocol.fold_types protocol ~init ~f
  | Enum enum -> Enum.fold_types enum ~init ~f
;;

let parse_top_level_ast (ast : Ast.t) =
  List.filter_map (Option.value ~default:[] ast.inner) ~f:(fun ast ->
    match ast.kind with
    | Some ObjCInterfaceDecl -> Class (Class.parse ast) |> Some
    | Some ObjCProtocolDecl -> Protocol (Protocol.parse ast) |> Some
    | Some EnumDecl -> Enum.parse ast |> Option.map ~f:(fun enum -> Enum enum)
    | _ -> None)
;;

let collect_all_types ts =
  List.fold ts ~init:[] ~f:(fun init t ->
    fold_types t ~init ~f:(fun types ty -> ty :: types))
;;

let run (ast : Ast.t) ~action =
  let ts = parse_top_level_ast ast in
  match action with
  | `Dump -> print_s [%sexp (ts : t list)]
  | `List_types ->
    let csv_columns =
      [ Csv_printer.column "Type" Type.Parsed.original
      ; Csv_printer.sexpable
          "Parsed"
          (Fn.compose [%sexp_of: Type.Specs.t] Type.Parsed.specs)
      ; Csv_printer.sexpable
          "Availability"
          (Fn.compose [%sexp_of: Type.Availability.t option] Type.Parsed.availability)
      ; Csv_printer.sexpable
          "Tokens"
          (Fn.compose [%sexp_of: Type.Token.t list] Type.Parsed.tokens)
      ]
    in
    collect_all_types ts
    |> List.concat_map ~f:(fun t ->
         List.filter_opt [ t.desugared_type; Some t.specified_type ])
    |> Csv_printer.to_csv csv_columns
    |> Csv.output_all (Csv.to_channel Out_channel.stdout)
;;

let command =
  Command.basic ~summary:""
  @@
  let%map_open.Command file = anon ("file" %: string)
  and action =
    choose_one
      [ flag "dump" (no_arg_some `Dump) ~doc:" dump parsed AST"
      ; flag "list-types" (no_arg_some `List_types) ~doc:" list parsed types"
      ]
      ~if_nothing_chosen:Raise
  in
  fun () -> Ast.load_exn ~file |> run ~action
;;
