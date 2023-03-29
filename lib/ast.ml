open! Core

module Of_sexpable (M : Sexpable.S) = struct
  include Sexpable.To_stringable (M)

  let t_of_yojson yojson = string_of_yojson yojson |> of_string
  let yojson_of_t t = to_string t |> yojson_of_string
end

module Of_sexpable_uncapitalize (M : Sexpable.S) = struct
  include Sexpable.To_stringable (M)

  let t_of_yojson yojson = string_of_yojson yojson |> of_string
  let yojson_of_t t = to_string t |> String.capitalize |> yojson_of_string
end

module Base_yojson = struct
  type t = Yojson.Safe.t

  let yojson_of_t x = x
  let t_of_yojson x = x
  let sexp_of_t x = Sexp.Atom (Yojson.Safe.to_string x)
end

module Value = struct
  type t =
    | Int of int
    | String of string
  [@@deriving sexp_of]

  let t_of_yojson = function
    | `Int i -> Int i
    | `String s -> String s
    | json -> raise_s [%message "Bad json" (json : Base_yojson.t)]
  ;;

  let yojson_of_t = function
    | Int i -> yojson_of_int i
    | String s -> yojson_of_string s
  ;;
end

module Type = struct
  type t =
    { qualType : string
    ; desugaredQualType : string option [@yojson.option] [@sexp.option]
    ; typeAliasDeclId : string option [@yojson.option] [@sexp.option]
    }
  [@@deriving sexp_of, yojson]
end

module Loc = struct
  module Included_from = struct
    type t = { file : string } [@@deriving sexp_of, yojson]
  end

  type t =
    { offset : int option [@yojson.option] [@sexp.option]
    ; file : string option [@yojson.option] [@sexp.option]
    ; includedFrom : Included_from.t option [@yojson.option] [@sexp.option]
    ; line : int option [@yojson.option] [@sexp.option]
    ; presumedLine : int option [@yojson.option] [@sexp.option]
    ; col : int option [@yojson.option] [@sexp.option]
    ; tokLen : int option [@yojson.option] [@sexp.option]
    ; spellingLoc : t option [@yojson.option] [@sexp.option]
    ; expansionLoc : t option [@yojson.option] [@sexp.option]
    ; isMacroArgExpansion : bool option [@yojson.option] [@sexp.option]
    }
  [@@deriving sexp_of, yojson, fields]
end

module Range = struct
  type t =
    { begin_ : Loc.t [@yojson.key "begin"]
    ; end_ : Loc.t [@yojson.key "end"]
    }
  [@@deriving sexp_of, yojson]
end

module Cast_kind = struct
  module T = struct
    type t =
      | ArrayToPointerDecay
      | BitCast
      | BuiltinFnToFnPtr
      | CPointerToObjCPointerCast
      | FloatingCast
      | FunctionToPointerDecay
      | IntegralCast
      | IntegralToBoolean
      | IntegralToFloating
      | IntegralToPointer
      | LValueToRValue
      | NoOp
      | NullToPointer
      | PointerToIntegral
      | ToVoid
    [@@deriving sexp]
  end

  include T
  include Of_sexpable (T)
end

module Kind = struct
  module T = struct
    type t =
      | AlignedAttr
      | AllocSizeAttr
      | AlwaysInlineAttr
      | ArcWeakrefUnavailableAttr
      | ArraySubscriptExpr
      | AsmLabelAttr
      | AttributedType
      | AvailabilityAttr
      | BinaryOperator
      | BlockPointerType
      | BreakStmt
      | BuiltinAttr
      | BuiltinType
      | CFAuditedTransferAttr
      | CFConsumedAttr
      | CFReturnsNotRetainedAttr
      | CFReturnsRetainedAttr
      | CStyleCastExpr
      | CallExpr
      | CaseStmt
      | CharacterLiteral
      | ColdAttr
      | CompoundAssignOperator
      | CompoundStmt
      | ConditionalOperator
      | ConstAttr
      | ConstantArrayType
      | ConstantExpr
      | DecayedType
      | DeclRefExpr
      | DeclStmt
      | DefaultStmt
      | DeprecatedAttr
      | DisableTailCallsAttr
      | ElaboratedType
      | EmptyDecl
      | EnumConstantDecl
      | EnumDecl
      | EnumExtensibilityAttr
      | EnumType
      | FieldDecl
      | FlagEnumAttr
      | FloatingLiteral
      | FormatArgAttr
      | FormatAttr
      | FunctionDecl
      | FunctionProtoType
      | GCCAsmStmt
      | IBActionAttr
      | IBOutletAttr
      | IfStmt
      | ImplicitCastExpr
      | IncompleteArrayType
      | IndirectFieldDecl
      | InitListExpr
      | IntegerLiteral
      | MaxFieldAlignmentAttr
      | MemberExpr
      | NSConsumedAttr
      | NSConsumesSelfAttr
      | NSErrorDomainAttr
      | NSReturnsRetainedAttr
      | NoEscapeAttr
      | NoThrowAttr
      | NonNullAttr
      | NotTailCalledAttr
      | ObjCBoolLiteralExpr
      | ObjCBoxableAttr
      | ObjCBridgeAttr
      | ObjCBridgeMutableAttr
      | ObjCBridgeRelatedAttr
      | ObjCCategoryDecl
      | ObjCDesignatedInitializerAttr
      | ObjCExceptionAttr
      | ObjCExplicitProtocolImplAttr
      | ObjCIndependentClassAttr
      | ObjCInterfaceDecl
      | ObjCInterfaceType
      | ObjCIvarDecl
      | ObjCMessageExpr
      | ObjCMethodDecl
      | ObjCObjectPointerType
      | ObjCObjectType
      | ObjCPropertyDecl
      | ObjCProtocolDecl
      | ObjCRequiresPropertyDefsAttr
      | ObjCRequiresSuperAttr
      | ObjCReturnsInnerPointerAttr
      | ObjCRootClassAttr
      | ObjCTypeParamDecl
      | PackedAttr
      | ParenExpr
      | ParenType
      | ParmVarDecl
      | PointerType
      | PureAttr
      | QualType
      | RecordDecl
      | RecordType
      | RestrictAttr
      | ReturnStmt
      | ReturnsTwiceAttr
      | SentinelAttr
      | SwiftAsyncAttr
      | SwiftAsyncNameAttr
      | SwiftAttrAttr
      | SwiftBridgedTypedefAttr
      | SwiftErrorAttr
      | SwiftNameAttr
      | SwiftNewTypeAttr
      | SwiftPrivateAttr
      | SwitchStmt
      | TranslationUnitDecl
      | TransparentUnionAttr
      | TypedefDecl
      | TypedefType
      | UnaryExprOrTypeTraitExpr
      | UnaryOperator
      | UnavailableAttr
      | UnusedAttr
      | UsedAttr
      | VarDecl
      | VisibilityAttr
      | WarnUnusedResultAttr
      | WeakImportAttr
    [@@deriving sexp, equal]
  end

  include T
  include Of_sexpable (T)
end

module Storage_class = struct
  module T = struct
    type t =
      | Extern
      | Static
    [@@deriving sexp]
  end

  include T
  include Of_sexpable_uncapitalize (T)
end

module Value_category = struct
  module T = struct
    type t =
      | Lvalue
      | Prvalue
    [@@deriving sexp]
  end

  include T
  include Of_sexpable_uncapitalize (T)
end

module Tag_used = struct
  module T = struct
    type t =
      | Struct
      | Union
    [@@deriving sexp]
  end

  include T
  include Of_sexpable_uncapitalize (T)
end

module Qualifiers = struct
  module T = struct
    type t =
      | Const
      | Volatile
    [@@deriving sexp]
  end

  include T
  include Of_sexpable_uncapitalize (T)
end

module Cc = struct
  module T = struct
    type t = Cdecl [@@deriving sexp]
  end

  include T
  include Of_sexpable_uncapitalize (T)
end

module Init = struct
  module T = struct
    type t = C [@@deriving sexp]
  end

  include T
  include Of_sexpable_uncapitalize (T)
end

module Control = struct
  module T = struct
    type t =
      | Optional
      | Required
    [@@deriving sexp, compare]
  end

  include T
  include Of_sexpable_uncapitalize (T)
end

module Access = struct
  module T = struct
    type t =
      | Package
      | Private
      | Protected
    [@@deriving sexp]
  end

  include T
  include Of_sexpable_uncapitalize (T)
end

module Receiver_kind = struct
  module T = struct
    type t = Instance [@@deriving sexp]
  end

  include T
  include Of_sexpable_uncapitalize (T)
end

module Variance = struct
  module T = struct
    type t =
      | Covariant
      | Contravariant
    [@@deriving sexp, compare]
  end

  include T
  include Of_sexpable_uncapitalize (T)
end

type t =
  { id : string
  ; kind : Kind.t option [@yojson.option] [@sexp.option]
  ; name : string option [@yojson.option] [@sexp.option]
  ; argType : Type.t option [@yojson.option] [@sexp.option]
  ; canOverflow : bool option [@yojson.option] [@sexp.option]
  ; castKind : Cast_kind.t option [@yojson.option] [@sexp.option]
  ; cc : Cc.t option [@yojson.option] [@sexp.option]
  ; completeDefinition : bool option [@yojson.option] [@sexp.option]
  ; computeLHSType : Type.t option [@yojson.option] [@sexp.option]
  ; computeResultType : Type.t option [@yojson.option] [@sexp.option]
  ; decl : t option [@yojson.option] [@sexp.option]
  ; fixedUnderlyingType : Type.t option [@yojson.option] [@sexp.option]
  ; hasElse : bool option [@yojson.option] [@sexp.option]
  ; implicit : bool option [@yojson.option] [@sexp.option]
  ; inherited : bool option [@yojson.option] [@sexp.option]
  ; init : Init.t option [@yojson.option] [@sexp.option]
  ; super : t option [@yojson.option] [@sexp.option]
  ; implementation : t option [@yojson.option] [@sexp.option]
  ; inline : bool option [@yojson.option] [@sexp.option]
  ; inner : t list option [@yojson.option] [@sexp.option]
  ; isArrow : bool option [@yojson.option] [@sexp.option]
  ; isBitfield : bool option [@yojson.option] [@sexp.option]
  ; isImplicit : bool option [@yojson.option] [@sexp.option]
  ; isInvalid : bool option [@yojson.option] [@sexp.option]
  ; isPartOfExplicitCast : bool option [@yojson.option] [@sexp.option]
  ; isPostfix : bool option [@yojson.option] [@sexp.option]
  ; isReferenced : bool option [@yojson.option] [@sexp.option]
  ; isUsed : bool option [@yojson.option] [@sexp.option]
  ; isValid : bool option [@yojson.option] [@sexp.option]
  ; loc : Loc.t option [@yojson.option] [@sexp.option]
  ; mangledName : string option [@yojson.option] [@sexp.option]
  ; protocols : t list option [@yojson.option] [@sexp.option]
  ; nrvo : bool option [@yojson.option] [@sexp.option]
  ; opcode : string option [@yojson.option] [@sexp.option]
  ; ownedTagDecl : t option [@yojson.option] [@sexp.option]
  ; parentDeclContextId : string option [@yojson.option] [@sexp.option]
  ; previousDecl : string option [@yojson.option] [@sexp.option]
  ; qualifiers : Qualifiers.t option [@yojson.option] [@sexp.option]
  ; range : Range.t option [@yojson.option] [@sexp.option]
  ; referencedDecl : t option [@yojson.option] [@sexp.option]
  ; referencedMemberDecl : string option [@yojson.option] [@sexp.option]
  ; size : int option [@yojson.option] [@sexp.option]
  ; storageClass : Storage_class.t option [@yojson.option] [@sexp.option]
  ; tagUsed : Tag_used.t option [@yojson.option] [@sexp.option]
  ; type_ : Type.t option [@yojson.option] [@sexp.option] [@yojson.key "type"]
  ; value : Value.t option [@yojson.option] [@sexp.option]
  ; valueCategory : Value_category.t option [@yojson.option] [@sexp.option]
  ; variadic : bool option [@yojson.option] [@sexp.option]
  ; returnType : Type.t option [@yojson.option] [@sexp.option]
  ; instance : bool option [@yojson.option] [@sexp.option]
  ; readonly : bool option [@yojson.option] [@sexp.option]
  ; atomic : bool option [@yojson.option] [@sexp.option]
  ; copy : bool option [@yojson.option] [@sexp.option]
  ; control : Control.t option [@yojson.option] [@sexp.option]
  ; access : Access.t option [@yojson.option] [@sexp.option]
  ; class_ : bool option [@yojson.option] [@sexp.option] [@yojson.key "class"]
  ; interface : t option [@yojson.option] [@sexp.option]
  ; retain : bool option [@yojson.option] [@sexp.option]
  ; selector : string option [@yojson.option] [@sexp.option]
  ; receiverKind : Receiver_kind.t option [@yojson.option] [@sexp.option]
  ; nullability : bool option [@yojson.option] [@sexp.option]
  ; strong : bool option [@yojson.option] [@sexp.option]
  ; weak : bool option [@yojson.option] [@sexp.option]
  ; assign : bool option [@yojson.option] [@sexp.option]
  ; unsafe_unretained : bool option [@yojson.option] [@sexp.option]
  ; variance : Variance.t option [@yojson.option] [@sexp.option]
  ; nonatomic : bool option [@yojson.option] [@sexp.option]
  ; readwrite : bool option [@yojson.option] [@sexp.option]
  ; null_resettable : bool option [@yojson.option] [@sexp.option]
  ; getter : t option [@yojson.option] [@sexp.option]
  ; bounded : bool option [@yojson.option] [@sexp.option]
  }
[@@deriving sexp_of, yojson, fields]

module Packed_field = struct
  type ast = t
  type t = T : (ast, 'a option) Fieldslib.Field.readonly_t -> t

  let all_optional =
    Fields.
      [ T kind
      ; T name
      ; T argType
      ; T canOverflow
      ; T castKind
      ; T cc
      ; T completeDefinition
      ; T computeLHSType
      ; T computeResultType
      ; T decl
      ; T fixedUnderlyingType
      ; T hasElse
      ; T implicit
      ; T inherited
      ; T init
      ; T super
      ; T implementation
      ; T inline
      ; T inner
      ; T isArrow
      ; T isBitfield
      ; T isImplicit
      ; T isInvalid
      ; T isPartOfExplicitCast
      ; T isPostfix
      ; T isReferenced
      ; T isUsed
      ; T isValid
      ; T loc
      ; T mangledName
      ; T protocols
      ; T nrvo
      ; T opcode
      ; T ownedTagDecl
      ; T parentDeclContextId
      ; T previousDecl
      ; T qualifiers
      ; T range
      ; T referencedDecl
      ; T referencedMemberDecl
      ; T size
      ; T storageClass
      ; T tagUsed
      ; T type_
      ; T value
      ; T valueCategory
      ; T variadic
      ; T returnType
      ; T instance
      ; T readonly
      ; T atomic
      ; T copy
      ; T control
      ; T access
      ; T class_
      ; T interface
      ; T retain
      ; T selector
      ; T receiverKind
      ; T nullability
      ; T strong
      ; T weak
      ; T assign
      ; T unsafe_unretained
      ; T variance
      ; T nonatomic
      ; T readwrite
      ; T null_resettable
      ; T getter
      ; T bounded
      ]
  ;;

  let%expect_test "All names included" =
    let all_names = Set.remove (String.Set.of_list Fields.names) "id" in
    let defined_names =
      List.map all_optional ~f:(fun (T field) -> Fieldslib.Field.name field)
      |> String.Set.of_list
    in
    assert (Set.equal all_names defined_names)
  ;;
end

module Parser = struct
  type ast = t

  module T = struct
    type 'a t =
      | Field : (ast, 'a) Fieldslib.Field.readonly_t -> 'a t
      | Return : 'a -> 'a t
      | Map2 : 'a t * 'b t * ('a -> 'b -> 'c) -> 'c t

    let return x = Return x
    let map2 a b ~f = Map2 (a, b, f)
    let map = `Define_using_map2
  end

  module A = struct
    include T
    include Applicative.Make_using_map2 (T)
  end

  include A

  module Open_on_rhs = struct
    module type S = sig
      include module type of Fields
      include Applicative.S with type 'a t := 'a t

      val field : (ast, 'a) Fieldslib.Field.readonly_t -> 'a t

      val list_option_field
        :  (ast, 'a list option) Fieldslib.Field.readonly_t
        -> 'a list t
      val bool_option_field
        :  (ast, bool option) Fieldslib.Field.readonly_t
        -> bool t
    end

    include (
      struct
        include Fields
        include A

        let field x = Field x
        let list_option_field x = A.map (Field x) ~f:(Option.value ~default:[])
        let bool_option_field x = A.map (Field x) ~f:(Option.value ~default:false)
      end :
        S)
  end

  include Applicative.Make_let_syntax (A) (Open_on_rhs) (Open_on_rhs)

  let all_to_check =
    List.map Packed_field.all_optional ~f:(fun (T field) -> Fieldslib.Field.name field)
    |> String.Set.of_list
  ;;

  let collect_exn t ~type_ (ast : ast) =
    let rec run : type a. a t -> String.Set.t * a = function
      | Return a -> String.Set.empty, a
      | Field field ->
        String.Set.singleton (Fieldslib.Field.name field), Fieldslib.Field.get field ast
      | Map2 (a, b, f) ->
        let set_a, a = run a in
        let set_b, b = run b in
        Set.union set_a set_b, f a b
    in
    let used, result = run t in
    let unused =
      List.filter_map Packed_field.all_optional ~f:(fun (T field) ->
        let name = Fieldslib.Field.name field in
        if Set.mem used name
        then None
        else (
          let%map.Option _ = Fieldslib.Field.get field ast in
          name))
    in
    if List.is_empty unused
    then result
    else raise_s [%message "Unused fields" type_ (ast : t) (unused : string list)]
  ;;
end

let rec collect_all ~nesting t ~f =
  let collect_all = collect_all ~nesting:(nesting + 1) in
  List.concat
    [ f ~nesting t
    ; Option.value_map ~f:(collect_all ~f) t.decl ~default:[]
    ; Option.value t.inner ~default:[] |> List.concat_map ~f:(collect_all ~f)
    ; Option.value t.protocols ~default:[] |> List.concat_map ~f:(collect_all ~f)
    ; Option.value_map t.ownedTagDecl ~f:(collect_all ~f) ~default:[]
    ; Option.value_map t.referencedDecl ~f:(collect_all ~f) ~default:[]
    ; Option.value_map t.super ~f:(collect_all ~f) ~default:[]
    ; Option.value_map t.implementation ~f:(collect_all ~f) ~default:[]
    ; Option.value_map t.interface ~f:(collect_all ~f) ~default:[]
    ; Option.value_map t.getter ~f:(collect_all ~f) ~default:[]
    ]
;;

let to_hierarchy t =
  collect_all ~nesting:0 t ~f:(fun ~nesting t ->
    Option.value_map
      t.name
      ~f:(fun name ->
        [ String.init nesting ~f:(const ' '); sprintf "%2d. %s" nesting name ]
        |> String.concat
        |> List.return)
      ~default:[])
;;

let load_exn ~file = Yojson.Safe.from_file file |> t_of_yojson

let command =
  Command.basic ~summary:""
  @@
  let%map_open.Command file = anon ("file" %: string) in
  fun () -> load_exn ~file |> to_hierarchy |> List.iter ~f:print_endline
;;

let parse () =
  try
    In_channel.input_all In_channel.stdin
    |> Yojson.Safe.from_string
    |> t_of_yojson
    (*|> collect_all ~f:(fun t -> Option.to_list t.variance)
    |> String.Set.of_list
    |> Set.iter ~f:print_endline
    *)
    |> sexp_of_t
    |> print_s
  with
  | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, json) ->
    let json_prefix = String.prefix (Yojson.Safe.to_string json) 256 in
    print_s [%message (exn : Exn.t) (json_prefix : string)]
;;
