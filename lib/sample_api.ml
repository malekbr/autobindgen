open! Core

module type S = sig
  module Obj : sig
    (* We don't support contravariant params, and they don't exist much
       in practice. *)
    type (-'cls, +'params) t constraint 'cls = [> ]

    (* Example: ([ `NSObject ], Nothing.t) t, ([ `NSObject | `NSArray ], 'key) t *)

    val unsafe_convert : (_, _) t -> (_, _) t
    val drop_type_params : ('cls, _) t -> ('cls, Nothing.t) t
    val ctype : ('a, 'b) t Ctypes.typ
  end

  module Is_obj : sig
    (** This is just used to simplify enforcing that a type is an object. *)
    type 'a t = 'a constraint 'a = _ Obj.t
  end

  module NSObject : sig
    module type S = sig
      type cls
      type 'params t

      val alloc : unit -> 'params t
      val init : 'params t -> 'params t
    end

    (* TODO same idea for protocols *)
    type cls = [ `NSObject ]
    type t = (cls, Nothing.t) Obj.t

    include S with type cls := cls and type 'params t := t
  end

  module NSArray : sig
    module type S = sig
      type cls
      type 'params t

      include NSObject.S with type 'params t := 'params t and type cls := cls

      val initWithObjects
        :  'params Is_obj.t t
        -> 'param Is_obj.t Ctypes.carray
        -> int
        -> 'params t
    end

    type cls =
      [ NSObject.cls
      | `NSArray
      ]

    type 'params t = (cls, 'params) Obj.t

    include S with type cls := cls and type 'params t := 'params t
  end

  module NSDictionary : sig
    module type S = sig
      type cls
      type 'params t

      include NSObject.S with type 'params t := 'params t and type cls := cls

      val initWithObjects
        :  ('key Is_obj.t * 'value Is_obj.t) t
        -> 'value Is_obj.t Ctypes.carray
        -> _ Is_obj.t Ctypes.carray (* Keys: TODO support protocols *)
        -> int
        -> ('key Is_obj.t * 'value Is_obj.t) t

      val objectForKey
        :  ('key Is_obj.t * 'value Is_obj.t) t
        -> 'key Is_obj.t
        -> 'value Is_obj.t option
    end

    type cls =
      [ NSObject.cls
      | `NSArray
      ]

    type 'params t = (cls, 'params) Obj.t

    include S with type cls := cls and type 'params t := 'params t
  end
end

module Example (M : S) = struct
  open! M

  let foo = NSObject.alloc () |> NSObject.init
  let bar = NSObject.alloc () |> NSObject.init

  let array : NSObject.t NSArray.t =
    NSArray.initWithObjects
      (NSArray.alloc ())
      (Ctypes.CArray.of_list Obj.ctype [ foo; bar ])
      2
  ;;

  let dictionary : (NSObject.t * NSObject.t NSArray.t) NSDictionary.t =
    NSDictionary.initWithObjects
      (NSArray.alloc ())
      (Ctypes.CArray.of_list Obj.ctype [ array; array ])
      (Ctypes.CArray.of_list Obj.ctype [ foo; bar ])
      2
  ;;

  let value_from_dictionary : NSObject.t NSArray.t option =
    NSDictionary.objectForKey dictionary foo
  ;;
end
