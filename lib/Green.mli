exception Todo
type syntax_kind = Syntax_kind of int [@@unboxed]
val pp_syntax_kind :
  Format.formatter ->
  syntax_kind -> unit
val show_syntax_kind : syntax_kind -> string
module rec Token :
  sig
    type t = { kind : syntax_kind; text : string; }
    val pp :
      Format.formatter -> t -> unit
    val show : t -> string
    val create : kind:syntax_kind -> string -> t
    val kind : t -> syntax_kind
    val text : t -> string
    val text_len : t -> int
  end
and Node :
  sig
    type t = {
      kind : syntax_kind;
      children : (t, Token.t) Node_or_token.t list;
      len : int;
    }
    val pp :
      Format.formatter -> t -> unit
    val show : t -> string
    val create : kind:syntax_kind -> (t, Token.t) Node_or_token.t list -> t
    val kind : t -> syntax_kind
    val text_len : t -> int
    val children : t -> (t, Token.t) Node_or_token.t list
    val replace_child : t -> int -> (t, Token.t) Node_or_token.t -> t
    val text : t -> string
  end
and Node_or_token :
  sig
    type ('a, 'b) t = Node of 'a | Token of 'b
    val pp :
      (Format.formatter ->
       'a -> unit) ->
      (Format.formatter ->
       'b -> unit) ->
      Format.formatter ->
      ('a, 'b) t -> unit
    val show :
      (Format.formatter ->
       'a -> unit) ->
      (Format.formatter ->
       'b -> unit) ->
      ('a, 'b) t -> string
    val text : (Node.t, Token.t) t -> string
    val into_node : ('a, 'b) t -> 'a option
    val into_token : ('a, 'b) t -> 'b option
  end
val push : 'a -> 'a list -> 'a list
type element = (Node.t, Token.t) Node_or_token.t

