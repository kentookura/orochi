exception Todo
type syntax_kind = Syntax_kind of int [@@unboxed ]
val pp_syntax_kind :
  Format.formatter ->
  syntax_kind ->
  unit
val show_syntax_kind : syntax_kind -> string
module rec Token:
sig
  type t
  val pp :
    Format.formatter -> t -> unit
  val show : t -> string
  val create : kind: syntax_kind -> string -> t
  val kind : t -> syntax_kind
  val text : t -> string
  val text_len : t -> int
end
and Node:
sig
  type t
  val pp :
    Format.formatter -> t -> unit
  val show : t -> string
  val create : kind: syntax_kind -> Child.t list -> t
  val kind : t -> syntax_kind
  val text_len : t -> int
  val children : t -> Child.t list
  val replace_child : t -> int -> Element.t -> t
  val text : t -> string
end
and Child:
sig
  type node_child = { rel_offset: int; node: Node.t } [@@deriving show]
  type token_child = { rel_offset: int; token: Token.t } [@@deriving show]
  type t =
    | Node of node_child
    | Token of token_child
  val pp :
    Format.formatter ->
    t ->
    unit
  val show :
    t -> string
  val text : t -> string
  val rel_offset : t -> int
  val text_len : t -> int
  val into_node : t -> Node.t option
  val into_token : t -> Token.t option
end
and Element: sig
  type t =
    | Node of Node.t
    | Token of Token.t
  [@@deriving show]
  val into_node : t -> Node.t option
  val kind : t -> syntax_kind
  val text_len : t -> int
  val text : t -> string
  val of_node : Node.t -> t
  val of_token : Token.t -> t
end
