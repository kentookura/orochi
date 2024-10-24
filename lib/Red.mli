exception Todo
module rec Node_or_token: sig
  type ('a, 'b) t
  val text : (Node.t, Token.t) t -> string
end
and Node:
sig
  type t
  type children
  val pp :
    Format.formatter -> t -> unit
  val show : t -> string
  val children : t -> children
  val create : Green.Element.t -> t
  val kind : t -> Green.syntax_kind
  val text : t -> string
  val parent : t -> t option
  val remove_child : t -> int -> t
  val replace_child :
    t ->
    int ->
    Green.Child.t ->
    t
end
and Token:
sig
  type t
  val text : t -> string
  val parent : t -> Node.t option
end
