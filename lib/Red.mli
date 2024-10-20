exception Todo
module Node_or_token = Green.Node_or_token
module rec Node:
sig
  type t = {
    green: Green.Node.t;
    parent: t option;
    index_in_parent: int;
    text_offset: int;
  }
  val pp :
    Format.formatter -> t -> unit
  val show : t -> string
  val children : t -> Element.t list
  val create : Green.Node.t -> t
  val kind : t -> Green.syntax_kind
  val text : t -> string
  val parent : t -> t option
  val replace_child :
    t ->
    int ->
    (Green.Node.t, Green.Token.t) Node_or_token.t ->
    t
end
and Token:
sig
  type t = {
    green: Green.Token.t;
    parent: Node.t option;
    text_offset: int;
  }
  val text : t -> string
  val parent : t -> Node.t option
end
and Element: sig type t = (Node.t, Token.t) Node_or_token.t end
