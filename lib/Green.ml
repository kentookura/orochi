exception Todo
type syntax_kind = Syntax_kind of int [@@unboxed ] [@@deriving show]

module rec Token:
  sig
    type t = { kind: syntax_kind; text: string; } [@@deriving show]
    val create : kind: syntax_kind -> string -> t
    val kind : t -> syntax_kind
    val text : t -> string
    val text_len : t -> int
  end
= struct
  type t = { kind: syntax_kind; text: string } [@@deriving show]
  let create ~kind text = { kind; text }
  let kind t = t.kind
  let text t = t.text
  let text_len t = String.length t.text
end
and Node:
  sig
    type t = {
      kind: syntax_kind;
      children: ((t, Token.t) Node_or_token.t) list;
      len: int;
    }
    [@@deriving show]
    val create : kind: syntax_kind -> (t, Token.t) Node_or_token.t list -> t
    val kind : t -> syntax_kind
    val text_len : t -> int
    val children : t -> (t, Token.t) Node_or_token.t list
    val replace_child : t -> int -> (t, Token.t) Node_or_token.t -> t
    val text : t -> string
  end
= struct
  type t = {
    kind: syntax_kind;
    children: (t, Token.t) Node_or_token.t list;
    len: int
  }
  [@@deriving show]
  let text_len t = t.len
  let kind t = t.kind

  let create : kind: syntax_kind -> (t, Token.t) Node_or_token.t list -> t = fun ~kind children ->
      let len =
        List.fold_left
          (
            fun acc c ->
              match c with
              | Node_or_token.Node a -> text_len a + acc
              | Node_or_token.Token b -> Token.text_len b + acc
          )
          0
          children
      in
      { kind; children; len }

  let children t = t.children

  let replace_child t idx new_child : t =
    let children = children t in
    assert (not (idx > List.length @@ children));
    let new_children =
      children
      |> List.mapi
        (fun i c -> if i = idx then new_child else c)
    in
    { t with children = new_children }

  let text t = List.map Node_or_token.text (children t) |> String.concat ""
end
and Node_or_token: sig
    type ('a, 'b) t = Node of 'a | Token of 'b [@@deriving show]
    val text : (Node.t, Token.t) t -> string
    val into_node : ('a, 'b) t -> 'a option
    val into_token : ('a, 'b) t -> 'b option
  end
= struct
  type ('a, 'b) t = Node of 'a | Token of 'b [@@deriving show]
  let text_len t =
    match t with
    | Node a -> Node.text_len a
    | Token b -> Token.text_len b

  let text t =
    match t with
    | Node a -> Node.text a
    | Token b -> Token.text b

  let into_node t =
    match t with
    | Node a -> Some a
    | Token _ -> None

  let into_token t =
    match t with
    | Node _ -> None
    | Token b -> Some b
end

(* TODO : Don't do this*)
let push x xs = xs @ [x]

type element = (Node.t, Token.t) Node_or_token.t
