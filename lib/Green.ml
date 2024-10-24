exception Todo

let split_n n t_orig =
  if n <= 0 then [], t_orig
  else
    (
      let rec loop n t accum =
        match t with
        | [] -> t_orig, [] (* in this case, t_orig = rev accum *)
        | hd :: tl -> if n = 0 then List.rev accum, t else loop (n - 1) tl (hd :: accum)
      in
      loop n t_orig []
    )

type syntax_kind = Syntax_kind of int [@@unboxed ] [@@deriving show]

module rec Token:
  sig
    type t [@@deriving show]
    val create : kind: syntax_kind -> string -> t
    val kind : t -> syntax_kind
    val text : t -> string
    val text_len : t -> int
    (* val index : t -> int *)
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
    type t [@@deriving show]
    val create : kind: syntax_kind -> Child.t list -> t
    val kind : t -> syntax_kind
    val text_len : t -> int
    val children : t -> Child.t list
    val replace_child : t -> int -> Element.t -> t
    val remove_child : t -> int -> t
    val insert_child : t -> int -> Child.t -> t
    val text : t -> string
  end
= struct
  type t = {
    kind: syntax_kind;
    children: Child.t list;
    len: int
  }
  [@@deriving show]
  let text_len t = t.len
  let kind t = t.kind

  let create : kind: syntax_kind -> Child.t list -> t = fun ~kind children ->
      let len =
        List.fold_left
          (
            fun acc c ->
              match c with
              | Child.Node a -> text_len a.node + acc
              | Child.Token b -> Token.text_len b.token + acc
          )
          0
          children
      in
      { kind; children; len }

  let children t = t.children

  let replace_child t idx new_child : t =
    let children = children t in
    let text_len = ref 0 in
    assert (not (idx > List.length @@ children));
    let new_children =
      children
      |> List.mapi
        (
          fun i c ->
            let rel_offset = !text_len in
            text_len := rel_offset + (Child.text_len c);
            (* if i = idx then {new_child} else c *)
            match c with
            | Child.Node node -> Child.Node ({ node with rel_offset; })
            | Child.Token token -> Child.Token ({ token with rel_offset; })
        )
    in
    { t with children = new_children }

  let insert_child t idx child : t =
    let children = children t in
    assert (not (idx > List.length @@ children));
    let new_children =
      children
      |> split_n (idx - 1)
      |> fun (alpha, omega) -> alpha @ child :: omega
    in
    { t with children = new_children }

  let remove_child t idx : t =
    let children = children t in
    assert (not (idx > List.length @@ children));
    let new_children =
      children |> split_n (idx - 1)
      |> function
      | (alpha, o :: os) -> alpha @ os
      | (alpha, []) -> alpha
    in
    { t with children = new_children }

  let text t = List.map Child.text (children t) |> String.concat ""
end
and Child: sig
    type node_child = { rel_offset: int; node: Node.t } [@@deriving show]
    type token_child = { rel_offset: int; token: Token.t } [@@deriving show]
    type t =
      | Node of node_child
      | Token of token_child
    [@@deriving show]
    val text : t -> string
    val text_len : t -> int
    val rel_offset : t -> int
    val into_node : t -> Node.t option
    val into_token : t -> Token.t option
  end
= struct
  type node_child = { rel_offset: int; node: Node.t } [@@deriving show]
  type token_child = { rel_offset: int; token: Token.t } [@@deriving show]
  type t =
    | Node of node_child
    | Token of token_child
  [@@deriving show]

  let text_len t =
    match t with
    | Node a -> Node.text_len a.node
    | Token b -> Token.text_len b.token

  let text t =
    match t with
    | Node a -> Node.text a.node
    | Token b -> Token.text b.token

  let rel_offset t =
    match t with
    | Node a -> a.rel_offset
    | Token b -> b.rel_offset

  let into_node t =
    match t with
    | Node a -> Some a.node
    | Token _ -> None

  let into_token t =
    match t with
    | Node _ -> None
    | Token b -> Some b.token
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
= struct
  type t =
    | Node of Node.t
    | Token of Token.t
  [@@deriving show]

  let into_node (t : t) =
    match t with
    | Node node -> Some node
    | Token _ -> None

  let kind (t : t) =
    match t with
    | Node node -> Node.kind node
    | Token token -> Token.kind token

  let text_len (t : t) =
    match t with
    | Node node -> Node.text_len node
    | Token token -> Token.text_len token

  let text (t : t) =
    match t with
    | Node node -> Node.text node
    | Token token -> Token.text token

  let of_node (node : Node.t) = Node node
  let of_token (token : Token.t) = Token token
end
