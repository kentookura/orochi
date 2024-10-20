exception Todo
module Node_or_token = Green.Node_or_token
module rec Node: sig
    type t = {
      green: Green.Node.t;
      parent: t option;
      index_in_parent: int;
      text_offset: int
    }
    [@@deriving show]
    val children : t -> Element.t list
    val create : Green.Node.t -> t
    val kind : t -> Green.syntax_kind
    val text : t -> string
    val parent : t -> t option
    val replace_child : t -> int -> (Green.Node.t, Green.Token.t) Node_or_token.t -> t
  end
= struct
  type t = {
    green: Green.Node.t;
    parent: t option;
    index_in_parent: int;
    text_offset: int
  }
  [@@deriving show]

  let create root =
    {
      green = root;
      parent = None;
      index_in_parent = 0;
      text_offset = 0
    }
  let text_offset t = t.text_offset
  let green t = t.green
  let kind t = t.green.kind
  let text_len t = Green.Node.text_len t.green
  let text t = Green.Node.text t.green

  let parent t = t.parent
  (* This is probably too clever.*)
  let ancestors t = Seq.unfold (fun t -> Option.map (fun t -> (t, t)) t.parent) t

  let children t =
    let offset_in_parent = ref 0 in
    green t
    |> Green.Node.children
    |> List.mapi
      (
        fun index_in_parent green_child ->
          (* offset_in_parent := !offset_in_parent + text_offset t; *)
          let text_offset = text_offset t + !offset_in_parent in
          match green_child with
          | Green.Node_or_token.Node node ->
            offset_in_parent := !offset_in_parent + Green.Node.text_len node;
            Green.Node_or_token.Node ({ parent = Some t; index_in_parent; text_offset; green = node }: Node.t)
          | Green.Node_or_token.Token token ->
            offset_in_parent := !offset_in_parent + Green.Token.text_len token;
            Green.Node_or_token.Token ({ parent = Some t; text_offset; green = token }: Token.t)
      )

  let nth_child t = raise Todo
  let child_containing_range t = raise Todo
  let child_containing_range t range = raise Todo

  let rec replace_child t idx new_child : Node.t =
    let children = children t in
    assert (not (idx > List.length @@ children));
    let new_green = t |> green |> (fun n -> Green.Node.replace_child n idx new_child) in
    replace_self t new_green

  and replace_self t new_green : Node.t =
    match t.parent with
    | None -> create new_green
    | Some parent -> replace_child parent t.index_in_parent (Node new_green)
end
and Token: sig
    type t = {
      green: Green.Token.t;
      parent: Node.t option;
      text_offset: int
    }
    val text : t -> string

    val parent : t -> Node.t option
  end
= struct
  type t = {
    green: Green.Token.t;
    parent: Node.t option;
    text_offset: int
  }
  [@@deriving show]
  let create ~parent text_offset green = { parent; text_offset; green; }

  let text_offset t = t.text_offset
  let green t = t.green
  let kind t = t.green.kind
  let text_len t = Green.Token.text_len t.green
  let text t = Green.Token.text t.green
  let parent t = t.parent
end
and Element: sig
    type t = (Node.t, Token.t) Node_or_token.t
  end
= struct
  type t = (Node.t, Token.t) Node_or_token.t
end
