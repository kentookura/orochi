(* TODO: I am using a lot of Option.get in this module. Either verify soundness
   by testing or actually match on option.
*)

let (let@) = ( @@ )
let ( @~ ) = Fun.flip
exception Todo

type node_data = {
  green: Green.Element.t;
  parent: node_data option;
  index_in_parent: int;
  offset: int
}
[@@deriving show]

module rec Node_or_token: sig
    type ('a, 'b) t = Node of 'a | Token of 'b [@@deriving show]
    val text : (Node.t, Token.t) t -> string
    val into_node : ('a, 'b) t -> 'a option
    val into_token : ('a, 'b) t -> 'b option
  end
= struct
  type ('a, 'b) t = Node of 'a | Token of 'b [@@deriving show]
  let text_len t =
    match t with
    | Node a -> Node.text_offset a
    | Token b -> Token.text_offset b

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
and Node: sig
    type t = node_data
    [@@deriving show]
    type children = { next: t option } [@@deriving show]
    val children : t -> children
    val text_offset : t -> int
    val create : Green.Element.t -> t
    val kind : t -> Green.syntax_kind
    val text : t -> string
    val parent : t -> t option
    val ancestors : t -> Node.t Seq.t
    val remove_child : t -> int -> t
    val replace_with : t -> Green.Node.t -> Green.Node.t
    val replace_child : t -> int -> Green.Child.t -> t
    (* val next_sibling : t -> t option *)
    (* val prev_sibling : t -> t option *)
  end
= struct
  type t = node_data [@@deriving show]
  type children = { next: t option } [@@deriving show]

  let create (root : Green.Element.t) =
    {
      green = root;
      parent = None;
      index_in_parent = 0;
      offset = 0
    }

  let new_child
      : Green.Node.t ->
      Node.t ->
      int ->
      int ->
      Node.t
    = fun
        green
        parent
        index_in_parent
        offset
      ->
      { green = Green.Element.Node green; parent = Some parent; index_in_parent; offset; }

  let text_offset t = t.offset
  let green t = t.green
  let kind t = Green.Element.kind t.green
  let text_len t = Green.Element.text_len t.green
  let text t = Green.Element.text t.green

  let parent t = t.parent
  (* This is probably too clever.*)
  let ancestors t = Seq.unfold (fun t -> Option.map (fun t -> (t, t)) t.parent) t

  let first_child : t -> t option = fun t ->
      match t with
      | { green; parent; index_in_parent; offset } ->
        try
          green
          |> Green.Element.into_node
          |> Option.get
          |> Green.Node.children
          |> List.rev
          |> List.find_mapi
            (
              fun index child ->
                child
                |> Green.Child.into_node
                |> Option.get
                |> fun green ->
                  new_child green t index (offset + Green.Child.rel_offset child)
                  |> Option.some
            )
        with
          | _ -> None

  let new_children : t -> children = fun t -> { next = first_child t }

  let children t =
    (* let offset_in_parent = ref 0 in *)
    { next = t |> first_child }

  (* |> Green.Node.children *)
  (* |> List.mapi *)
  (*   ( *)
  (*     fun index_in_parent green_child -> *)
  (*       let text_offset = text_offset t + !offset_in_parent in *)
  (*       match green_child with *)
  (*       | Green.Child.Node{ node; _ } -> *)
  (*         offset_in_parent := !offset_in_parent + Green.Child.text_len green_child; *)
  (*         Node_or_token.Node *)
  (*           { parent = Some t; index_in_parent; text_offset; green = node } *)
  (*       | Green.Child.Token{ token; _ } -> *)
  (*         offset_in_parent := !offset_in_parent + Green.Child.text_len green_child; *)
  (*         Node_or_token.Token (Token.create ~parent: (Some t) text_offset token) *)
  (*   ) *)

  let green_siblings (t : t) =
    let@ parent = Option.map @~ t.parent in
    parent.green
    |> Green.Element.into_node
    |> Option.get
    |> Green.Node.children

  let next_sibling (t : t) =
    let@ siblings = Option.map @~ green_siblings t in
    List.find_mapi
      (
        fun index child ->
          Green.Child.into_node child
          |> fun green ->
            (* WARNING: Using Option.get!! *)
            let parent = parent t |> Option.get in
            let offset = text_offset parent + Green.Child.rel_offset child in
            Some (new_child (green |> Option.get) parent index offset)
      )
      siblings

  let nth_child t = raise Todo
  let child_containing_range t = raise Todo
  let child_containing_range t range = raise Todo

  let remove_child t idx = raise Todo

  let rec replace_with
      : t ->
      Green.Node.t ->
      Green.Node.t
    = fun t replacement ->
      assert (kind t = Green.Node.kind replacement);
      let parent = parent t |> Option.get in
      let me = t.index_in_parent in
      let new_parent =
        Green.Node.replace_child
          (
            parent.green
            |> Green.Element.into_node
            |> Option.get
          )
          me
          (Green.Element.Node replacement)
      in
      replace_with parent new_parent

  let replace_child = raise Todo

  (* let rec replace_child t idx new_child : Node.t = *)
  (*   let children = children t in *)
  (*   assert (not (idx > List.length @@ children)); *)
  (*   let new_green = t |> green |> (fun n -> Green.Node.replace_child n idx new_child) in *)
  (*   replace_self t new_green *)

  (* and replace_self t new_green : Node.t = *)
  (*   match t.parent with *)
  (*   | None -> create new_green *)
  (*   | Some parent -> replace_child parent t.index_in_parent new_green *)
end
and Token: sig
    type t
    val text : t -> string

    val create : parent: Node.t option -> int -> int -> Green.Token.t -> t
    val text_len : t -> int
    val parent : t -> Node.t option
    val text_offset : t -> int
    val replace_with : t -> Green.Token.t -> Green.Node.t
  end
= struct
  type t = {
    green: Green.Token.t;
    parent: Node.t option;
    index: int;
    text_offset: int
  }
  [@@deriving show]
  let create ~parent index text_offset green = { parent; text_offset; green; index; }

  let text_offset t = t.text_offset
  let green t = t.green
  let kind t = Green.Token.(kind t.green)
  let text_len t = Green.Token.text_len t.green
  let text t = Green.Token.text t.green
  let parent t = t.parent

  let replace_with : t -> Green.Token.t -> Green.Node.t = fun t replacement ->
      assert (Green.Token.kind t.green = Green.Token.kind replacement);
      let parent = t.parent |> Option.get in
      let me = t.index in
      let new_parent =
        Green.Node.replace_child
          (
            parent.green
            |> Green.Element.into_node
            |> Option.get
          )
          me
          (replacement |> Green.Element.of_token)
      in
      Node.replace_with parent new_parent
end
