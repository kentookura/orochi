open Orochi

let smoke () =
  let open Green in
  let ws = Token.create ~kind: whitespace " " in
  let one = Token.create ~kind: int "1" in
  let two = Token.create ~kind: int "2" in
  let three = Token.create ~kind: int "3" in
  let plus = Token.create ~kind: plus "+" in
  let star = Token.create ~kind: star "*" in
  let multiplication =
    let open Node_or_token in
    Node.create
      ~kind: bin_expr
      [
        Token one;
        Token ws;
        Token star;
        Token ws;
        Token two;
      ]
  in
  let addition =
    let open Node_or_token in
    Node.create
      ~kind: bin_expr
      [
        Node multiplication;
        Token ws;
        Token plus;
        Token ws;
        Node multiplication;
      ]
  in
  let addition = Red.Node.create addition in
  let mul2 = List.nth (addition |> Red.Node.children) 4 |> Node_or_token.into_node |> Option.get in
  let one2 = mul2 |> Red.Node.children |> List.hd |> Node_or_token.into_token |> Option.get in
  let new_root = Red.Node.replace_child mul2 0 (Token three) in
  Format.printf "%s\n\n" (Red.Node.show addition);
  Format.printf "%s\n\n" (Red.Node.text addition);
  Format.printf "%s\n\n" (Red.Token.text one2);
  Format.printf "%s\n\n" (Red.Node.text mul2);
  Format.printf "%s\n\n" (Red.Node.text new_root)

let _ =
  smoke ()
