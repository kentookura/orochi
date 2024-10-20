open Orochi

module Smoke = struct
  let run () =
    let open Green in
    let xml_elt_ident = Syntax_kind 1 in
    let whitespace = Syntax_kind 2 in
    let verbatim = Syntax_kind 3 in
    let text = Syntax_kind 4 in
    let subtree = Syntax_kind 5 in
    let scope = Syntax_kind 6 in
    let rsquare = Syntax_kind 7 in
    let rparen = Syntax_kind 8 in
    let rbrace = Syntax_kind 9 in
    let put = Syntax_kind 10 in
    let patch = Syntax_kind 11 in
    let open_ = Syntax_kind 12 in
    let object_ = Syntax_kind 13 in
    let namespace = Syntax_kind 14 in
    let lsquare = Syntax_kind 15 in
    let lparen = Syntax_kind 16 in
    let let_ = Syntax_kind 17 in
    let lbrace = Syntax_kind 18 in
    let import = Syntax_kind 19 in
    let ident = Syntax_kind 20 in
    let hash_lbrace = Syntax_kind 21 in
    let hash_hash_lbrace = Syntax_kind 22 in
    let get = Syntax_kind 23 in
    let fun_ = Syntax_kind 24 in
    let export = Syntax_kind 25 in
    let eof = Syntax_kind 26 in
    let default = Syntax_kind 27 in
    let def = Syntax_kind 28 in
    let decl_xmlns = Syntax_kind 29 in
    let call = Syntax_kind 30 in
    let alloc = Syntax_kind 31 in
    let p = Token.create ~kind: ident "\\p" in
    let lparen = Token.create ~kind: lparen "{" in
    let rparen = Token.create ~kind: rparen "}" in
    let content = Token.create ~kind: text "asdf" in
    let paragraph =
      Node.create
        ~kind: (Syntax_kind 100)
        [
          Token p;
          Token lparen;
          Token content;
          Token rparen;
        ]
    in
    let paragraph = Red.Node.create paragraph in
    let replaced = Red.Node.replace_child paragraph 2 (Token (Token.create ~kind: text "replaced!")) in
    Format.printf "%s\n\n" (Red.Node.show paragraph);
    Format.printf "%s\n\n" (Red.Node.text paragraph);
    Format.printf "%s\n\n" (Red.Node.text replaced)
end

let () =
  Smoke.run ()
