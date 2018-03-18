open T;

let rec print = (term) => switch(term) {
  | Literal(StrLit(s)) => "Lit(\"" ++ s ++ "\")"
  | Literal(NumLit(n)) => "Lit(" ++ string_of_int(n) ++ ")"
  | BlockTerm(terms) => "[" ++ print_terms(terms, " ") ++ "]"
  | Pop => "_"
  | Inv(Fn(Module(mod_,_), FnDef(fun_, _)), args) =>
      mod_ ++ "." ++ fun_ ++ "(" ++ print_terms(args, ", ") ++ ")"
  | Seq(terms) => print_terms(terms, " ")
}
and print_terms = (terms, sep) => switch (List.length(terms)) {
  | 0 => ""
  | _ =>
    terms
    |> List.tl
    |> List.fold_left((r, term) => r ++ sep ++ print(term), print(terms |> List.hd))
};
