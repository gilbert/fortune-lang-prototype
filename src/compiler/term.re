open T;

let rec getType = (ctx : Context.t, term) => switch(term) {
| Literal(StrLit(_)) => (ctx, Str)
| Literal(NumLit(_)) => (ctx, Num)
| Pop => Context.popType(ctx)
| BlockTerm(terms) => (ctx, Block(terms))
| Inv(Fn(_, FnDef(_, ty)), args) =>
    let (ctx3, argTypes) = args
    |> List.fold_left( ((ctx,tys), term) => {
        let (ctx2, ty) = getType(ctx, term);
        (ctx2, [ty, ...tys])
      }, (ctx,[]));

    Apply.apply(ctx3, ty, argTypes |> List.rev)
| Seq(terms) =>
    let ctx2 = consume(ctx, terms);
    (ctx2, ctx2 |> Context.topType)
}

and consume = (context : Context.t, terms) => {
  terms
  |> List.fold_left( (ctx, term) => {
      let (ctx2, ty) = getType(ctx, term);
      ty |> Context.pushType(ctx2)
    }, context)
};

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
