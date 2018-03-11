
let map = (fname, tcname) =>
  T.DepType(fname, (ctx, args) => switch (args) {
    | [T.TypeCon(name, [|itemType|]), Block(terms)] when name == tcname =>
      let ctx2 = ctx
        |> Context.pushNewSingle
        |> Context.pushType(_, itemType)
        |> Term.consume(_, terms);

      (ctx, T.TypeCon(name, [|ctx2 |> Context.topType|] ))
    | _ => raise(T.TypeError("Argument Mismatch"))
  });
