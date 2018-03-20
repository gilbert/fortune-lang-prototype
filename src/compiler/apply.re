open T;

let rec zip_ = (xs, ys) => switch (xs, ys) {
  | ([x,...xs], [y,...ys]) => [(x,y), ...zip_(xs,ys)]
  | ([], []) => []
  | _ => raise(T.TypeError("Different list lengths (this shouldn't happen)"))
};

let zip = (xs, ys) =>  if ( List.length(xs) == List.length(ys) ) {
  zip_(xs, ys)
} else {
  raise(T.TypeError("[apply] Incorrect number of elements"))
};

let rec sub = (subs, ty) => switch (ty) {
  | Var(x, xn) =>
    try(List.assoc(x, subs)) {
      | Not_found => raise(TypeError("Type variable not found: " ++ xn))
    }
  | TypeCon(n, tys) => TypeCon(n, tys |> Array.map(sub(subs)))
  | Block(a,b) => Block(a |> sub(subs), b |> sub(subs))
  | other => other
};

let rec getType = (ctx : Context.t, term) => switch(term) {
| Literal(StrLit(_)) => (ctx, Str)
| Literal(NumLit(_)) => (ctx, Num)
| Pop => Context.popType(ctx)
| BlockTerm(terms) => (ctx, UBlock(terms))
| Inv(Fn(_, FnDef(_, ty)), args) =>
    let (ctx3, argTypes) = args
    |> List.fold_left( ((ctx,tys), term) => {
        let (ctx2, ty) = getType(ctx, term);
        (ctx2, [ty, ...tys])
      }, (ctx,[]));

    let (_subs, ctx4, ty2) = apply([], ctx3, ty, argTypes |> List.rev);
    (ctx4, ty2)
| BranchInv(_, _) => (ctx, Unit)
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
}

and apply = (subs, ctx, ftype, args) => switch (ftype) {
| T.BasicFn(params, ret) => {
  let (subs2, ctx2) = unify_all([], ctx, zip(params,args));
  (subs2, ctx2, ret |> sub(subs2))
}

| T.DepType(_name, f) => {
    let (ctx2, ty) = f((ctx, args));
    (subs, ctx2, ty)
  }
| _ => raise(T.TypeError("[apply] Type is not a function: " ++ T.print(ftype)))
}

and unify_all = (subs, ctx, tpairs) => switch (tpairs) {
  | [(a1,a2), ...rest] => {
    let (subs2, ctx2) = unify(subs, ctx, a1, a2);
    unify_all(subs2, ctx2, rest)
  }
  | [] => (subs, ctx)
}

and unify = (subs, ctx, a, b) => {
  Js.log("Unifying " ++ print(a) ++ " and " ++ print(b));
switch (a,b) {
  | (Var(x,xn), Var(y,yn)) => if (x == y) {
    (subs, ctx)
  } else {
    raise(TypeError("Incompatible type variables: " ++ xn ++ " != " ++ yn))
  }
  | (Var(x,_), _) => ([(x,b),...subs], ctx)
  | (_,Var(_,_)) => unify(subs, ctx, b, a)
  | (BranchBlock(ty, AnyBranch), UBlock(terms)) =>
    /* Ensure block ends up branching */
    let inputTy = switch (ty) {
      | Var(id,name) => try(List.assoc(id, subs)) {
        | Not_found => raise(TypeError("Unresolved branch block input type: " ++ name))
      }
      | _ => ty
    };
    let ctx2 = ctx
      |> Context.pushNewSingle
      |> switch(inputTy){ | Unit => (x=>x) | _ => Context.pushType(_, inputTy) }
      |> consume(_, terms);

    (subs, ctx2)
  | (BranchBlock(_ty, _branch), UBlock(_terms)) =>
    /* TODO: Ensure matching branch */
    (subs, ctx)
  | (Block(Var(id1,name1),Var(id2,_name2)), UBlock(terms)) =>
    /* id1 needs to be a concrete type so we can resolve the block */
    let inputTy = try(List.assoc(id1, subs)) {
      | Not_found => raise(TypeError("[internal] Unresolved block input type: " ++ name1))
    };
    /* id2 might have already been resolved */
    let (subs3, ctx3) = try(
      List.assoc(id2, subs) |> ((_) => (subs, ctx))
    ) {
    | Not_found =>
      /* It hasn't, so calculate it */
      let ctx2 = ctx
        |> Context.pushNewSingle
        |> Context.pushType(_, inputTy)
        |> consume(_, terms);
      let ty = ctx2 |> Context.topType;
      ([(id2,ty),...subs], ctx2)
    };
    (subs3, ctx3)

  | (TypeCon(con1,args1), TypeCon(con2,args2)) =>
    if (con1 == con2 && Array.length(args1) == Array.length(args2)) {
      zip_(Array.to_list(args1),Array.to_list(args2)) |> unify_all(subs, ctx)
    } else {
      raise(TypeError("Constructors do not match"))
    }
  | (Str,Str) | (Num,Num) | (Bool,Bool) => (subs, ctx)
  | _ => raise(TypeError("Incompatible types: " ++ print(a) ++ " != " ++ print(b)))
}};
