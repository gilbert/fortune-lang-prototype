open T;

let log = (label, x) => {Js.log(label);Js.log(x);x};
type s = {
  types: list((int,ty)),
  constants: list((int,const_num))
};

let empty_subs = { types: [], constants: [] };
let push_type_sub = (id, ty, subs : s) => {
  Js.log("Sub type ." ++ string_of_int(id) ++ " = " ++ print(ty));
  {
    ...subs,
    types: [(id,ty), ...subs.types]
  }
};
let push_range_sub = (id, num_lit, subs : s) => {
  Js.log("Sub const_num ." ++ string_of_int(id) ++ " = " ++ print_const_num(num_lit));
  {
    ...subs,
    constants: [(id,num_lit), ...subs.constants]
  }
};

module Range = {

  let rec unify_const_num = (subs, c1, c2) => switch (c1,c2) {
    | (ConstNumVar(id1,_), ConstNumVar(id2,_)) =>
      if (id1 != id2) {
        raise(TypeError("Const num mismatch: " ++ print_const_num(c1) ++ " and " ++ print_const_num(c2)))
      }
      else {
        (subs, c1)
      }
    | (ConstNumVar(id,name), _) =>
      switch (Belt.List.getAssoc(subs.constants, id, (==))) {
        | Some(c3) =>
          Js.log("Found const var value " ++ name ++ "." ++ string_of_int(id) ++ " = " ++ print_const_num(c3));
          unify_const_num(subs, c2, c3)
        | None =>
          Js.log("Subbing num const " ++ name ++ "." ++ string_of_int(id) ++ " = " ++ print_const_num(c2));
          (subs |> push_range_sub(id, c2), c2)
      }
    | (ConstNum(n1), (ConstNum(n2))) =>
      if (n1 == n2) {
        (subs, ConstNum(n2))
      }
      else {
        raise(TypeError("Const num does not unify: " ++ string_of_int(n1) ++ " != " ++ string_of_int(n2)))
      }
  };

  let unify_val_common = (subs, v1, v2) => switch (v1,v2) {
    | (RangeVal(c1), RangeVal(c2)) =>
      Js.log("Unifyig const nums " ++ print_const_num(c1) ++ " =? " ++ print_const_num(c2));
      let (subs3, c3) = unify_const_num(subs, c1, c2);
      (subs3, RangeVal(c3))

    | (RangeAdd(ConstNumVar(id,_),ConstNum(n)), RangeVal(ConstNum(m)))
    | (RangeAdd(ConstNum(n),ConstNumVar(id,_)), RangeVal(ConstNum(m))) =>
      (subs |> push_range_sub(id, ConstNum(m - n)), v2)

    | (RangeSub(ConstNumVar(id,_),ConstNum(n)), RangeVal(ConstNum(m))) =>
      (subs |> push_range_sub(id, ConstNum(m + n)), v2)
    | (RangeSub(ConstNum(n),ConstNumVar(id,_)), RangeVal(ConstNum(m))) =>
      (subs |> push_range_sub(id, ConstNum(m - n)), v2)
    | _ => raise(TypeError("Invalid range comparison: " ++ print_range_val(v1) ++ " and " ++ print_range_val(v2)))
  };

  let merge_min_val = (subs, v1,v2) => switch (v1,v2) {
    | (RangeValMax, v) | (v, RangeValMax) => (subs, v)
    | (RangeVal(ConstNum(x)), RangeVal(ConstNum(y))) => (subs, RangeVal(ConstNum(min(x,y))))
    | _ => unify_val_common(subs, v1, v2)
  };

  let merge_max_val = (subs, v1,v2) => switch (v1,v2) {
    | (RangeValMax, _) | (_, RangeValMax) => (subs, RangeValMax)
    | (RangeVal(ConstNum(x)), RangeVal(ConstNum(y))) => (subs, RangeVal(ConstNum(max(x,y))))
    | _ => unify_val_common(subs, v1, v2)
  };

  let merge_ranges = (subs, ctx, Range(x1,y1) as r1, Range(x2,y2) as r2) => {
    Js.log("! Merging range " ++ print_range(r1) ++ " and " ++ print_range(r2));
    Js.log("  (subs:" ++
      (subs.constants
          |> List.fold_left((s,(id,n)) => s ++ " " ++ string_of_int(id) ++ "=" ++ print_const_num(n), ""))
      ++ ")");
    let (subs2, minVal) = merge_min_val(subs,  x1, x2);
    let (subs3, maxVal) = merge_max_val(subs2, y1, y2);
    let r3 = Range(minVal, maxVal);

    Js.log("  Merged range to:" ++ print_range(r3));
    (subs3, ctx, r3)
  };

  /* r1 is base; r2 must match */
  let unify_ranges = (subs, ctx, Range(x1,y1) as r1, Range(x2,y2) as r2) => {
    Js.log("Unifying range " ++ print_range(r1) ++ " and " ++ print_range(r2));
    Js.log("  (subs:" ++
      (subs.constants
          |> List.fold_left((s,(id,n)) => s ++ " " ++ string_of_int(id) ++ "=" ++ print_const_num(n), ""))
      ++ ")");

    let (subs2, x3) = switch (x1,x2) {
      | (RangeValMin, RangeValMin) => (subs, x1)
      | (RangeValMin, RangeVal(ConstNum(_))) => (subs, x1)
      | (RangeVal(ConstNum(n1)), RangeVal(ConstNum(n2))) =>
        if (n1 <= n2) {
          (subs, x1)
        }
        else {
          raise(TypeError("Range min out of bounds: " ++ string_of_int(n1) ++ " > " ++ string_of_int(n2)))
        }
      | _ => unify_val_common(subs, x1, x2)
    };

    let (subs3, y3) = switch (y1,y2) {
      | (RangeValMax, RangeValMax) => (subs, y1)
      | (RangeValMax, RangeVal(ConstNum(_))) => (subs, y1)
      | (RangeVal(ConstNum(n1)), RangeVal(ConstNum(n2))) =>
        if (n2 <= n1) {
          (subs, y1)
        }
        else {
          raise(TypeError("Range max out of bounds: " ++ string_of_int(n2) ++ " > " ++ string_of_int(n1)))
        }
      | _ => unify_val_common(subs2, y1, y2)
    };

    let r3 = Range(x3,y3);

    Js.log("Unified range to:" ++ print_range(r3));
    (subs3, ctx, r3)
  };

  let unify = (strict) => strict ? unify_ranges : merge_ranges;
};

let rec zip_ = (xs, ys) => switch (xs, ys) {
  | ([x,...xs], [y,...ys]) => [(x,y), ...zip_(xs,ys)]
  | ([], []) => []
  | _ => raise(T.TypeError("Different list lengths (this shouldn't happen)"))
};

let zip = (xs, ys) =>  if ( List.length(xs) == List.length(ys) ) {
  zip_(xs, ys)
} else {
  raise(T.TypeError(
    "[apply] Incorrect number of elements\n"
    ++ "  Expected: (" ++ print_types(xs, ", ") ++ ")\n"
    ++ "  Actual: ("  ++ print_types(ys, ", ") ++ ")\n"
  ))
};


let sub_num = (subs, c) => switch (c) {
  | ConstNumVar(id, name) as var =>
    switch (Belt.List.getAssoc(subs.constants, id, (==))) {
      | Some(num) =>
        Js.log("Subbing num const " ++ name ++ "." ++ string_of_int(id) ++ " = " ++ print_const_num(num));
        num
      | None =>
        Js.log("[Warning] Not subbing num const " ++ name ++ "." ++ string_of_int(id));
        var
    }
  | other => other
};

let sub_range_val = (subs, rval) => switch(rval) {
  | RangeVal(n) => RangeVal(sub_num(subs,n))
  | RangeAdd(x,y) =>
    let x2 = sub_num(subs,x);
    let y2 = sub_num(subs,y);
    /* Collapse constants when possible */
    switch(x2, y2) {
      | (ConstNum(x), ConstNum(y)) => RangeVal(ConstNum(x + y))
      | _ => RangeAdd(x2,y2)
    }
  | RangeSub(x,y) =>
    let x2 = sub_num(subs,x);
    let y2 = sub_num(subs,y);
    /* Collapse constants when possible */
    switch(x2, y2) {
      | (ConstNum(x), ConstNum(y)) => RangeVal(ConstNum(x - y))
      | _ => RangeSub(x2,y2)
    }
  | other => other
};

let sub_range = (subs, Range(x,y)) => Range(sub_range_val(subs, x), sub_range_val(subs, y));

let rec sub = (subs, ty) => switch (ty) {
  | Var(id, name) =>
    switch (Belt.List.getAssoc(subs.types, id, (==))) {
      | Some(t) =>
        Js.log("Found type var value " ++ name ++ "." ++ string_of_int(id) ++ " = " ++ print(t));
        t
      | None =>
        Js.log("[Warning] Type var not found " ++ name ++ "." ++ string_of_int(id));
        ty
    }
  | TypeCon(n, tys) => TypeCon(n, tys |> Array.map(sub(subs)))
  | Arr(ty, r) => Arr(ty |> sub(subs), r |> sub_range(subs))
  | Str(r) => Str(r |> sub_range(subs))
  | Num(r) => Num(r |> sub_range(subs))
  | Block(a,b) => Block(a |> sub(subs), b |> sub(subs))
  | other => other
};

let rec getTypes = (ctx, terms) =>
  terms
  |> List.fold_left( ((ctx,tys), term) => {
       let (ctx2, ty) = getType(ctx, term);
       (ctx2, [ty, ...tys])
     }, (ctx,[]))
  |> ((ctx3, tys)) => (ctx3, List.rev(tys))

and getType = (ctx : Context.t, term) => switch(term) {
| Literal(StrLit(s)) => (ctx, Str(makeRangeN(s |> String.length)))
| Literal(NumLit(n)) => (ctx, Num(makeRangeN(n)))
| Literal(ArrLit(terms)) =>
  let length = terms |> List.length;
  let n = ConstNum(length);
  if (length == 0) {
    (ctx, Arr(Var(next_id(), "i"), makeRangeV(n,n)))
  }
  else {
    /* Get type of each term in the array literal */
    let (ctx3, termTypes) = getTypes(ctx, terms);

    /* Make sure all types are compatible */
    let (_subs4, ctx4, itemType) = termTypes
    |> List.fold_left( ((subs, ctx, prev), termType) => {
        unify(~strict=false, subs, ctx, prev, termType |> sub(subs))
      }, (empty_subs, ctx3, List.hd(termTypes)));

    (ctx4, Arr(itemType, makeRangeV(n,n)))
  }
| IfElse(cond, then_, else_) =>
  let (ctx2, condTy) = getType(ctx, cond);
  let (subs1, ctx3, _boolType) = unify(empty_subs, ctx2, condTy, Bool);

  /* then_ and else_ can/should only be UBlock */
  let (_, UBlock(thenTerms)) = getType(ctx3, then_);
  let (_, UBlock(elseTerms)) = getType(ctx3, else_);

  let ctx_a = consume(ctx3, thenTerms);
  let ctx_b = consume(ctx3, elseTerms);

  Js.log("ctx_a"); Js.log(ctx_a |> Context.topRtStack |> print_types(_, ";"));
  Js.log("ctx_b"); Js.log(ctx_b |> Context.topRtStack |> print_types(_, ";"));

  let rtStack_a = ctx_a |> Context.topRtStack;
  let rtStack_b = ctx_b |> Context.topRtStack;

  if ( List.length(rtStack_a) != List.length(rtStack_b) ) {
    raise(TypeError("@if then-else push an inconsistent number of items onto the stack"))
  };

  let (_subs2, ctx4, newRtStack) = unify_all(~strict=false, subs1, ctx_a, zip(rtStack_a, rtStack_b));

  let ctx5 = ctx4 |> Context.replaceTopRtStack(_, newRtStack);

  (ctx5, List.length(newRtStack) > 0 ? List.hd(newRtStack) : Void)

| Pop => Context.popType(ctx)
| BlockTerm(terms) => (ctx, UBlock(terms))
| Inv(Fn(_, FnDef(_, ty)), args) =>
    let (ctx3, argTypes) = args
    |> List.fold_left( ((ctx,tys), term) => {
        let (ctx2, ty) = getType(ctx, term);
        (ctx2, [ty, ...tys])
      }, (ctx,[]));

    let (_subs, ctx4, ty2) = apply(empty_subs, ctx3, ty, argTypes |> List.rev);
    (ctx4, ty2)

| BranchInv(AnyBranch, _) =>
  raise(TypeError("getType of AnyBranch is unsupported"))
| BranchInv(BranchPath(_) as br_path, args) =>
  switch(args |> List.length) {
    | 0 => (ctx, Branch(br_path, Void))
    | _ => raise(TypeError("BranchPath has args (currently unsupported)"))
  }
| BranchInv(BranchFn(_, BranchDef(name, fn_ty)) as br_fn, args) =>
  /* Ensure args are compatible with branch function */
  let (ctx2, argTypes) = getTypes(ctx, args);
  let (_subs, ctx3, ret) = apply(empty_subs, ctx, fn_ty, argTypes);
  (ctx2, Branch(br_fn, ret))
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
  Js.log("-> Applying fn: " ++ print(ftype));
  let (subs2, ctx2, _) = unify_all(empty_subs, ctx, zip(params,args));
  (subs2, ctx2, ret |> sub(subs2))
}

| T.DepType(_name, f) => {
    let (ctx2, ty) = f((ctx, args));
    (subs, ctx2, ty)
  }
| _ => raise(T.TypeError("[apply] Type is not a function: " ++ T.print(ftype)))
}

and unify_all = (~acc=[], ~strict=true, subs, ctx, tpairs) => switch (tpairs) {
  | [(a1,a2), ...rest] => {
    Js.log(" ~~~ Pair " ++ print(a1) ++ ", " ++ print(a2));
    Js.log("  (const subs:" ++
      (subs.constants
          |> List.fold_left((s,(id,n)) => s ++ " ." ++ string_of_int(id) ++ "=" ++ print_const_num(n), ""))
      ++ ")");
    let (subs2, ctx2, result) = unify(~strict, subs, ctx, a1 |> sub(subs), a2 |> sub(subs));
    unify_all(~acc=[result,...acc], ~strict, subs2, ctx2, rest)
  }
  | [] => (subs, ctx, acc)
}

and unify = (~strict=true, subs, ctx, a, b) => {
  Js.log((strict ? "[!]" : "[~]") ++ " Unifying " ++ print(a) ++ " and " ++ print(b));
switch (a,b) {
  | (Var(x,xn), Var(y,yn)) => if (x == y) {
    (subs, ctx, Var(x,xn))
  } else {
    raise(TypeError("Incompatible type variables: " ++ xn ++ " != " ++ yn))
  }
  | (Var(id,name), _)
  | (_, Var(id,name)) => (subs |> push_type_sub(id,b), ctx, Var(id, name))

  | (BranchBlock(ty, AnyBranch), UBlock(terms)) =>
    /* Ensure block ends up branching */
    let inputTy = switch (ty) {
      | Var(id,name) => try(List.assoc(id, subs.types)) {
        | Not_found => raise(TypeError("Unresolved branch block input type: " ++ name))
      }
      | _ => ty
    };
    let resultType = ctx
      |> Context.pushNewSingle
      |> switch(inputTy){ | Void => (x=>x) | _ => Context.pushType(_, inputTy) }
      |> consume(_, terms)
      |> Context.topType;

    switch(resultType) {
      | Branch(_,_) as br => (subs, ctx, br)
      | other => raise(TypeError("Block must resolve to a branch (not " ++ print(other) ++ ")"))
    }

  | (BranchBlock(ty, branch), UBlock(_terms)) =>
    /* TODO: Ensure matching branch */
    (subs, ctx, BranchBlock(ty, branch))

  | (Block(Var(id1,name1) as v1, Var(id2,_name2) as v2), UBlock(terms)) =>
    /* id1 needs to be a concrete type so we can resolve the block */
    let inputTy = try(List.assoc(id1, subs.types)) {
      | Not_found => raise(TypeError("[internal] Unresolved block input type: " ++ name1))
    };
    unify(~strict, subs, ctx, Block(inputTy, v2), b)

  | (Branch(_,_), Branch(_,_)) when strict == false =>
    (subs, ctx, Branch(AnyBranch, Void))

  | (Block(inputTy, Var(id,_name) as var), UBlock(terms)) =>
    /* type var might have already been resolved */
    let (subs3, ctx3) = try(
      List.assoc(id, subs.types) |> ((_) => (subs, ctx))
    ) {
    | Not_found =>
      /* It hasn't, so calculate it */
      let ctx2 = ctx
        |> Context.pushNewSingle
        |> Context.pushType(_, inputTy)
        |> consume(_, terms);
      let ty = ctx2 |> Context.topType;
      (subs |> push_type_sub(id,ty), ctx2)
    };
    (subs3, ctx3, Block(inputTy, sub(subs3, var)))

  | (TypeCon(con1,args1), TypeCon(con2,args2)) =>
    if (con1 == con2 && Array.length(args1) == Array.length(args2)) {
      let (subs3, ctx3, args3)
       = zip_(Array.to_list(args1),Array.to_list(args2))
      |> unify_all(subs, ctx);
      (subs3, ctx3, TypeCon(con1, args3 |> Array.of_list))
    } else {
      raise(TypeError("Constructors do not match"))
    }

  | (Arr(ty1, r1), Arr(ty2, r2)) =>
    let (subs3, ctx3, ty3) = unify(~strict, subs, ctx, ty1, ty2);
    let (subs4, ctx4, r3) = Range.unify(strict, subs3, ctx3, r1, r2);
    (subs4, ctx4, Arr(ty3, r3))

  | (Num(r1),Num(r2)) =>
    let (subs2, ctx2, r3) = Range.unify(strict, subs, ctx, r1, r2);
    (subs2, ctx2, Num(r3))

  | (Str(r1),Str(r2)) =>
    let (subs2, ctx2, r3) = Range.unify(strict, subs, ctx, r1, r2);
    (subs2, ctx2, Str(r3))

  | (TypeSpec(NumSpec(r1)), TypeSpec(NumSpec(r2))) =>
    let (subs2, ctx2, r3) = Range.unify(strict, subs, ctx, r1, r2);
    (subs2, ctx2, TypeSpec(NumSpec(r3)))

  | (Bool,Bool) => (subs, ctx, Bool)
  | _ =>
    raise(TypeError("Incompatible types: " ++ print(a) ++ " != " ++ print(b)))
}};
