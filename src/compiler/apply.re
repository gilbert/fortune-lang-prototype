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

let range_val_common = (subs, v1, v2) => switch (v1,v2) {
  /* WARNING: Range unification relies on the (params, arg) invariant */
  | (RangeVal(ConstNumVar(id1,_)), RangeVal(ConstNum(n))) =>
    (subs |> push_range_sub(id1, ConstNum(n)), v2)
  | (RangeVal(ConstNumVar(id1,_)), RangeVal(ConstNumVar(id2,_))) =>
    if (id1 != id2) {
      raise(TypeError("Range type var mismatch: " ++ print_range_val(v1) ++ " and " ++ print_range_val(v2)))
    }
    else {
      (subs, v1)
    }
  | (RangeAdd(ConstNumVar(id,_),ConstNum(n)), RangeVal(ConstNum(m)))
  | (RangeAdd(ConstNum(n),ConstNumVar(id,_)), RangeVal(ConstNum(m))) =>
    (subs |> push_range_sub(id, ConstNum(m - n)), v2)

  | (RangeSub(ConstNumVar(id,_),ConstNum(n)), RangeVal(ConstNum(m))) =>
    (subs |> push_range_sub(id, ConstNum(m + n)), v2)
  | (RangeSub(ConstNum(n),ConstNumVar(id,_)), RangeVal(ConstNum(m))) =>
    (subs |> push_range_sub(id, ConstNum(m - n)), v2)
  | _ => raise(TypeError("Invalid range comparison: " ++ print_range_val(v1) ++ " and " ++ print_range_val(v2)))
};

let range_min_val = (subs, v1,v2) => switch (v1,v2) {
  | (RangeValMax, v) | (v, RangeValMax) => (subs, v)
  | (RangeVal(ConstNum(x)), RangeVal(ConstNum(y))) => (subs, RangeVal(ConstNum(min(x,y))))
  | _ => range_val_common(subs, v1, v2)
};

let range_max_val = (subs, v1,v2) => switch (v1,v2) {
  | (RangeValMax, _) | (_, RangeValMax) => (subs, RangeValMax)
  | (RangeVal(ConstNum(x)), RangeVal(ConstNum(y))) => (subs, RangeVal(ConstNum(max(x,y))))
  | _ => range_val_common(subs, v1, v2)
};

let merge_ranges = (subs, Range(x1,y1), Range(x2,y2)) => {
  let (subs2, minVal) = range_min_val(subs,  x1, x2);
  let (subs3, maxVal) = range_max_val(subs2, y1, y2);
  (subs3, Range(minVal,maxVal))
};

/* This is intended to be used after subs have been subbed */
let merge_type_ranges = (ty1, ty2) => switch(ty1,ty2) {
  | (Str(r1), Str(r2)) =>
    let (_, r3) = merge_ranges(empty_subs,r1,r2); Str(r3)
  | (Num(r1), Num(r2)) =>
    let (_, r3) = merge_ranges(empty_subs,r1,r2); Num(r3)
  | (Arr(a1, r1), Arr(_a2, r2)) =>
    let (_, r3) = merge_ranges(empty_subs,r1,r2); Arr(a1, r3)
  | _ => ty2
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

let unify_range = (subs, ctx, r1, r2) => {
  Js.log("Unifying range " ++ print_range(r1) ++ " and " ++ print_range(r2));
  let (subs2, r3) = merge_ranges(subs, r1,r2);
  Js.log("Unified range to:" ++ print_range(r3));
  (subs2, ctx)
};


let sub_num = (subs, c) => switch (c) {
  | ConstNumVar(id, name) =>
    let num = try(List.assoc(id, subs.constants)) {
      | Not_found => raise(TypeError("Constant variable not found: " ++ name))
    };
    Js.log("Subbing num const " ++ name ++ " = " ++ print_const_num(num));
    num
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
  | other => other
};

let sub_range = (subs, Range(x,y)) => Range(sub_range_val(subs, x), sub_range_val(subs, y));

let rec sub = (subs, ty) => switch (ty) {
  | Var(x, xn) =>
    try(List.assoc(x, subs.types)) {
      | Not_found => raise(TypeError("Type variable not found: " ++ xn))
    }
  | TypeCon(n, tys) => TypeCon(n, tys |> Array.map(sub(subs)))
  | Arr(ty, r) => Arr(ty |> sub(subs), r |> sub_range(subs))
  | Str(r) => Str(r |> sub_range(subs))
  | Num(r) => Num(r |> sub_range(subs))
  | Block(a,b) => Block(a |> sub(subs), b |> sub(subs))
  | other => other
};

let rec getType = (ctx : Context.t, term) => switch(term) {
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
    let (ctx3, termTypes) = terms
    |> List.fold_left( ((ctx,tys), term) => {
        let (ctx2, ty) = getType(ctx, term);
        (ctx2, [ty, ...tys])
      }, (ctx,[]));

    /* Make sure all types are compatible */
    let (ctx5, itemType) = termTypes
    |> List.fold_left( ((ctx, prev), termType) => {
        let (subs, ctx4) = unify(empty_subs, ctx, prev, termType);
        (ctx4, termType |> sub(subs) |> merge_type_ranges(prev))
      }, (ctx3, List.hd(termTypes)));

    (ctx5, Arr(itemType, makeRangeV(n,n)))
  }
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
  let (subs2, ctx2) = unify_all(empty_subs, ctx, zip(params,args));
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
  | (Var(id,_), _)
  | (_, Var(id,_)) => (subs |> push_type_sub(id,b), ctx)
  | (BranchBlock(ty, AnyBranch), UBlock(terms)) =>
    /* Ensure block ends up branching */
    let inputTy = switch (ty) {
      | Var(id,name) => try(List.assoc(id, subs.types)) {
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
    let inputTy = try(List.assoc(id1, subs.types)) {
      | Not_found => raise(TypeError("[internal] Unresolved block input type: " ++ name1))
    };
    /* id2 might have already been resolved */
    let (subs3, ctx3) = try(
      List.assoc(id2, subs.types) |> ((_) => (subs, ctx))
    ) {
    | Not_found =>
      /* It hasn't, so calculate it */
      let ctx2 = ctx
        |> Context.pushNewSingle
        |> Context.pushType(_, inputTy)
        |> consume(_, terms);
      let ty = ctx2 |> Context.topType;
      (subs |> push_type_sub(id2,ty), ctx2)
    };
    (subs3, ctx3)

  | (TypeCon(con1,args1), TypeCon(con2,args2)) =>
    if (con1 == con2 && Array.length(args1) == Array.length(args2)) {
      zip_(Array.to_list(args1),Array.to_list(args2)) |> unify_all(subs, ctx)
    } else {
      raise(TypeError("Constructors do not match"))
    }
  | (Arr(ty1, r1), Arr(ty2, r2)) =>
    let (subs3, ctx3) = unify(subs, ctx, ty1, ty2);
    unify_range(subs3, ctx3, r1, r2)

  | (Num(r1),Num(r2))
  | (Str(r1),Str(r2))
  | (TypeSpec(NumSpec(r1)), TypeSpec(NumSpec(r2))) =>
    unify_range(subs, ctx, r1, r2)

  | (Bool,Bool) => (subs, ctx)
  | _ =>
    raise(TypeError("Incompatible types: " ++ print(a) ++ " != " ++ print(b)))
}};
