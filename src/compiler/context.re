
type t = T.context;
type st = T.single_context;

let push = (ctx : t, s_ctx) => {
  ...ctx,
  stacks: [s_ctx, ...ctx.stacks]
};

let pushNewSingle = (ctx) => push(ctx, {
  rtStack: [],
  tyVars: []
});

let pushType_ = (single_ctx : st, ty) => {
  ...single_ctx,
  rtStack: [ty, ...single_ctx.rtStack]
};
let pushType = (ctx : t, ty) => {
  ...ctx,
  stacks: [ty |> pushType_(ctx.stacks |> List.hd), ...List.tl(ctx.stacks)]
};

let pushTypeVar_ = (single_ctx : st, tvars) => {
  ...single_ctx,
  tyVars: List.append(tvars, single_ctx.tyVars)
};
let pushTypeVars = (ctx : t, tvars) => {
  ...ctx,
  stacks: [tvars |> pushTypeVar_(ctx.stacks |> List.hd), ...List.tl(ctx.stacks)]
};

let popType = (ctx : t) => {
  let s_ctx = List.hd(ctx.stacks);
  switch (s_ctx.rtStack) {
  | [top, ...rest] => ({ ...ctx, stacks: [{ ...s_ctx, rtStack: rest }, ...List.tl(ctx.stacks)] }, top)
  | _ => raise(T.TypeError("Runtime stack is empty!"))
  }
};

let topType = (ctx : t) =>
  List.hd(ctx.stacks).rtStack
  |> List.hd;

let create = (mods) => {
  let result : t = {
    modules: mods,
    stacks: [{ rtStack: [], tyVars: [] }]
  };
  result
};

let lookup = (ctx : t, modName, fnName) => {
  let m = try(ctx.modules |> List.find((T.Module(name_,_)) => name_ == modName)) {
  | Not_found => raise(T.TypeError("No such module: " ++ modName))
  };
  let f = switch (m) {
    | T.Module(_,fns) => try(fns |> List.find((T.FnDef(name_,_)) => name_ == fnName)) {
      | Not_found => raise(T.TypeError("No such function: " ++ modName ++ "." ++ fnName))
      }
  };
  T.Fn(m,f)
};


let rec lookupTyVar_ = (stacks : list(st), id, name) => switch (stacks) {
  | [top, ...rest] => try(List.assoc(id, top.tyVars)) {
    | Not_found => lookupTyVar_(rest, id, name)
  }
  | [] => raise(T.TypeError("No such type variable: " ++ name))
};

let lookupTyVar = (ctx : t, id, name) => lookupTyVar_(ctx.stacks, id, name);
