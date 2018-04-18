
type t = T.context;
type st = T.single_context;

let push = (ctx : t, s_ctx) => {
  ...ctx,
  stacks: [s_ctx, ...ctx.stacks]
};

let pushBranchPath = (ctx : t, path) =>
  ctx.branchPaths |> List.mem(path)
    ? ctx
    : {
      ...ctx,
      branchPaths: [path, ...ctx.branchPaths]
    };

let addAvailableBranchPaths = (ctx : t, paths) =>
  {
    ...ctx,
    availableBranchPaths:
      ctx.availableBranchPaths
      |> T.BranchPathSet.union( T.BranchPathSet.of_list(paths) )
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
  try(List.hd(ctx.stacks).rtStack |> List.hd) {
    | Failure("hd") => T.Void
  };

let topRtStack = (ctx : t) => List.hd(ctx.stacks).rtStack;

let replaceTopRtStack = (ctx : t, rtStack) => push({
  ...ctx,
  stacks: try(List.tl(ctx.stacks)) {
    | Failure("hd") => raise(T.TypeError("No more stacks!"))
  }
}, { rtStack, tyVars: [] });

let create = (branches, mods, stack) => {
  let result : t = {
    branches: branches,
    modules: mods,
    stacks: [{ rtStack: stack, tyVars: [] }],
    branchPaths: [],
    availableBranchPaths: T.BranchPathSet.empty
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


let lookupBranchFn = (ctx : t, modName, fnName) => {
  let m = try(ctx.branches |> List.find((T.Module(name_,_)) => name_ == modName)) {
  | Not_found => raise(T.TypeError("No such branch module: " ++ modName))
  };
  let f = switch (m) {
    | T.Module(_,fns) => try(fns |> List.find((T.BranchDef(name_,_)) => name_ == fnName)) {
      | Not_found => raise(T.TypeError("No such branch function: " ++ modName ++ "." ++ fnName))
      }
  };
  T.BranchFn(m,f)
};


let rec lookupTyVar_ = (stacks : list(st), id, name) => switch (stacks) {
  | [top, ...rest] => try(List.assoc(id, top.tyVars)) {
    | Not_found => lookupTyVar_(rest, id, name)
  }
  | [] => raise(T.TypeError("No such type variable: " ++ name))
};

let lookupTyVar = (ctx : t, id, name) => lookupTyVar_(ctx.stacks, id, name);
