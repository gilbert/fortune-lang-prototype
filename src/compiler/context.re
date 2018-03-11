
type t = T.context;
type st = T.single_context;

let push = (ctx : t, s_ctx) => {
  ...ctx,
  stacks: [s_ctx, ...ctx.stacks]
};

let pushNewSingle = (ctx) => push(ctx, {
  rtStack: []
});

let pushType_ = (single_ctx : st, ty) => {
  ...single_ctx,
  rtStack: [ty, ...single_ctx.rtStack]
};
let pushType = (ctx : t, ty) => {
  ...ctx,
  stacks: [ty |> pushType_(ctx.stacks |> List.hd), ...ctx.stacks]
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
    stacks: [{ rtStack: [] }]
  };
  result
};

let lookup = (ctx : t, modName, fnName) => {
  let m = try(ctx.modules |> List.find((T.Module(name_,_)) => name_ == modName)) {
  | Not_found => raise(T.TypeError("No such module: " ++ modName))
  };
  let f = switch (m) {
    | T.Module(_,fns) => fns |> List.find((T.FnDef(name_,_)) => name_ == fnName)
  };
  T.Fn(m,f)
};
