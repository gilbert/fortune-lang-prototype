
type single_context = {
  rtStack: list(T.ty)
};

type t = {
  modules: list(T.module_),
  stacks: list(single_context)
};

let push = (ctx, s_ctx) => {
  ...ctx,
  stacks: [s_ctx, ...ctx.stacks]
};

let pushType_ = (single_ctx, ty) => {
  ...single_ctx,
  rtStack: [ty, ...single_ctx.rtStack]
};
let pushType = (ctx, ty) => {
  ...ctx,
  stacks: [ty |> pushType_(ctx.stacks |> List.hd), ...ctx.stacks]
};

let popType = (ctx) => {
  let s_ctx = List.hd(ctx.stacks);
  switch (s_ctx.rtStack) {
  | [top, ...rest] => ({ ...ctx, stacks: [{ ...s_ctx, rtStack: rest }, ...List.tl(ctx.stacks)] }, top)
  | _ => raise(T.TypeError("Runtime stack is empty!"))
  }
};

let topType = (ctx) =>
  List.hd(ctx.stacks).rtStack
  |> List.hd;

let create = (mods) => {
  modules: mods,
  stacks: [{ rtStack: [] }]
};

let lookup = (modName, fnName, ctx) => {
  let m = try(ctx.modules |> List.find((T.Module(name_,_)) => name_ == modName)) {
  | Not_found => raise(T.TypeError("No such module: " ++ modName))
  };
  let f = switch (m) {
    | T.Module(_,fns) => fns |> List.find((T.FnDef(name_,_)) => name_ == fnName)
  };
  T.Fn(m,f)
};
