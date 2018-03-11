
type fnDef = FnDef(string, Type.t);
type module_ = Module(string, list(fnDef));

type fn = Fn(module_, fnDef);
type var = Var(string, Type.t);

type single_context = {
  vars: list(var),
  rtStack: list(Type.t)
};

type t = {
  modules: list(module_),
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
  | _ => raise(Type.TypeError("Runtime stack is empty!"))
  }
};

let topType = (ctx) =>
  List.hd(ctx.stacks).rtStack
  |> List.hd;

let create = (mods) => {
  modules: mods,
  stacks: [{ vars: [], rtStack: [] }]
};

let lookup = (modName, fnName, ctx) => {
  let m = try(ctx.modules |> List.find((Module(name_,_)) => name_ == modName)) {
  | Not_found => raise(Type.TypeError("No such module: " ++ modName))
  };
  let f = switch (m) {
    | Module(_,fns) => fns |> List.find((FnDef(name_,_)) => name_ == fnName)
  };
  Fn(m,f)
};
