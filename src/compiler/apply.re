
let rec apply = (ctx, ftype, args) => switch (ftype) {
| T.SimpleFn(params, ret) => (ctx, ret)
| T.DepType(_name, f) => f(ctx, args)
| _ => raise(T.TypeError("[apply] Type is not a function: " ++ T.print(ftype)))
};
