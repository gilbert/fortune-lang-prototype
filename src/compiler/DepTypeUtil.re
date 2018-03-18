
let (>>) = (f, g, x) => g(f(x));
let compose = (fns) => fns |> List.fold_left((f, g) => (x) => f(g(x)), (x => x));

let def = (fname, pipeline, endpoint) =>
  T.DepType(fname, compose(pipeline) >> endpoint);

let arity = (num) => ((ctx, args)) => switch (args |> List.length) {
  | n when n == num => (ctx, args)
  | n when n > num => raise(T.TypeError("Too many arguments"))
  | _ => raise(T.TypeError("Not enough arguments"))
}

/*let interface = (arg_i, fname)*/
