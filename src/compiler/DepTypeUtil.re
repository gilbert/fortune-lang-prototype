
let (>>) = (f, g, x) => g(f(x));
let compose = (fns) => fns |> List.fold_left((f, g) => (x) => f(g(x)), (x => x));

let def = (fname, pipeline, endpoint) =>
  T.DepType(fname, compose(pipeline) >> endpoint);

let arity = (num) => ((ctx, args)) => switch (args |> List.length) {
  | n when n == num => (ctx, args)
  | n when n > num => raise(T.TypeError("Too many arguments"))
  | _ => raise(T.TypeError("Too few arguments"))
};

let requireConstNum = (i) => ((ctx, args)) => T.(switch (args |> List.nth(_, i)) {
  | Num(Range(RangeVal(ConstNum(n1)), RangeVal(ConstNum(n2)))) when n1 == n2 => (ctx, args)
  | _ => raise(T.TypeError("Arg type " ++ string_of_int(i) ++ " is not a num const"))
});
