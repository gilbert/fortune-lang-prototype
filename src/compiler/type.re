exception TypeError(string);

type stack = list(t)

and t =
  | Str
  | Num
  | Tuple(stack)
  | Seq(stack, t)
  | Fn(list(t), t)
  | Arr(t)
  | DepType(list(t) => t)
  | Bottom;
