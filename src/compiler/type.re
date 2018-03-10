exception TypeError(string);

type t =
  | Str
  | Num
  | Tuple(list(t));
