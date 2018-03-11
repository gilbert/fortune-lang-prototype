exception TypeError(string);

type t =
  | Str
  | Num
  | Tuple(list(t))
  | Fn(list(t), t)
  | Arr(t)
  | DepType(string, list(t) => t)
  | Unit
  | Hole;

let rec print = (t) => switch(t) {
  | Str => "Str"
  | Num => "Num"
  | Tuple(types) => "(" ++ print_types(types, ", ") ++ ")"
  | Fn(args, ret) => "(" ++ print_types(args, ", ") ++ ") => " ++ print(ret)
  | Arr(t) => "Array(" ++ print(t) ++ ")"
  | DepType(name, _f) => "DepType(" ++ name ++ ")"
  | Unit => "unit"
  | Hole => "?";
}
and print_types = (types, sep) => switch (List.length(types)) {
  | 0 => ""
  | _ =>
    types
    |> List.tl
    |> List.fold_left((r, term) => r ++ sep ++ print(term), print(types |> List.hd))
};

let rec apply = (ftype, args) => switch (ftype) {
| Fn(params, ret) => ret
| DepType(_name, f) => f(args)
| _ => raise(TypeError("[apply] Type is not a function: " ++ print(ftype)))
};
