exception TypeError(string);


type literal =
  | StrLit(string)
  | NumLit(int);

type ty =
  | Str
  | Num
  | Fn(list(ty), ty)
  | Arr(ty)
  | DepType(string, list(ty) => ty)
  | Unit
  | Hole

and block =
  | Evaluated(ty)
  | Deferred(list(term))

and term =
  | Literal(literal)
  | Pop
  | Inv(fn, list(term))
  | Seq(list(term))

and fnDef = FnDef(string, ty)
and module_ = Module(string, list(fnDef))
and fn = Fn(module_, fnDef);



let rec print = (ty) => switch(ty) {
  | Str => "Str"
  | Num => "Num"
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

let rec eq = (a,b) => switch (a,b) {
  | (Str, Str) => true
  | (Num, Num) => true
  | _ => raise(TypeError("[eq] Cannot equate " ++ print(a) ++ " and " ++ print(b)))
};
