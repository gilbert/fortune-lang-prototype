exception TypeError(string);


type literal =
  | StrLit(string)
  | NumLit(int);

type ty =
  | Str
  | Num
  | SimpleFn(list(ty), ty)
  | TypeCon(string, array(ty))
  | DepType(string, (context, list(ty)) => (context, ty))
  | Unit
  | Hole
  | Block(list(term))

and term =
  | Literal(literal)
  | Pop
  | Inv(fn, list(term))
  | Seq(list(term))
  | BlockTerm(list(term))

and fnDef = FnDef(string, ty)
and module_ = Module(string, list(fnDef))
and fn = Fn(module_, fnDef)

and single_context = {
  rtStack: list(ty)
}
and context = {
  modules: list(module_),
  stacks: list(single_context)
};



let rec print = (ty) => switch(ty) {
  | Str => "Str"
  | Num => "Num"
  | SimpleFn(args, ret) => "(" ++ print_types(args, ", ") ++ ") => " ++ print(ret)
  | TypeCon(name, tys) => name ++ "(" ++ print_types(tys |> Array.to_list, ", ") ++ ")"
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

let rec eq = (a,b) => switch (a,b) {
  | (Str, Str) => true
  | (Num, Num) => true
  | _ => raise(TypeError("[eq] Cannot equate " ++ print(a) ++ " and " ++ print(b)))
};
