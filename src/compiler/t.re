exception TypeError(string);


type literal =
  | StrLit(string)
  | NumLit(int);

type ty =
  | Str
  | Num
  | Bool
  | BasicFn(list((int,string)), list(ty), ty)
  | TypeCon(string, array(ty))
  | DepType(string, ((context, list(ty))) => (context, ty))
  | Var(int, string)
  | Unit
  | Hole
  | UBlock(list(term)) /* Unresolved Block */
  | Block(ty, ty)

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
  rtStack: list(ty),
  tyVars: list((int, string))
}
and context = {
  modules: list(module_),
  stacks: list(single_context)
};

let tyVarAssoc = List.map( (Var(id,name)) => (id,name) );


let rec print = (ty) => switch(ty) {
  | Str => "Str"
  | Num => "Num"
  | Bool => "Bool"
  | Var(id, num) => num ++ "." ++ string_of_int(id)
  | BasicFn(_vars, args, ret) => "(" ++ print_types(args, ", ") ++ ") => " ++ print(ret)
  | TypeCon(name, tys) => name ++ "(" ++ print_types(tys |> Array.to_list, ", ") ++ ")"
  | DepType(name, _f) => "DepType(" ++ name ++ ")"
  | UBlock(_) => "UBlock"
  | Block(a,b) => "[" ++ print(a) ++ " -> " ++ print(b) ++ "]"
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
