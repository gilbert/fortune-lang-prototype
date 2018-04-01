exception TypeError(string);

let id_counter = ref(0);

let next_id = () => {
  id_counter := id_counter^ + 1;
  id_counter^
};

type num_literal =
  | ConstNum(int)
  | ConstNumVar(int, string);

type range_val =
  | RangeVal(num_literal)
  | RangeAdd(num_literal, num_literal)
  | RangeValMax
  | RangeValMin;

type range = Range(range_val, range_val);
let makeRangeV = (x,y) => Range(RangeVal(x), RangeVal(y));
let makeRangeN = (n) => Range(RangeVal(ConstNum(n)), RangeVal(ConstNum(n)));
let makeRangeAdd = (x1,y1,x2,y2) => Range(RangeAdd(x1,x2), RangeAdd(y1,y2));
let makeRangeAny = (a,b) => Range(RangeVal(ConstNumVar(next_id(), a)), RangeVal(ConstNumVar(next_id(), b)));


type tySpec =
  | StrSpec(range)
  | NumSpec(range);

type literal =
  | StrLit(string)
  | ArrLit(list(term))
  | NumLit(int)

and ty =
  | Str(range)
  | Num(range)
  | Bool
  | BasicFn(list(ty), ty)
  | Arr(ty, range)
  | TypeCon(string, array(ty))
  | DepType(string, ((context, list(ty))) => (context, ty))
  | Var(int, string)
  | Unit
  | Hole
  | UBlock(list(term)) /* Unresolved Block */
  | Block(ty, ty)
  | BranchBlock(ty, branchFn)
  | TypeSpec(tySpec)

and term =
  | Literal(literal)
  | Pop
  | Inv(fn, list(term))
  | BranchInv(branchFn, list(term))
  | Seq(list(term))
  | BlockTerm(list(term))

and module_('a) = Module(string, list('a))
and fn = Fn(module_(fnDef), fnDef)
and fnDef = FnDef(string, ty)

and branchFn = BranchFn(module_(branchDef), branchDef) | AnyBranch
and branchDef = BranchDef(string, ty)

and single_context = {
  rtStack: list(ty),
  tyVars: list((int, string))
}
and context = {
  branches: list(module_(branchDef)),
  modules: list(module_(fnDef)),
  stacks: list(single_context)
};

let tyVarAssoc = List.map( (Var(id,name)) => (id,name) );


let print_var = (id,name) => name ++ "." ++ string_of_int(id);

let print_num_literal = (lit) => switch(lit) {
  | ConstNum(n) => string_of_int(n)
  | ConstNumVar(id, name) => print_var(id, name)
};

let print_range_val = (v) => switch(v) {
  | RangeVal(v) => print_num_literal(v)
  | RangeAdd(x,y) => print_num_literal(x) ++ " + " ++ print_num_literal(y)
  | RangeValMax => "MAX"
};
let print_range = (Range(x,y)) => "[" ++ print_range_val(x) ++ ":" ++ print_range_val(y) ++ "]";

let rec print = (ty) => switch(ty) {
  | Str(range) => "Str" ++ print_range(range)
  | Num(range) => "Num" ++ print_range(range)
  | Bool => "Bool"
  | Var(id, name) => print_var(id,name)
  | BasicFn(args, ret) => "(" ++ print_types(args, ", ") ++ ") => " ++ print(ret)
  | TypeCon(name, tys) => name ++ "(" ++ print_types(tys |> Array.to_list, ", ") ++ ")"
  | Arr(ty, range) => "Arr(" ++ print(ty) ++ ")" ++ print_range(range)
  | DepType(name, _f) => "DepType(" ++ name ++ ")"
  | BranchBlock(ty, AnyBranch) =>
    "@branch(" ++ print(ty) ++ " -> any)"
  | BranchBlock(ty, BranchFn(Module(mod_,_), BranchDef(fun_,_ty))) =>
    "@branch(" ++ print(ty) ++ " -> " ++ mod_ ++ "." ++ fun_ ++ ")"
  | UBlock(_) => "UBlock"
  | Block(a,b) => "[" ++ print(a) ++ " -> " ++ print(b) ++ "]"
  | Unit => "unit"
  | Hole => "?"
  | TypeSpec(StrSpec(r)) => "StrSpec" ++ print_range(r)
  | TypeSpec(NumSpec(r)) => "NumSpec" ++ print_range(r);
}
and print_types = (types, sep) => switch (List.length(types)) {
  | 0 => ""
  | _ =>
    types
    |> List.tl
    |> List.fold_left((r, term) => r ++ sep ++ print(term), print(types |> List.hd))
};
