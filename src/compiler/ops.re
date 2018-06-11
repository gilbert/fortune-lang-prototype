open T;

let num = x => Js.Json.number(x |> float_of_int);

let compileConstNumType = (n) => Js.Json.(switch(n) {
  | ConstNum(n) => num(n)
  | ConstNumVar(id, name) => array([| string("cvar"), num(id), string(name) |])
});

let compileRangeValType = (val_) => Js.Json.(switch(val_) {
  | RangeVal(n) => compileConstNumType(n)
  | RangeAdd(x,y) => array([| string("radd"), compileConstNumType(x), compileConstNumType(y) |])
  | RangeValMax => array([| string("rmax") |])
});

let compileRangeType = (Range(x,y)) =>
  Js.Json.array([| compileRangeValType(x), compileRangeValType(y) |]);

/* TODO: Fill out rest of types */
let rec compileType = (ty) => Js.Json.(switch (ty) {
  | Bool => array([| string("Bool") |])
  | Str(range) => array([| string("Str"), compileRangeType(range) |])
  | Num(range) => array([| string("Num"), compileRangeType(range) |])
  | Void => array([| string("void") |])
  | Arr(t,range) =>
    array([|
      string("Arr"),
      compileType(t),
      compileRangeType(range)
    |])
  | Tup(tys) =>
    array([|
      string("Tup"),
      tys |> Array.map(compileType) |> array
    |])
  | TypeCon(name, tys) =>
    array([|
      string("TypeCon"),
      string(name),
      tys |> Array.map(compileType) |> array
    |])
  | Branch(BranchPath(path), ty) =>
    array([|
      string("Branch"),
      array([| string("br_path"), string(path) |]),
      compileType(ty)
    |])
  | Branch(AnyBranch, ty) =>
    array([|
      string("Branch"),
      array([| string("br_any") |]),
      compileType(ty)
    |])
});


let rec compileAst = (term) => Js.Json.(switch (term) {
| Literal(StrLit(s)) => array([|string("lit"), string(s)|])
| Literal(NumLit(n)) => array([|string("lit"), num(n)|])
| Literal(ArrLit(terms)) =>
  array([|
    string("arr"),
    terms |> List.map(compileAst) |> Array.of_list |> array
  |])
| Literal(TupLit(terms)) =>
  array([|
    string("tup"),
    terms |> List.map(compileAst) |> Array.of_list |> array
  |])
| Pop => array([|string("pop")|])
| IfElse(cond, then_, else_) =>
  array([|
    string("if"),
    compileAst(cond),
    compileAst(then_),
    compileAst(else_),
  |])
| Seq(terms) | BlockTerm(terms) =>
  array([|
    string("seq"),
    terms |> List.map(compileAst) |> Array.of_list |> array
  |])
| Inv(Fn(Module(mod_,_), FnDef(fun_,_)), args) =>
  array([|
    string("inv"),
    string(mod_),
    string(fun_),
    args |> List.map(compileAst) |> Array.of_list |> array
  |])
| BranchInv(BranchFn(Module(mod_,_), BranchDef(fun_,_)), args) =>
  array([|
    string("br"),
    string(mod_),
    string(fun_),
    args |> List.map(compileAst) |> Array.of_list |> array
  |])
| BranchInv(BranchPath(path), _args) =>
  array([| string("brp"), string(path) |])
| BranchInv(AnyBranch, _args) =>
  raise(TypeError("[compileAst] Branch invocation should be resolved to a concrete branch"))
});
