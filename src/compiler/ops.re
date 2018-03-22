open T;

let rec compileAst = (term) => Js.Json.(switch (term) {
| Literal(StrLit(s)) => array([|string("lit"), string(s)|])
| Literal(NumLit(n)) => array([|string("lit"), number(n |> float_of_int)|])
| Literal(ArrLit(t)) =>
  array([|
    string("arr"),
    t |> List.map(compileAst) |> Array.of_list |> array
  |])
| Pop => array([|string("pop")|])
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
| BranchInv(AnyBranch, _args) =>
  raise(TypeError("[compileAst] Branch invocation should be resolved to a concrete branch"))
});
