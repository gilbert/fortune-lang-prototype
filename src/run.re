open DepTypeUtil;

let stdlib = T.{
  modules: [
    Module("Str", [
      FnDef("split", BasicFn([Str,Str], TypeCon("Array", [|Str|])) ),
      FnDef("upcase", BasicFn([Str], Str) ),
      FnDef("cap", BasicFn([Str], Str) ),
      FnDef("toNum", BasicFn([Str], TypeCon("Maybe", [|Num|])) )
    ]),
    Module("Num", [
      FnDef("add", BasicFn([Num, Num], Num) ),
    ]),
    Module("Arr", [
      FnDef("get", {
        let a = Var(next_id(), "a");
        BasicFn([TypeCon("Array", [|a|]), Num], a)
      }),
      FnDef("map", {
        let a = Var(next_id(), "a");
        let b = Var(next_id(), "b");
        BasicFn([TypeCon("Array", [|a|]), Block(a,b)], TypeCon("Array", [|b|]))
      }),
    ]),
    Module("IO", [
      FnDef("log", DepType("IO.log", ((ctx, args)) => switch (args |> List.length) {
      | 0 => raise(TypeError("IO.log requires at least one argument"))
      | _ => (ctx, Unit)
      }))
    ]),
    Module("Maybe", [
      FnDef("unwrap!", {
        let ty = Var(next_id(), "ty");
        BasicFn([TypeCon("Maybe", [|ty|]), BranchBlock(Unit, AnyBranch)], ty)
      }),
      /* m(Maybe(a)) => m(a) where m.filter :: (m(a), [a -> Bool]) => m(b) */
      FnDef("filter", def("Maybe.filter", [arity(1)], ((ctx, args)) => switch (args) {
      | [TypeCon("Maybe", [|_|])] => (ctx, Bool)
      | other => raise(TypeError("Expected Maybe(type), given " ++ print_types(other, ", ")))
      }))
    ]),
  ],

  branches: [
    Module("Program", [
      BranchDef("exit", BasicFn([Str, Num], Num))
    ])
  ],

  stacks: []
};
