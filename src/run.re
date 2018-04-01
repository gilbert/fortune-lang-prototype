open DepTypeUtil;

let stdlib = T.{
  modules: [
    Module("Str", [
      FnDef("split", {
        let s1var = ConstNumVar(next_id(), "s1");
        let s1 = makeRangeV(ConstNum(0), s1var);
        let s2 = makeRangeV(ConstNum(0), ConstNumVar(next_id(), "s2"));
        BasicFn([Str(s1),Str(s2)], Arr(Str(s1), Range(RangeVal(ConstNum(1)), RangeAdd(s1var, ConstNum(1)))))
      }),
      FnDef("upcase", {
        let s = Str(makeRangeV(ConstNum(0), ConstNumVar(next_id(), "n")));
        BasicFn([s], s)
      }),
      FnDef("cap", {
        let s = Str(makeRangeV(ConstNum(0), ConstNumVar(next_id(), "n")));
        BasicFn([s], s)
      }),
      FnDef("toNum", {
        let s = Str(makeRangeV(ConstNum(0), ConstNumVar(next_id(), "n")));
        let r = makeRangeAny("min","max");
        BasicFn([s, TypeSpec(NumSpec(r))], TypeCon("Maybe", [|Num(r)|]))
      })
    ]),
    Module("Num", [
      FnDef("spec", def("Num.spec",
        [arity(2), requireConstNum(0), requireConstNum(1)],
        ((ctx, args)) => switch (args) {
        | [Num(Range(min, _)), Num(Range(max, _))] =>
          (ctx, TypeSpec(NumSpec(Range(min, max))))
        | other => raise(TypeError("Unreachable."))
        })
      ),
      FnDef("add", {
        let x1 = ConstNumVar(next_id(), "x1");
        let x2 = ConstNumVar(next_id(), "x2");
        let y1 = ConstNumVar(next_id(), "y1");
        let y2 = ConstNumVar(next_id(), "y2");
        let a = makeRangeV(x1,y1);
        let b = makeRangeV(x2,y2);
        let c = makeRangeAdd(x1,y1,x2,y2);
        BasicFn([Num(a), Num(b)], Num(c))
      }),
    ]),
    Module("Arr", [
      FnDef("get", {
        let a = Var(next_id(), "a");
        let n = ConstNumVar(next_id(), "n");
        let i = Range(RangeVal(ConstNum(0)), RangeAdd(n, ConstNum(-1)));
        BasicFn([Arr(a, makeRangeV(ConstNum(1), n)), Num(i)], a)
      }),
      FnDef("map", {
        let a = Var(next_id(), "a");
        let b = Var(next_id(), "b");
        let n = ConstNumVar(next_id(), "n");
        BasicFn([Arr(a, makeRangeV(n,n)), Block(a,b)], Arr(b, makeRangeV(n,n)))
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
      BranchDef("exit", {
        let s = Str(Range(RangeVal(ConstNum(0)), RangeValMax));
        let n = Num(makeRangeV(ConstNum(0), ConstNum(99)));
        BasicFn([s, n], n)
      })
    ])
  ],

  stacks: []
};
