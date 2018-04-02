open DepTypeUtil;

let stdtypes = [("Maybe",1)];
let ann = (source) => TypeAnn.compile(stdtypes, source);

let stdlib = T.{
  modules: [
    Module("Str", [
      FnDef("split", ann("(Str[0:s1], Str[1:s2]) => Arr[1:s1+1](Str[0:s1])")),
      FnDef("upcase", ann("(Str[0:n]) => Str[0:n]")),
      FnDef("cap", ann("(Str[0:n]) => Str[0:n]")),
      FnDef("toNum", ann("(Str[0:n], NumSpec[min:max]) => Maybe(Num[min:max])")),
      FnDef("toNumBounded", ann("(Str[0:n], NumSpec[min:max]) => Maybe(Num[min:max])")),
    ]),
    Module("Num", [
      FnDef("spec", def("Num.spec",
        [arity(2), requireConstNum(0), requireConstNum(1)],
        ((ctx, args)) => switch (args) {
        | [Num(Range(min, _)), Num(Range(max, _))] =>
          (ctx, TypeSpec(NumSpec(Range(min, max))))
        | _ => raise(TypeError("Unreachable."))
        })
      ),
      FnDef("add", ann("(Num[x1:y1], Num[x2:y2]) => Num[x1+x2:y1+y2]")),
    ]),
    Module("Arr", [
      FnDef("get", ann("(Arr[1:n](itemType), Num[0:n-1]) => itemType")),
      FnDef("map", ann("(Arr[n:m](a), Block(a,b)) => Arr[n:m](b)")),
    ]),
    Module("IO", [
      FnDef("log", DepType("IO.log", ((ctx, args)) => switch (args |> List.length) {
      | 0 => raise(TypeError("IO.log requires at least one argument"))
      | _ => (ctx, Unit)
      }))
    ]),
    Module("Maybe", [
      FnDef("unwrap!", ann("(Maybe(ty), BranchBlock(void)) => ty")),
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
