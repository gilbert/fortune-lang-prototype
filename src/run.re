open DepTypeUtil;

let id_counter = ref(0);

let next_id = () => {
  id_counter := id_counter^ + 1;
  id_counter^
};

let stdlib = T.[
  Module("Str", [
    FnDef("split", BasicFn([], [Str,Str], TypeCon("Array", [|Str|])) ),
    FnDef("upcase", BasicFn([], [Str], Str) ),
    FnDef("to_num", BasicFn([], [Str], TypeCon("Maybe", [|Num|])) )
  ]),
  Module("Num", [
  ]),
  Module("Arr", [
    FnDef("map", {
      let a = Var(next_id(), "a");
      let b = Var(next_id(), "b");
      BasicFn([a,b] |> tyVarAssoc, [TypeCon("Array", [|a|]), Block(a,b)], TypeCon("Array", [|b|]))
    }),
  ]),
  Module("IO", [
    FnDef("log", DepType("IO.log", ((ctx, args)) => switch (args |> List.length) {
    | 0 => raise(TypeError("IO.log requires at least one argument"))
    | _ => (ctx, Unit)
    }))
  ]),
  Module("Maybe", [
    /* m(Maybe(a)) => m(a) where m.filter :: (m(a), [a -> Bool]) => m(b) */
    FnDef("filter", def("Maybe.filter", [arity(1)], ((ctx, args)) => switch (args) {
    | [TypeCon("Maybe", [|_|])] => (ctx, Bool)
    | other => raise(TypeError("Expected Maybe(type), given " ++ print_types(other, ", ")))
    }))
  ]),
];
