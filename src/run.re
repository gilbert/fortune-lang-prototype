
let stdlib = T.[
  Module("String", [
    FnDef("split", Fn([Str,Str], Arr(Str)) )
  ]),
  Module("IO", [
    FnDef("log", DepType("IO.log", args => switch (args |> List.length) {
    | 0 => raise(TypeError("IO.log requires at least one argument"))
    | _ => Unit
    }))
  ]),
];
