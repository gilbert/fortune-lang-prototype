open DepTypeUtil;

let stdlib = T.[
  Module("String", [
    FnDef("split", SimpleFn([Str,Str], TypeCon("Array", [|Str|])) ),
    FnDef("upcase", SimpleFn([Str], Str) )
  ]),
  Module("Array", [
    FnDef("map", map("Array.map", "Array"))
  ]),
  Module("IO", [
    FnDef("log", DepType("IO.log", (ctx, args) => switch (args |> List.length) {
    | 0 => raise(TypeError("IO.log requires at least one argument"))
    | _ => (ctx, Unit)
    }))
  ]),
];
