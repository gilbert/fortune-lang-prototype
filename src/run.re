
let stdlib = Context.[
  Module("String", [
    FnDef("split", Type.(Type.Fn([Str,Str], Arr(Str))) )
  ]),
  Module("IO", [
    FnDef("log", Type.DepType( args => switch (args |> List.length) {
    | 0 => raise(Type.TypeError("IO.log requires at least one argument"))
    | _ => args |> List.rev |> List.hd
    }))
  ]),
];

let make_context = () => {
  let ctx : Context.t = {
    modules: stdlib,
    stack: []
  };
  ctx
};
