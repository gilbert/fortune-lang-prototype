
type fnDef = FnDef(string, Type.t);
type module_ = Module(string, list(fnDef));

type fn = Fn(module_, fnDef);

type t = {
  modules: list(module_),
  mutable stack: list(Type.t)
};

let lookup = (modName, fnName, ctx) => {
  let m = try(ctx |> List.find((Module(name_,_)) => name_ == modName)) {
  | Not_found => raise(Type.TypeError("No such module: " ++ modName))
  };
  let f = switch (m) {
    | Module(_,fns) => fns |> List.find((FnDef(name_,_)) => name_ == fnName)
  };
  Fn(m,f)
};
