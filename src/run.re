
let stdlib = Context.({
  modules: [
    Module("String", [
      FnDef("split", Type.(Tuple([Str,Str])) )
    ]),
    Module("IO", [
      FnDef("log", Type.Str)
    ]),
  ]
})
