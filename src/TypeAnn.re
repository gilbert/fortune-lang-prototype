exception TypeAnnError(string);

module TypeAnnContext = {
  let id_counter = ref(0);

  let next_id = () => {
    id_counter := id_counter^ + 1;
    id_counter^
  };

  type t = {
    typeCons: list((string, int)),
    typeVars: list(T.ty /* Should only be T.Var */),
    constVars: list(T.const_num)
  };

  let pushConstVar = (ctx : t, cvar) => {
    ...ctx,
    constVars: [cvar, ...ctx.constVars]
  };
  let pushTypeVar = (ctx : t, cvar) => {
    ...ctx,
    typeVars: [cvar, ...ctx.typeVars]
  };

  let lookupTypeCons = (ctx : t, typeName, args) => {
    let (ty, len) = try(ctx.typeCons |> List.find(((name,_)) => name == typeName)) {
    | Not_found => raise(TypeAnnError("No such type: " ++ typeName))
    };
    let argCount = List.length(args);
    switch (argCount) {
      | n when n == len => T.TypeCon(typeName, args |> Array.of_list)
      | _ =>
        raise(TypeAnnError("Bad number of type arguments given to "
              ++ typeName ++ " (" ++ string_of_int(argCount) ++ " given, "
                                  ++ string_of_int(len) ++ " required)"))
    }
  };

  let findOrCreateConstVar = (ctx, varName) =>
    try(
      (ctx, ctx.constVars |> List.find((T.ConstNumVar(_id, name)) => name == varName))
    ) {
    | Not_found =>
      let new_var = T.ConstNumVar(next_id(), varName);
      (new_var |> pushConstVar(ctx), new_var)
    };

  let findOrCreateTypeVar = (ctx, varName) =>
    try(
      (ctx, ctx.typeVars |> List.find((T.Var(_id, name)) => name == varName))
    ) {
    | Not_found =>
      let new_var = T.Var(next_id(), varName);
      (new_var |> pushTypeVar(ctx), new_var)
    };
};

module Parser = BsLittleParser.MakeParser(TypeAnnContext);
open Parser;


/* Crude hack to solve mutual recursion */
let ty_ranged_cons_ : unit => (Input.t => ParseResult.t(T.ty))
  = [%raw {| function() { return ty_ranged_cons } |}];
let ty_cons_ : unit => (Input.t => ParseResult.t(T.ty))
  = [%raw {| function() { return ty_cons } |}];

let cap = regex([%bs.re "/[A-Z][a-zA-Z_0-9]*/"]);
let idf = regex([%bs.re "/[a-z][a-zA-Z_0-9]*/"]);
let num = regex([%bs.re "/[0-9]+/"]) ^^^ int_of_string;


let ty_kw = str("void") ^^^ (_s => T.Void);
let ty_var = idf ^^> TypeAnnContext.findOrCreateTypeVar;


let const_num = (idf ^^> TypeAnnContext.findOrCreateConstVar)
             <|> (num ^^^ (n => T.ConstNum(n)));

let calc_op = regex([%bs.re "/[+\\-]/"]);
let const_calc = (const_num <*> calc_op <*> const_num) ^^^ ((((a,op),b)) => switch(op) {
  | "+" => T.RangeAdd(a,b)
  | "-" => T.RangeSub(a,b)
  | _ => raise(TypeAnnError("Invalid const operator: " ++ op))
});

let range_val = const_calc <|> (const_num ^^^ (n => T.RangeVal(n)));

let rng = (chr('[') *> range_val <* chr(':') <*> range_val <* chr(']'))
      ^^^ (((min,max)) => T.Range(min,max));

let ty_plain = cap ^^> ((ctx, name) => switch(name) {
  | "Bool" => (ctx, T.Bool)
  | _ => (ctx, TypeAnnContext.lookupTypeCons(ctx, name, []))
});

let ty_ranged = (cap <*> rng) ^^^ (((name, range)) => switch (name) {
  | "Str" => T.Str(range)
  | "Num" => T.Num(range)
  | "NumSpec" => T.TypeSpec(T.NumSpec(range))
  | "Arr" => raise(TypeAnnError("Arr requires an item type: Arr" ++ T.print_range(range)))
  | _ => raise(TypeAnnError("No such ranged type: " ++ name))
});

let ty = ty_ranged_cons_ |<|> ty_ranged <|>| ty_cons_ <|> ty_plain <|> ty_kw <|> ty_var;

let typeArg = ty;

let typeArgs = (typeArg <*> rep(chr(',') *> typeArg))
       ^^^ ((first, rest)) => [first, ...rest];

let typeList = (chr('(') *> typeArgs <* chr(')'));

let ty_ranged_cons = (cap <*> rng <*> typeList) ^^^ ((((name, range), args)) => switch(name) {
  | "Arr" => switch(args |> List.length) {
    | 0 => raise(TypeAnnError("Arr requires an item type: Arr" ++ T.print_range(range) ++ "()"))
    | 1 => T.Arr(List.hd(args), range)
    | _ => raise(TypeAnnError("Arr given too many type arguments: Arr"
                ++ T.print_range(range) ++ "(" ++ T.print_types(args, ", ") ++ ")"))
    }
  | "NumSpec" =>
    raise(TypeAnnError("NumSpec does not accept type arguments: NumSpec("
                        ++ T.print_types(args, ", ") ++ ")"))
  | _ => raise(TypeAnnError("Unimplemented: " ++ name))
});

let ty_cons = (cap <*> typeList) ^^> ((ctx, (name, args)) => switch(name) {
  | "Str" | "Num" | "Arr" =>
    raise(TypeAnnError(name ++ " requires a range: "
          ++ name ++ "(" ++ T.print_types(args, ", ") ++ ")"))
  | "Block" => switch(args) {
    | [] | [_] =>
      raise(TypeAnnError("Block requires an input and output type: Block("
      ++ T.print_types(args, ", ") ++ ")"))
    | [in_,out_] => (ctx, T.Block(in_, out_))
    | _ => raise(TypeAnnError("Block given too many type arguments: Block("
                ++ T.print_types(args, ", ") ++ ")"))
    }
  | "BranchBlock" => switch(args) {
    | [] =>
      raise(TypeAnnError("BranchBlock requires an input type: BranchBlock("
                          ++ T.print_types(args, ", ") ++ ")"))
    | [in_] => (ctx, T.BranchBlock(in_, T.AnyBranch))
    | _ => raise(TypeAnnError("BranchBlock given too many type arguments: BranchBlock("
                               ++ T.print_types(args, ", ") ++ ")"))
    }
  | "NumSpec" =>
    raise(TypeAnnError("NumSpec requires a range (type arguments were given instead): NumSpec("
                        ++ T.print_types(args, ", ") ++ ")"))
  | _ =>
    (ctx, TypeAnnContext.lookupTypeCons(ctx, name, args))
});

let fn = (typeList <* str("=>") <*> ty)
     ^^^ (((args, ret)) => T.BasicFn(args, ret));

let annotation = fn <|> ty;

/* Public */
let compile = (typeCons, source) => {
  let input = Input.{
    text: source,
    index: 0,
    whitespace: " \n",
    context: {
      typeCons: typeCons,
      typeVars: [],
      constVars: []
    }
  };

  let result = input |> annotation |> ParseResult.getResult;
  switch (result) {
    | Ok((_ctx, ty)) => ty
    | Err(SyntaxErr(_line, _col, msg)) => switch (msg) {
      | "no_match" => raise(TypeAnnError("Invalid type annotation: " ++ source))
      | _ => raise(TypeAnnError("Syntax error: " ++ msg))
    }
  }
};
