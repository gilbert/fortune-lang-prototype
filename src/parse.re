module Parser = BsLittleParser.MakeParser(Context);
open Parser;

let orElseLazy = (p, q, input) =>
  switch (p(input)) {
  | [@implicit_arity] ParseResult.ParseSuccess(s, t) =>
    [@implicit_arity] ParseResult.ParseSuccess(s, t)
  | ParseResult.ParseFailure(_) => (q())(input)
  };

let (<|>|) = (p, q) => orElseLazy(p,q);

let inspect = [%raw {|
  function(x) {
    console.log( require('util').inspect(x, { depth: 10 }) )
  }
|}];


let source = Node.Fs.readFileSync("./lab/basic.fortune", `utf8);

Js.log("Hello, BuckleScript and Reason!");
Js.log(source);

let module T = {
  type module_ = Module(string);
  type identifier = Id(string);
  type literal =
    | Str(string)
    | Num(int);
  type term =
    | Literal(literal)
    | Pop
    | Inv(Context.fn, list(term))
    | Seq(list(term));

  type tterm = (term, Type.t);

  let rec getType = (ctx : Context.t, term) => switch(term) {
  | Literal(Str(_)) => (ctx, Type.Str)
  | Literal(Num(_)) => (ctx, Type.Num)
  | Pop => Context.popType(ctx)
  | Inv(Fn(_, FnDef(_, ty)), args) =>
      let (ctx3, argTypes) = args
      |> List.fold_left( ((ctx,tys), term) => {
          let (ctx2, ty) = getType(ctx, term);
          (ctx2, [ty, ...tys])
        }, (ctx,[]));

      (ctx3, Type.apply(ty, argTypes |> List.rev))
  | Seq(terms) =>
      let ctx2 = consume(ctx, terms);
      (ctx2, ctx2 |> Context.topType)
  }

  and consume = (context : Context.t, terms) => {
    terms
    |> List.fold_left( (ctx, term) => {
        let (ctx2, ty) = getType(ctx, term);
        ty |> Context.pushType(ctx2)
      }, context)
  };

  let rec print = (term) => switch(term) {
    | Literal(Str(s)) => "Lit(\"" ++ s ++ "\")"
    | Literal(Num(n)) => "Lit(" ++ string_of_int(n) ++ ")"
    | Pop => "_"
    | Inv(Fn(Module(mod_,_), FnDef(fun_, _)), args) =>
        mod_ ++ "." ++ fun_ ++ "(" ++ print_terms(args, ", ") ++ ")"
    | Seq(terms) => print_terms(terms, " ")
  }
  and print_terms = (terms, sep) => switch (List.length(terms)) {
    | 0 => ""
    | _ =>
      terms
      |> List.tl
      |> List.fold_left((r, term) => r ++ sep ++ print(term), print(terms |> List.hd))
  };
};



let input = Input.{
  text: source,
  index: 0,
  whitespace: " \n",
  context: Context.create(Run.stdlib)
};

let cap = regex([%bs.re "/[A-Z][a-zA-Z_0-9]*/"]);
let idf = regex([%bs.re "/[a-z][a-zA-Z_0-9]*/"]);

let str = regex([%bs.re "/\"[^\"]*\"/"]) ^^^ (x => T.Str(x));
let num = regex([%bs.re "/[1-9][0-9]*/"]) ^^^ (x => T.Num(x |> int_of_string));

let literal = (str <|> num) ^^^ (x => T.Literal(x));

let modFn = (cap <* chr('.') <*> idf) ^^> (ctx, (mName,fName)) => {
  (ctx, ctx |> Context.lookup(mName, fName))
};

let pop = (chr('_') <* notPred(regex([%bs.re "/[a-zA-Z_0-9]/"]))) ^^^ ((_) => T.Pop);


/* Crude hack to solve mutual recursion */
let inv_ : unit => (Input.t => ParseResult.t(T.term))
  = [%raw {| function() { return inv } |}];


let term = literal <|> pop <|>| inv_;

let args = (term <*> rep(chr(',') *> term))
       ^^^ ((first, rest)) => [first, ...rest];

let argList = (chr('(') *> args <* chr(')'));

let inv = (modFn <*> argList) ^^^ ( ((fn, args)) => T.Inv(fn, args) );


let program = rep(term) ^^> ((ctx, terms) => {
  let seq = T.Seq(terms);
  let (ctx2, ty) = T.getType(ctx, seq);
  (ctx2, (seq, ty))
});


let subresult = input |> program;

let result = subresult |> ParseResult.getResult;

switch (result) {
| Some((_ctx, (seq, ty))) =>
    T.print(seq) |> Js.log;
    Type.print(ty) |> Js.log
| None => Js.log("Nothin")
};
