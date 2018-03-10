open BsLittleParser.Parser;
let (^^^) = BsLittleParser.Parser.(^^);

let orElseLazy = (p, q, input) =>
  switch (p(input)) {
  | [@implicit_arity] BsLittleParser.ParseResult.ParseSuccess(s, t) =>
    [@implicit_arity] BsLittleParser.ParseResult.ParseSuccess(s, t)
  | BsLittleParser.ParseResult.ParseFailure(_) => (q())(input)
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
    | Seq(list(term), Type.t);

  let getType = (ctx : Context.t, term) => switch(term) {
  | Literal(Str(_)) => Type.Str
  | Literal(Num(_)) => Type.Num
  | Pop => List.hd(ctx.stack)
  | Inv(Fn(_, FnDef(_, ty)), _) => ty
  | Seq(_, ty) => ty
  };

  let rec print = (term) => switch(term) {
    | Literal(Str(s)) => "Lit(\"" ++ s ++ "\")"
    | Literal(Num(n)) => "Lit(" ++ string_of_int(n) ++ ")"
    | Pop => "_"
    | Inv(Fn(Module(mod_,_), FnDef(fun_, _)), args) =>
        mod_ ++ "." ++ fun_ ++ "(" ++ print_terms(args, ", ") ++ ")"
    | Seq(terms, _) => print_terms(terms, " ")
  }
  and print_terms = (terms, sep) => switch (List.length(terms)) {
    | 0 => ""
    | _ =>
      terms
      |> List.tl
      |> List.fold_left((r, term) => r ++ sep ++ print(term), print(terms |> List.hd))
  };
};



let ctx = Run.make_context();


let input = BsLittleParser.Input.{text: source, index: 0, whitespace: " \n"};

let cap = regex([%bs.re "/[A-Z][a-zA-Z_0-9]*/"]);
let idf = regex([%bs.re "/[a-z][a-zA-Z\\-0-9]*/"]);

let str = regex([%bs.re "/\"[^\"]*\"/"]) ^^^ (x => T.Str(x));
let num = regex([%bs.re "/[1-9][0-9]*/"]) ^^^ (x => T.Num(x |> int_of_string));

let literal = (str <|> num) ^^^ (x => T.Literal(x));

let mod_ = cap ^^^ (x => T.Module(x));

let modFn = (mod_ <* chr('.') <*> idf) ^^^ ((T.Module(mName),fName)) => {
  ctx.modules |> Context.lookup(mName, fName)
};

let pop = (chr('_') <* notPred(regex([%bs.re "/[a-zA-Z_0-9]/"]))) ^^^ (_x => T.Pop);


/* Crude hack to solve mutual recursion */
let inv_ : unit => (BsLittleParser.Input.t => BsLittleParser.ParseResult.t(T.term))
  = [%raw {| function() { return inv } |}];


let term = literal <|> pop <|>| inv_;

let args = (term <*> rep(chr(',') *> term))
       ^^^ ((first, rest)) => [first, ...rest];

let argList = (chr('(') *> args <* chr(')'));

let inv = (modFn <*> argList) ^^^ ( ((fn, args)) => T.Inv(fn, args) );



let program = rep(term) ^^^ (terms => T.Seq(terms, Type.Bottom));


let subresult = input |> program;

let result = subresult |> BsLittleParser.ParseResult.getResult;

switch (result) {
| Some(term) => T.print(term) |> Js.log
| None => Js.log("Nothin")
};
