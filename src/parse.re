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

let input = Input.{
  text: source,
  index: 0,
  whitespace: " \n",
  context: Context.create(Run.stdlib)
};

let cap = regex([%bs.re "/[A-Z][a-zA-Z_0-9]*/"]);
let idf = regex([%bs.re "/[a-z][a-zA-Z_0-9]*[?]?/"]);

let str = regex([%bs.re "/\"[^\"]*\"/"]) ^^^ (x => T.StrLit(x));
let num = regex([%bs.re "/[1-9][0-9]*/"]) ^^^ (x => T.NumLit(x |> int_of_string));

let literal = (str <|> num) ^^^ (x => T.Literal(x));

let modFn = (cap <* chr('.') <*> idf) ^^> (ctx, (mName,fName)) => {
  (ctx, Context.lookup(ctx, mName, fName))
};

let pop = (chr('_') <* notPred(regex([%bs.re "/[a-zA-Z_0-9]/"]))) ^^^ ((_) => T.Pop);


/* Crude hack to solve mutual recursion */
let inv_ : unit => (Input.t => ParseResult.t(T.term))
  = [%raw {| function() { return inv } |}];
let block_ : unit => (Input.t => ParseResult.t(T.term))
  = [%raw {| function() { return block } |}];


let term = literal <|> pop <|>| inv_;

let arg = term <|>| block_;

let args = (arg <*> rep(chr(',') *> arg))
       ^^^ ((first, rest)) => [first, ...rest];

let argList = (chr('(') *> args <* chr(')'));

let inv = (modFn <*> argList) ^^^ ( ((fn, args)) => T.Inv(fn, args) );

let seq = rep(term);

let block = (chr('[') *> seq <* chr(']')) ^^^ (terms => T.BlockTerm(terms));


let program = seq ^^> ((ctx, terms) => {
  let seq = T.Seq(terms);
  let (ctx2, ty) = Apply.getType(ctx, seq);
  (ctx2, (seq, ty))
});


let subresult = input |> program;

let result = subresult |> ParseResult.getResult;

switch (result) {
| Some((_ctx, (seq, ty))) =>
    Term.print(seq) |> Js.log;
    T.print(ty) |> Js.log
| None => Js.log("Nothin")
};
