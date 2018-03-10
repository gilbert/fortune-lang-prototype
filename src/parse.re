open BsLittleParser.Parser;
let (^^^) = BsLittleParser.Parser.(^^);

let orElseLazy = (p, q, input) =>
  switch (p(input)) {
  | [@implicit_arity] BsLittleParser.ParseResult.ParseSuccess(s, t) =>
    [@implicit_arity] BsLittleParser.ParseResult.ParseSuccess(s, t)
  | BsLittleParser.ParseResult.ParseFailure(_) => Lazy.force(q)(input)
  };

let (<|>|) = (p, q) => orElseLazy(p,q);

let inspect = [%raw {|
  function(x) {
    return require('util').inspect(x, { depth: 10 })
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
    | Inv(Context.fn, list(term));
};



let ctx = Run.stdlib;


let input = BsLittleParser.Input.{text: source, index: 0, whitespace: " \n"};

let cap = regex([%bs.re "/[A-Z][a-zA-Z_0-9]*/"]);
let idf = regex([%bs.re "/[a-z][a-zA-Z\\-0-9]*/"]);

let str = (chr('"') *> regex([%bs.re "/[^\"]*/"]) <* chr('"')) ^^^ (x => T.Str(x));
let num = regex([%bs.re "/[1-9][0-9]*/"]) ^^^ (x => T.Num(x |> int_of_string));

let literal = (str <|> num) ^^^ (x => T.Literal(x));

let mod_ = cap ^^^ (x => T.Module(x));

let modFn = (mod_ <* chr('.') <*> idf) ^^^ ((T.Module(mName),fName)) => {
  ctx.modules |> Context.lookup(mName, fName)
};

let pop = (chr('_') <* notPred(regex([%bs.re "/[a-zA-Z_0-9]/"]))) ^^^ (_x => T.Pop);


/* Crude hack to solve mutual recursion */
let inv__ : unit => (BsLittleParser.Input.t => BsLittleParser.ParseResult.t(T.term))
  = [%raw {| function() { return inv } |}];
let inv_ = Lazy.from_fun(inv__);

let term = literal <|> pop <|>| inv_;

let args = (term <*> rep(chr(',') *> term))
       ^^^ ((first, rest)) => [first, ...rest];

let argList = (chr('(') *> args <* chr(')'));

let inv = (modFn <*> argList) ^^^ ( ((fn, args)) => T.Inv(fn, args) );



let result = input |> rep(term);

result
|> BsLittleParser.ParseResult.getResult
|> inspect
|> Js.log(_);
