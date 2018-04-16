module Parser = BsLittleParser.MakeParser(Context);
open Parser;

exception ParseError(string);

type macroArg = (T.term, option(T.term));
type macro =
  /* name, args, block, macroArgs */
  | Macro(string, list(T.term), option(T.term), list(macroArg));

let inspect = [%raw {|
  function(x) {
    console.log( require('util').inspect(x, { depth: 10 }) )
  }
|}];

/* Crude hack to solve mutual recursion */
let inv_ : unit => (Input.t => ParseResult.t(T.term))
  = [%raw {| function() { return inv } |}];
let block_ : unit => (Input.t => ParseResult.t(T.term))
  = [%raw {| function() { return block } |}];
let m_if_ : unit => (Input.t => ParseResult.t(T.term))
  = [%raw {| function() { return m_if } |}];
let arrLit_ : unit => (Input.t => ParseResult.t(T.literal))
  = [%raw {| function() { return arrLit } |}];


let cap = regex([%bs.re "/[A-Z][a-zA-Z_0-9]*/"]);
let idf = regex([%bs.re "/[a-z][a-zA-Z_0-9]*[?!]?/"]);
let idfTerm = idf ^^^ (x => T.Identifier(x));

let str = regex([%bs.re "/\"[^\"]*\"/"]) ^^^ (x => String.sub(x,1,String.length(x)-2));
let strLit = str ^^^ (x => T.StrLit(x));
let numLit = regex([%bs.re "/[0-9]+/"]) ^^^ (x => T.NumLit(x |> int_of_string));

let mword = (word) => chr('@') *> Parser.str(word);

let literal = (strLit <|> numLit <|>| arrLit_) ^^^ (x => T.Literal(x));

let modFn = (cap <* chr('.') <*> idf) ^^> (ctx, (mName,fName)) => {
  (ctx, Context.lookup(ctx, mName, fName))
};

let branchFn = mword("branch") *> (cap <* chr('.') <*> idf) ^^> (ctx, (mName,fName)) => {
  (ctx, Context.lookupBranchFn(ctx, mName, fName))
};

let branchPath = mword("branch") *> str ^^> (ctx, path) => {
  (Context.pushBranchPath(ctx, path), T.BranchInv(T.BranchPath(path), []))
};

let kw = (name) => name <* notPred(regex([%bs.re "/[a-zA-Z_0-9]/"]));

let pop = kw(chr('_')) ^^^ ((_) => T.Pop);
let top = kw(Parser.str("top")) ^^^ ((_) => T.Peek);

let term = literal <|> pop <|> top <|> branchPath <|>| inv_ <|>| m_if_;

let arg = term <|>| block_;

let args = (arg <*> rep(chr(',') *> arg))
       ^^^ ((first, rest)) => [first, ...rest];

let argList = (chr('(') *> args <* chr(')'));

let arrLit = (Parser.str("@Arr") *> argList) ^^^ (terms => T.ArrLit(terms));

let inv = ((branchFn <*> argList) ^^^ (((fn, args)) => T.BranchInv(fn, args)))
<|> ((modFn <*> argList) ^^^ (((fn, args)) => T.Inv(fn, args)));

let seq = rep(term);

let block = (chr('[') *> seq <* chr(']')) ^^^ (terms => T.BlockTerm(terms));

let macroArg = chr(':') *> (idfTerm <*> opt(block)) ^^^ (((name, arg)) => (name, arg));

let macro = (name) => (
  mword(name) *>
  opt(argList) <*>
  opt(block) <*>
  rep(macroArg)
) ^^^ ( (((args, block), margs)) => switch (args) {
  | Some(args) => Macro(name, args, block, margs)
  | None => Macro(name, [], block, margs)
});


let m_if = macro("if") ^^^ ( (Macro(_, args, block, margs)) => {
  if (args |> List.length != 1) {
    raise(ParseError("@if requires one argument (the conditional)"))
  };
  switch(block) {
    | Some(thenArg) =>
      switch(margs) {
        | [(T.Identifier("else"), Some(elseArg))] => T.IfElse(List.hd(args), thenArg, elseArg)
        | [_, ..._] => raise(ParseError("@if only accepts an :else argument, e.g. @if(a) [b] :else [c]"))
        | _ => T.If(List.hd(args), thenArg)
      }
    | None =>
      switch(margs) {
        | [(T.Identifier("then"), Some(thenArg)), (T.Identifier("else"), Some(elseArg))] =>
          T.IfElse(List.hd(args), thenArg, elseArg)
        | _ => raise(ParseError("@if requires a block or :then argument"
                                 ++ "\n  e.g. @if(a) [b] :else c"
                                 ++ "\n  or   @if(a) :then [b] :else [c]"))
      }
  }
});


let program = seq ^^> ((ctx, terms) => {
  let seq = T.Seq(terms);
  Js.log("Got");
  seq |> Ops.compileAst |> Js.log;
  let (ctx2, ty) = Apply.getType(ctx, seq);
  (ctx2, (seq, ty))
});


let parse = (stack, source) => {
  let input = Input.{
    text: source,
    index: 0,
    whitespace: " \n",
    context: Run.stdlib |> Context.push(_, stack)
  };

  switch (input |> program |> ParseResult.getResult) {
    | Ok((ctx, (seq, ty))) =>
      let branchPaths = ctx.branchPaths |> List.map(Js.Json.string) |> Array.of_list |> Js.Json.array;
      ("success", [|branchPaths|], [|seq|], [|ty|], "")
    | Err(SyntaxErr(_line, _col, msg)) =>
      ("error", [||], [||], [||], msg)
    | exception T.TypeError(msg) =>
      ("error", [||], [||], [||], msg)
  }
};

/*let result = Node.Fs.readFileSync("./lab/branch.fortune", `utf8)
  |> parse([||]);

switch (result) {
| Some((_ctx, (seq, ty))) =>
    Term.print(seq) |> Js.log;
    T.print(ty) |> Js.log;
    Js.log("\n----====----\n");
    seq |> Ops.compileAst |> inspect
| None => Js.log("Nothin")
};
*/
