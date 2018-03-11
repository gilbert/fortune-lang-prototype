/*
  An excellent library taken from https://github.com/Henoc/bs-little-parser
  and modified to allow context.
*/

module type ContextI = {
  type t;
};

module MakeParser = (C: ContextI) => {

  module Input = {
    type t = {
      text: string,
      index: int,
      whitespace: string,
      context: C.t
    };
  };

  module ParseResult = {
    type t('a) =
      | ParseSuccess('a, Input.t)
      | ParseFailure(string, Input.t);
    let getResult = parseResult =>
      switch (parseResult) {
      | [@implicit_arity] ParseSuccess(p, q) => Some((q.context, p))
      | [@implicit_arity] ParseFailure(_, _) => None
      };
    let getIndex = parseResult =>
      switch (parseResult) {
      | [@implicit_arity] ParseSuccess(_, i) => i.index
      | [@implicit_arity] ParseFailure(_, i) => i.index
      };
    let map = (f, parseResult) =>
      switch (parseResult) {
      | [@implicit_arity] ParseSuccess(p, q) => {
        let (ctx2, a) = f(q.context, p);
        [@implicit_arity] ParseSuccess(a, { ...q, context: ctx2 })
        }
      | [@implicit_arity] ParseFailure(x, y) =>
        [@implicit_arity] ParseFailure(x, y)
      };
    let mapNoContext = (f, parseResult) =>
      switch (parseResult) {
      | [@implicit_arity] ParseSuccess(p, q) => {
        [@implicit_arity] ParseSuccess(f(p), q)
        }
      | [@implicit_arity] ParseFailure(x, y) =>
        [@implicit_arity] ParseFailure(x, y)
      };
  };

  type t('a) = Input.t => ParseResult.t('a);
  let parse = (input: Input.t, parser: t('a)) => parser(input);
  let andThen = (p, q, input) =>
    switch (p(input)) {
    | [@implicit_arity] ParseResult.ParseSuccess(result1, input2) =>
      switch (q(input2)) {
      | [@implicit_arity] ParseResult.ParseSuccess(result2, input3) =>
        [@implicit_arity]
        ParseResult.ParseSuccess((result1, result2), input3)
      | [@implicit_arity] ParseResult.ParseFailure(message, input) =>
        [@implicit_arity] ParseResult.ParseFailure(message, input)
      }
    | [@implicit_arity] ParseResult.ParseFailure(message, input) =>
      [@implicit_arity] ParseResult.ParseFailure(message, input)
    };
  let (<*>) = (p, q) => andThen(p, q);
  let onlyLeft = (p, q, input) =>
    switch (p(input)) {
    | [@implicit_arity] ParseResult.ParseSuccess(result1, input2) =>
      switch (q(input2)) {
      | [@implicit_arity] ParseResult.ParseSuccess(_, input3) =>
        [@implicit_arity] ParseResult.ParseSuccess(result1, input3)
      | [@implicit_arity] ParseResult.ParseFailure(message, input) =>
        [@implicit_arity] ParseResult.ParseFailure(message, input)
      }
    | [@implicit_arity] ParseResult.ParseFailure(message, input) =>
      [@implicit_arity] ParseResult.ParseFailure(message, input)
    };
  let ( <* ) = (p, q) => onlyLeft(p, q);
  let onlyRight = (p, q, input) =>
    switch (p(input)) {
    | [@implicit_arity] ParseResult.ParseSuccess(_, input2) =>
      switch (q(input2)) {
      | [@implicit_arity] ParseResult.ParseSuccess(result2, input3) =>
        [@implicit_arity] ParseResult.ParseSuccess(result2, input3)
      | [@implicit_arity] ParseResult.ParseFailure(message, input) =>
        [@implicit_arity] ParseResult.ParseFailure(message, input)
      }
    | [@implicit_arity] ParseResult.ParseFailure(message, input) =>
      [@implicit_arity] ParseResult.ParseFailure(message, input)
    };
  let ( *> ) = (p, q) => onlyRight(p, q);
  let orElse = (p, q, input) =>
    switch (p(input)) {
    | [@implicit_arity] ParseResult.ParseSuccess(s, t) =>
      [@implicit_arity] ParseResult.ParseSuccess(s, t)
    | ParseResult.ParseFailure(_) => q(input)
    };
  let (<|>) = (p, q) => orElse(p, q);
  let rep = (p, input) => {
    let rec loop = (acc, input) =>
      switch (p(input)) {
      | [@implicit_arity] ParseResult.ParseSuccess(r, i) =>
        loop([r, ...acc], i)
      | ParseResult.ParseFailure(_) => (List.rev(acc), input)
      };
    let (r, i) = loop([], input);
    [@implicit_arity] ParseResult.ParseSuccess(r, i);
  };
  let rep1 = p => andThen(p) @@ rep(p);
  let optional = (p, input) =>
    switch (p(input)) {
    | ParseResult.ParseFailure(_) =>
      [@implicit_arity] ParseResult.ParseSuccess(None, input)
    | [@implicit_arity] ParseResult.ParseSuccess(r, i) =>
      [@implicit_arity] ParseResult.ParseSuccess(Some(r), i)
    };
  let opt = p => optional(p);
  let andPred = (p, input) =>
    switch (p(input)) {
    | [@implicit_arity] ParseResult.ParseSuccess(r, _) =>
      [@implicit_arity] ParseResult.ParseSuccess(r, input)
    | others => others
    };
  let notPred = (p, input) =>
    switch (p(input)) {
    | [@implicit_arity] ParseResult.ParseSuccess(_, i) =>
      [@implicit_arity] ParseResult.ParseFailure("notPred failure", i)
    | ParseResult.ParseFailure(_) =>
      [@implicit_arity] ParseResult.ParseSuccess(None, input)
    };
  let into = (p, fnq, input) =>
    switch (p(input)) {
    | [@implicit_arity] ParseResult.ParseSuccess(r, i) => (fnq(r))(i)
    | others => others
    };
  let (>>) = (p, fnq) => into(p, fnq);

  let map = (p, fn, input) => ParseResult.map(fn, p(input));
  let (^^>) = (p, fn) => map(p, fn);

  let mapNoContext = (p, fn, input) => ParseResult.mapNoContext(fn, p(input));
  let (^^^) = (p, fn) => mapNoContext(p, fn);

  /** internal use only */
  let skipWhitespace = (whitepspace: string, input: Input.t) => {
    let rec listChar = str =>
      switch (str) {
      | "" => []
      | str => [
          str.[0],
          ...listChar(String.sub(str, 1, String.length(str) - 1)),
        ]
      };
    let spaceChars = listChar(whitepspace);
    let rec contain = (chr, charList) =>
      switch (charList) {
      | [] => false
      | [c, ...tl] =>
        if (chr === c) {
          true;
        } else {
          contain(chr, tl);
        }
      };
    let rec loop = (input: Input.t) =>
      if (String.length(input.text) <= input.index) {
        input;
      } else if (contain(input.text.[input.index], spaceChars)) {
        loop({...input, index: input.index + 1});
      } else {
        input;
      };
    loop(input);
  };
  let charParser = (c, rawInput: Input.t) => {
    let input = skipWhitespace(rawInput.whitespace, rawInput);
    String.length(input.text) <= input.index ?
      [@implicit_arity] ParseResult.ParseFailure("no more length", input) :
      {
        let firstChar = input.text.[input.index];
        firstChar === c ?
          [@implicit_arity]
          ParseResult.ParseSuccess(c, {...input, index: input.index + 1}) :
          [@implicit_arity]
          ParseResult.ParseFailure(
            "different char '"
            ++ Char.escaped(firstChar)
            ++ "' found, expected: '"
            ++ Char.escaped(c)
            ++ "'",
            input,
          );
      };
  };
  let chr = c => charParser(c);
  let stringParser = (s, rawInput: Input.t) => {
    let input = skipWhitespace(rawInput.whitespace, rawInput);
    String.length(input.text) - input.index < String.length(s) ?
      [@implicit_arity] ParseResult.ParseFailure("no more length", input) :
      {
        let substr = String.sub(input.text, input.index, String.length(s));
        substr === s ?
          [@implicit_arity]
          ParseResult.ParseSuccess(
            s,
            {...input, index: input.index + String.length(s)},
          ) :
          [@implicit_arity]
          ParseResult.ParseFailure(
            "remined text doesn't start with " ++ s,
            input,
          );
      };
  };
  let str = s => stringParser(s);
  let regexParser = (r: Js.Re.t, rawInput: Input.t) => {
    let input = skipWhitespace(rawInput.whitespace, rawInput);
    let substr = Js.String.substr(~from=input.index, input.text);
    switch (Js.Re.exec(substr, r)) {
    | None =>
      [@implicit_arity]
      ParseResult.ParseFailure("remind text doesn't start with regex", input)
    | Some(result) =>
      if (Js.Re.index(result) !== 0) {
        [@implicit_arity]
        ParseResult.ParseFailure(
          "remind text doesn't start with regex",
          input,
        );
      } else {
        switch (Js.Nullable.toOption(Js.Re.captures(result)[0])) {
        | None =>
          [@implicit_arity]
          ParseResult.ParseFailure(
            "remind text doesn't start with regex",
            input,
          )
        | Some(matchedString) =>
          [@implicit_arity]
          ParseResult.ParseSuccess(
            matchedString,
            {...input, index: input.index + String.length(matchedString)},
          )
        };
      }
    };
  };
  let regex = r => regexParser(r);

};
