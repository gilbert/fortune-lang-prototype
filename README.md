# Fortune

Fortune is a language designed to run untrusted code in a safe and intelligent manner. It is:

- **Not** Turing Complete by design, solving the halting problem
- Statically typed, so users cannot upload code that crashes
- Resource-aware, so providers can strategically limit the amount of computing power a program has access to.

Fortune is currently in its prototype stage.

## Runtime

Fortune runs *on top of* a programming language environment. It currently supports Node.js.

```js
const Fortune = require('fortune')
const source = `
  "hello world"
  Str.split(" ")
  Arr.map [Str.capitalize]
  Str.join(" ").add("!")
`
const program = Fortune.compile(source)
Fortune.run(program) //=> ["Hello World!"]
```

After guaranteeing the code is type safe, compiling will return instruction code (as JSON) that you can run immediately or save in the database to run later.

### IO

By default, Fortune code does not have access to the outside. However, you can inject controlled modules for your untrusted code to invoke as it sees fit:

```js
const untrustedSource = `
  _.params.username

  @match
    :"alice" ["Welcome, Alice!"]
    :"bob" ["Welcome, Bob!"]
    : [
      IO.log("Unknown user: ", _)
      "Who are you?"
    ]

  @branch Server.r200({ message: _ })
`
const modules = {
  IO: {
    log: {
      type: `(...any) => Unit`,
      run(...args) { console.log(...args) }
    }
  },
  Server: {
    r200: {
      type: `@branch({ message: any })`,
      run(body) {
        this.res.status(200).send(body)
      }
    }
  }
}
const program = Fortune.compile(untrustedSource, {
  modules: modules,
  branch: modules.Server
})

const args = { username: "alice" }
Fortune.run(program, args, { res: ... })
```

Several interesting features are being used in the above example:

- By passing `modules:` into `Fortune.compile`, the untrusted code gains access to those functions
  - This allows you to control exactly how the code interacts with your system
- By passing `branch:` also, Fortune validates that the source code to finalize
  - A `@branch` halts Fortune execution, passing control to the function's module
  - In this case, branches help ensure the untrusted code can only send back one HTTP response.
- By passing a context to `Fortune.run`, our module functions gain access to it via `this`.

## Syntax Overview

Fortune is a stack-based [concatenative language](http://evincarofautumn.blogspot.com/2012/02/why-concatenative-programming-matters.html). However, its syntax is unique from other concatenative languages (like [Forth](https://www.forth.com/forth/) and [Kitten](http://kittenlang.org/)) in one significant way:

- Arguments are not required to be written before a function call.

As a result, Fortune reads more fluently and is less centered around manipulating the runtime stack.

### Primitives

Fortune supports JSON primitives:

```
"a string"
null
true
10100
@Arr("an", "array")
@List("a", "list")
{ an: "object", with: "multiple", "props": 99 }
```

### Function Calls

All available functions exist in a module. Here are some of the built-in ones:

```js
Str.split(" ", "hello there") //=> Arr("hello", "there")

// _ means "pop from the stack".
// This example puts "hello" on the stack
// and then feeds it to Str.split
"hello"
Str.split(" ", _) //=> Arr("hello", "there")

// Some functions implicitly pop from the stack for convenience,
// and Str.split is one of them. You can write the above like so:
"hello"
Str.split(" ") //=> Arr("hello", "there")

// If a function only takes one argument,
// it can always implicitly pop from the stack.
// In this cane you can leave out parenthesis.
"alice"
Str.capitalize //=> "Alice"
```

### Function Calls: Blocks

Some functions require a block of code to run. Blocks are written with square brackets `[]`

```js
"hello there"
Str.split(" ") //=> Arr("hello", "there")
Arr.map(_) [Str.capitalize] //=> Arr("Hello", "There")

// Arr.map can implicitly pop its subject off the stack
"hello there"
Str.split(" ")
Arr.map [Str.capitalize]
```

## Developing

Hello! This project was generated to quickly get started with Reason and BuckleScript.

# Build
```
npm run build
```

# Build + Watch

```
npm run start
```


# Editor
If you use `vscode`, Press `Windows + Shift + B` it will build automatically
