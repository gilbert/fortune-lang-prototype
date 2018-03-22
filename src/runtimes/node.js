//
// All runtimes must implement the standard library
//
var Maybe = {
  Yes: (x) => ['Yes', x],
  No: ['No'],
  'unwrap!': {
    run(m, onError) { return m[0] === 'Yes' ? m[1] : onError() }
  }
}

exports.stdlib = {
  modules: {
    Str: {
      split: {
        run(sep, str) { return str.split(sep) },
      },
      upcase: {
        run(str) { return str.toUpperCase() },
      },
      cap: {
        run(str) { return str[0].toUpperCase() + str.slice(1) },
      },
      toNum: {
        run(str) {
          var n = Number(str)
          return n ? Maybe.Yes(n) : Maybe.No
        },
      },
    },
    Num: {
      add: {
        run(x,y) { return x + y },
      },
    },
    Arr: {
      get: {
        run(a, index) { return a[index] },
      },
      map: {
        run(a, f) { return a.map(x => f(x)) },
      },
    },
    IO: {
      log: {
        run() {
          console.log.apply(console, arguments)
        }
      }
    },
    Maybe: Maybe,
  },

  branches: {
    Program: {
      exit: {
        run(message, code) {
          console.log('[Fortune.run] Program exit', code, message)
          return code
        }
      }
    }
  }
}

exports.run = function (program, runtimeStack) {
  // program.ops is always a 'seq'
  try {
    var stack = run(program.context, program.ops[1], runtimeStack)
    return stack[stack.length-1]
  }
  catch (err) {
    if ( err instanceof BranchExit ) {
      return err.value
    }
    else throw err
  }
}

function run (ctx, operations, stack) {
  for (var i=0; i < operations.length; i++) {
    let op = operations[i]

    if ( op[0] === 'lit' ) {
      stack.push(op[1])
    }
    else if ( op[0] === 'seq' ) {
      stack.push(val => resolveValue(ctx, op[1], val ? [val] : []))
    }
    else if ( op[0] === 'arr' ) {
      var [_, unresolvedItems] = op
      var items = resolveArray(ctx, stack, unresolvedItems)
      stack.push(items)
    }
    else if ( op[0] === 'inv' ) {
      var [_, mod, fun, unresolvedArgs] = op
      var args = resolveArray(ctx, stack, unresolvedArgs)

      var ret = ctx.modules[mod][fun].run.apply(ctx, args)
      stack.push(ret)
    }
    else if ( op[0] === 'br' ) {
      var [_, mod, fun, unresolvedArgs] = op
      var args = resolveArray(ctx, stack, unresolvedArgs)

      var ret = ctx.branches[mod][fun].run.apply(ctx, args)
      throw new BranchExit(`${mod}.${fun}`, ret)
    }
    else {
      throw new Error('[Fortune.node.run] Unknown op: ' + JSON.stringify(op[0] || null))
    }
  }
  return stack
}

function resolveValue (ctx, operations, stack) {
  run(ctx, operations, stack)
  return stack.pop()
}

function resolveArray (ctx, stack, args) {
  return args
    // Resolve pops before other arguments
    .map(a => a[0] === 'pop' ? yes(stack.pop()) : wait(a))
    // Now resolve the rest
    .map(a => isYes(a) ? a[1] : resolveValue(ctx, [a[1]], stack))
}

const yes = (v) => ['yes',v]
const wait = (v) => ['wait',v]
const isYes = (x) => x[0] === 'yes'

class BranchExit extends Error {
  constructor(message, value) {
    super(message)
    this.value = value
  }
}
