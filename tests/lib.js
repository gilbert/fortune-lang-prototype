var parse = require('../src/parse.bs').parse
var compile = require('../src/compiler/ops.bs').compileAst
var run = require('../src/runtimes/node').run

exports.compileAndRun = (source, typeStack=[], runtimeStack=[], opts={}) => {
  var [_, [ast], [returnType]] = parse(typeStack, source)

  if ( ! ast ) {
    throw new Error("AST failed to parse")
  }

  var ops = compile(ast)
  var program = {
    context: require('../src/runtimes/node').stdlib,
    ops: ops,
  }
  // console.log("Running program", require('util').inspect(program, { depth: 10 }))
  return run(program, runtimeStack)
}
