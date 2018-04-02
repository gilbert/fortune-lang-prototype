var parse = require('../src/parse.bs').parse
var Ops = require('../src/compiler/ops.bs')
var Node = require('../src/runtimes/node')
var TypeAnn = require('../src/TypeAnn.bs')

exports.compileAndRun = (source, typeStack=[], runtimeStack=[], opts={}) => {
  var [result, [ast], [returnType], errMsg] = parse(typeStack, source)

  if ( result === 'error' ) {
    throw new Error(errMsg)
  }

  if ( ! ast ) {
    throw new Error("AST failed to parse")
  }

  var ops = Ops.compileAst(ast)
  var program = {
    context: require('../src/runtimes/node').stdlib,
    ops: ops,
  }
  // console.log("Running program", require('util').inspect(program, { depth: 10 }))
  return Node.run(program, runtimeStack)
}

exports.getType = (source, typeStack, opts={}) => {
  var [type, [ast], [returnType], errMsg] = parse(typeStack, source)
  if ( type === 'success' ) {
    return { type: type, result: Ops.compileType(returnType) }
  }
  else {
    throw new Error(errMsg)
  }
}

exports.compileType = (typeAnn) => {
  var ty = TypeAnn.compile(typeAnn)
  return Ops.compileType(ty)
}
