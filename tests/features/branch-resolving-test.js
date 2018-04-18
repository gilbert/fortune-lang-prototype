var o = require("ospec")
var lib = require('../lib')

o.spec('Branch Resolving', function(){
  o('simple', function(){
    var { type, branchPaths } = lib.getType(`
      "abc"
      @branch "other/node"
    `, {
      availableBranchPaths: ['one']
    })
    o(type).equals('success')
    o(branchPaths).deepEquals(['other/node'])
  })

  o('branch detection', function() {
    var { type, result, branchPaths } = lib.getType(`
      Num.eq(10,20)
      @if(_)
      :then [@branch "one"]
      :else [@branch "two"]
    `, {
      availableBranchPaths: ['one', 'two', 'three']
    })
    o(type).equals('success')
    o(branchPaths.sort()).deepEquals(['one', 'two'])
  })
})
