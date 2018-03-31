var o = require("ospec")
var lib = require('../lib')

o.spec('[Ranges] Literals', function(){
  o('string', function(){
    var { type, result } = lib.getType(`
      "abc"
    `, [])
    o(type).equals('success')
    o(result).deepEquals(['Str', [3,3]])
  })

  o('array', function(){
    var { type, result } = lib.getType(`
      @Arr(10, 20)
    `, [])
    o(type).equals('success')
    o(result).deepEquals(['Arr', ['Num'], [2,2]])
  })
})

o.spec('[Ranges] Calculation', function(){
  o('arrays', function(){
    var { type, result } = lib.getType(`
      "a b c"
      Str.split(_, " ")
    `, [])
    o(type).equals('success')
    o(result).deepEquals(['Arr', ['Str', [0,5]], [1,6]])
  })
})
