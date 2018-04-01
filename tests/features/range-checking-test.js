var o = require("ospec")
var lib = require('../lib')

o.spec('[Ranges] Literals', function(){
  o('strings', function(){
    var { type, result } = lib.getType(`
      "abc"
    `, [])
    o(type).equals('success')
    o(result).deepEquals(['Str', [3,3]])
  })

  o('numbers', function(){
    var { type, result } = lib.getType(`
      8
    `, [])
    o(type).equals('success')
    o(result).deepEquals(['Num', [8,8]])
  })

  o('arrays', function(){
    var { type, result } = lib.getType(`
      @Arr(10, 20, 15)
    `, [])
    o(type).equals('success')
    o(result).deepEquals(['Arr', ['Num', [10, 20]], [3,3]])
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

o.spec('[Ranges] Type Specs', function(){
  o('numbers', function(){
    var { type, result } = lib.getType(`
      "150"
      Str.toNum(_, Num.spec(50,250))
    `, [])
    o(type).equals('success')
    o(result).deepEquals(['TypeCon', 'Maybe', [['Num', [50,250]]]])
  })
})
