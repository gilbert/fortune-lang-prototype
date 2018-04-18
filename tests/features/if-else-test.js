var o = require("ospec")
var lib = require('../lib')

o.spec('@if statements', function(){
  o('if-then-else', function(){
    var { type, result } = lib.getType(`
      Num.eq(10,20)
      @if(_)
      :then ["equal"]
      :else ["not equal"]
    `)
    o(type).equals('success')
    o(result).deepEquals(['Str', [5, 9]])
  })

  o('if-then-else empty', function(){
    var { type, result } = lib.getType(`
      Num.eq(10,20)
      @if(_)
      :then []
      :else []
    `)
    o(type).equals('success')
    o(result).deepEquals(['void'])
  })

  o('if-then-else range checking', function(){
    var { type, errMsg } = lib.getType(`
      Num.eq(10,20)
      @if(_)
      :then [0]
      :else [4]
      Arr.get(@Arr(99), _)
    `)
    o(type).equals('error')
    o(errMsg.indexOf('max out of bounds') >= 0).equals(true)
  })

  o('if-then-else extraneous stack', function() {})
})
