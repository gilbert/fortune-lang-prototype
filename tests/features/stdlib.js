var o = require("ospec")
var lib = require('../lib')

o.spec('String', function(){
  o('split', function(){
    var result = lib.compileAndRun(`
      "hello there"
      Str.split(" ", _)
    `)
    o(result).deepEquals(["hello", "there"])
  })

  o('upcase', function(){
    var result = lib.compileAndRun(`"hi" Str.upcase(_)`)
    o(result).deepEquals('HI')
  })

  o('cap', function(){
    var result = lib.compileAndRun(`"hi" Str.cap(_)`)
    o(result).deepEquals('Hi')
  })

  o('toNum', function(){
    var result = lib.compileAndRun(`"10" Str.toNum(_)`)
    o(result).deepEquals(['Yes', 10])
  })
})


o.spec('Num', function(){
  o('add', function(){
    var result = lib.compileAndRun(`10 Num.add(15,_)`)
    o(result).deepEquals(25)
  })
})


o.spec('Arr', function(){
  o('get', function(){
    var result = lib.compileAndRun(`@{10,20} Arr.get(_,1)`)
    o(result).deepEquals(20)
  })
  o('map', function(){
    var result = lib.compileAndRun(`@{10,20} Arr.map(_,[Num.add(1,_)])`)
    o(result).deepEquals([11,21])
  })
})


o.spec('branches.Program', function(){
  o('exit', function(){
    var result = lib.compileAndRun(`
      10
      @branch Program.exit("adios", 20)
      30
    `)
    o(result).deepEquals(20)
  })
})


o.spec('Maybe', function(){
  o('unwrap!', function(){
    var result = lib.compileAndRun(`
      Str.toNum("5")
      Maybe.unwrap!(_, [
        @branch Program.exit("didn't work", 1)
      ])
    `)
    o(result).deepEquals(5)

    var result = lib.compileAndRun(`
      Str.toNum("not a num")
      Maybe.unwrap!(_, [
        @branch Program.exit("not a num", 1)
      ])
      999
    `)
    o(result).deepEquals(1)
  })
})


o.spec('IO', function(){
  o('log', function(){
    var result = lib.compileAndRun(`IO.log("5", 10, @{15})`)
    o(result).deepEquals(undefined)
  })
})
