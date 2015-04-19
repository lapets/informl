E = require('./Evaluate.js');
var prog1 = Test.Program([
                    Test.Print(Test.Term(Test.Num(1)))
                    ]);

var eval1 = E.execute(prog1);

var prog2 = Test.Program([
                    Test.Print(Test.Term(Test.Minus(Test.Num(32),Test.Num(10)))),
                    Test.If(Test.Formula(Test.Vero()),Test.Print(Test.Term(Test.Num(99))))
                    ]);
var eval2 = E.execute(prog2);

var prog3 = Test.Program([
                    Test.Assign(Test.Var("x"),Test.Term(Test.Mult(Test.Num(11),Test.Num(34)))),
                    Test.Print(Test.Term(Test.Var("x")))
                    ]);
var eval3 = E.execute(prog3);

console.log('print 1')
console.log(eval1);
console.log('print 32 - 10, if true print 99')
console.log(eval2);
console.log('Assign x = 11 * 34, print x')
console.log(eval3);