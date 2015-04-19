import Evaluate
prog1 = Evaluate.Test.Program([Evaluate.Test.Print(Evaluate.Test.Term(Evaluate.Test.Num(1)))])
eval1 = Evaluate.Evaluate.execute(prog1)

prog2 =Evaluate.Test.Program([
                    Evaluate.Test.Print(Evaluate.Test.Term(Evaluate.Test.Minus(Evaluate.Test.Num(32),Evaluate.Test.Num(10)))),
                    Evaluate.Test.If(Evaluate.Test.Formula(Evaluate.Test.Vero()),Evaluate.Test.Print(Evaluate.Test.Term(Evaluate.Test.Num(99))))
                    ]);

eval2 = Evaluate.Evaluate.execute(prog2)

prog3 = Evaluate.Test.Program([
                    Evaluate.Test.Assign(Evaluate.Test.Var("x"),Evaluate.Test.Term(Evaluate.Test.Mult(Evaluate.Test.Num(11),Evaluate.Test.Num(34)))),
                    Evaluate.Test.Print(Evaluate.Test.Term(Evaluate.Test.Var("x")))
                    ])
eval3 = Evaluate.Evaluate.execute(prog3)

print("evaluation print 1")
print(eval1)
print("evaluation print 32 - 10, if true print 99")
print(eval2)
print("evaluation assign x = 11 * 34, print x")
print(eval3)