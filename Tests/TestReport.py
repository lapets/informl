import richreports as R
import ReportExample

RE = ReportExample.ReportExample
L = ReportExample.La

prog = L.Program([L.Repeat([L.Prints(L.OpPlus(L.Num(2), L.Num(3))), L.Pass(), L.Errors(), L.Repeat([L.Prints(L.Num(7)), L.Pass()])]), L.Prints(L.OpMax(L.OpAbs(L.Num(4)), L.Num(5))), L.Pass()])

ux = RE.report(prog)

ux2 = R.html(ux)

print(prog)
print(ux2)

open('report.html', 'w').write(ux2)