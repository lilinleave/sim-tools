import nlopt
from numpy import *

def myfunc(x, grad):
    if grad.size > 0:
        grad[0] = 0.0
        grad[1] = 0.5 / sqrt(x[1])
    return sqrt(x[1])

def myconstraint(x, grad):
    if grad.size > 0:
        grad[0] = 2.0*(x[0]-1.0)
        grad[1] = -1.0
    return (x[0]-1.0)**2 - x[1] + 1.0

opt = nlopt.opt(nlopt.LD_SLSQP, 2)
opt.set_lower_bounds([-float('inf'), 0])
opt.set_min_objective(myfunc)
opt.add_equality_constraint(myconstraint, 1e-8)
opt.set_xtol_rel(1e-4)
x = opt.optimize([1.234, 1.234])
minf = opt.last_optimum_value()
print "optimum at ", x[0],x[1]
print "minimum value = ", minf
print "result code = ", opt.last_optimize_result()
