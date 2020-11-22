from scipy import integrate
import numpy as np
import math


def f(x,y):
    return np.exp(-0.001*x - 0.002*y)*6*(10**-6)

### part a #######

def bounds_y():
    return [0, 2000]

def bounds_x(y):
    return [y, 1000]

ans_a, _ = integrate.nquad(f, [bounds_x, bounds_y])
print(f'answer a {ans_a}')

### part b #########

def bounds_y():
    return [2000, np.inf]

def bounds_x(y):
    return [0, y]

ans_b, _ = integrate.nquad(f, [bounds_x, bounds_y])
print(f'answer b {ans_b}')

### part c #########


def bounds_y():
    return [0, np.inf]

def bounds_x(y):
    return [0, y]

ey, _ = integrate.nquad(lambda x,y: y*f(x,y), [bounds_x, bounds_y])
var_y, err = integrate.nquad(lambda x,y: ((y-ey)**2)*f(x,y), [bounds_x, bounds_y])

# print(var_y, err)

def bounds_y(x):
    return [x, np.inf]

def bounds_x():
    return [0, np.inf]

ex, _ = integrate.nquad(lambda x,y: x*f(x,y), [bounds_y, bounds_x])
var_x, err = integrate.nquad(lambda x,y: ((x-ex)**2)*f(x,y), [bounds_y, bounds_x])

# print(var_x, err)

def bounds_y():
    return [0, np.inf]

def bounds_x(y):
    return [0, y]

cov_xy, err = integrate.nquad(lambda x,y: (x-ex)*(y-ey)*f(x,y), [bounds_x, bounds_y])
# print(cov_xy, err)


# cov_xy = exy-ex*ey

# var_x = ex2 - ex**2
# var_y = ey2 - ey**2

# print(cov_xy, var_x, var_y)
print(f'answer c {cov_xy/math.sqrt(var_x*var_y)}')

### part d, e #########

def g(x):
    '''
    Marginal PDF of X
    '''
    def bounds_y():
        return [x, np.inf]

    fx, _ = integrate.nquad(lambda y: f(x,y), [bounds_y])

    return fx

X = 1500
def bounds_y():
        return [X, np.inf]

ans_d, _ = integrate.nquad(lambda y: y*f(X, y)/g(X), [bounds_y])
print(f'answer d {ans_d}')

def bounds_y():
    return [2000, np.inf]

ans_e, _ = integrate.nquad(lambda y: f(X, y)/g(X), [bounds_y])
print(f'answer e {ans_e}')