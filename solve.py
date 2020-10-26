import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import fsolve
import math
# Define the expression whose roots we want to find


R = 0.5

func = lambda T : R - (np.exp(-0.5*np.array(T))*(1+np.array(T)*0.5+ np.power(T,2)*0.25/2 + np.power(T,3)*0.125/6 + np.power(T,4)*0.625/24)) 

# Plot it

tau = np.linspace(0, 100, 500)
tau = list(tau)
plt.plot(tau, func(tau))
plt.xlabel("tau")
plt.ylabel("expression value")
plt.grid()
plt.show()

# Use the numerical solver to find the roots

tau_initial_guess = 16.7
tau_solution = fsolve(func, tau_initial_guess)

print("The solution is tau =",tau_solution)
print("at which the value of the expression is", func(tau_solution))