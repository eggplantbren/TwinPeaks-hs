import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng

# 1D example
L = np.linspace(-10.0, 10.0, 20001)
dL = L[1] - L[0]

# Prior
pi = np.exp(-0.5*L**2)
pi /= np.trapz(pi, x=L)

# Enclosed mass
X = 1.0 - dL*np.cumsum(pi)

# NS distribution
pNS = np.zeros(len(L))
pNS[X > 0.0] = pi[X > 0.0]/X[X > 0.0] # Avoid division by zero
pNS /= np.trapz(pNS, x=L)

# Plot the prior and the NS distribution
plt.plot(L, pi,  label=r"$\pi$")
plt.plot(L, pNS, label=r"$p_{\rm NS}$")
plt.legend()
plt.xlabel(r"$L$")
plt.ylabel("Probability Density")
plt.show()

