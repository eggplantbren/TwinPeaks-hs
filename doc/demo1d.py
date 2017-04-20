import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng
from scipy.stats import norm as norm

# 1D example
L = np.linspace(-10.0, 10.0, 1000000)
dL = L[1] - L[0]

# Prior
pi = np.exp(-0.5*(L/3)**2)
pi /= np.sum(pi)*dL

# NS distribution
X = 1.0 - (np.cumsum(pi) - pi)*dL
pNS = pi/X
C = np.sum(pNS)*dL
pNS /= C

# Return a constrained prior
# and its X-value.
def constrained_prior(ell):
    p = pi.copy()
    p[L < ell] = 0.0
    X = np.sum(p)*dL
    p /= X
    return {"p": p, "X": X}

# KL divergence
def KL(p, q):
    integrand = p*np.log((p + 1E-300)/q)
    return np.sum(integrand)*dL

print(KL(constrained_prior(0.5)["p"], pNS))
print(KL(constrained_prior(0.6)["p"], pNS))
print(KL(constrained_prior(0.7)["p"], pNS))

plt.plot(L, pi)
plt.plot(L, X)
plt.show()

