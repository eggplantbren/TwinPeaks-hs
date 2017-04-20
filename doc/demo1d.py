import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng
from scipy.stats import norm as norm

# 1D example
L = np.linspace(-10.0, 10.0, 20001)
dL = L[1] - L[0]

# Prior
pi = np.exp(-0.5*np.abs(L + 0.1)**2)
pi /= np.trapz(pi, x=L)

# NS distribution
X = 1.0 - np.cumsum(pi)*dL
print(X[0])
pNS = pi/(X + 1E-300)
pNS /= np.trapz(pNS, x=L)

# Return a constrained prior
# and its X-value.
def constrained_prior(ell):
    p = pi.copy()
    p[L <= ell] = 0.0
    X = np.trapz(p, x=L)
    p /= X
    return {"p": p, "X": X}

# KL divergence
def KL(p, q):
    integrand = p*np.log(p/(q + 1E-300) + 1E-300)
    return np.trapz(integrand, x=L)

l1, l2 = 1.10, 1.15
cp1 = constrained_prior(l1)
cp2 = constrained_prior(l2)
dl = l2 - l1
KL1 = KL(cp1["p"], pNS)
KL2 = KL(cp2["p"], pNS)
print("X1 = {X}".format(X=cp1["X"]))
print("X2 = {X}".format(X=cp2["X"]))
print("")
print("KL1 = {kl}".format(kl=KL1))
print("KL2 = {kl}".format(kl=KL2))

print("d(KL)/dl = {deriv}".format(deriv=(KL2 - KL1)/dl))

plt.plot(L, pi)
plt.plot(L, cp1["p"])
plt.plot(L, cp2["p"])
plt.plot(L, pNS)
plt.show()

