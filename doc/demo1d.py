import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng
from scipy.stats import norm as norm

# 1D example
L = np.linspace(-30.0, 30.0, 100001)
dL = L[1] - L[0]

# Prior
pi = np.exp(-np.abs(L))
pi /= np.trapz(pi, x=L)

# Enclosed mass
X = 1.0 - dL*np.cumsum(pi)

# NS distribution
pNS = np.zeros(len(L))
pNS[X > 0.0] = pi[X > 0.0]/X[X > 0.0] # Avoid division by zero
C = np.trapz(pNS, x=L)
pNS /= C

# Plot the prior and the NS distribution
plt.plot(L, pi,  label=r"$\pi$")
plt.plot(L, pNS, label=r"$p_{\rm NS}$")
plt.legend()
plt.xlabel(r"$L$")
plt.ylabel("Probability Density")
plt.show()

# Some thresholds to try
thresholds = [-0.4, 0.7, 2.2]
for thresh in thresholds:
    # Constrained prior
    constrained_prior = pi.copy()
    constrained_prior[L <= thresh] = 0.0
    X = np.trapz(constrained_prior, x=L)
    constrained_prior /= X
    KL = np.trapz(constrained_prior*\
                  np.log(constrained_prior/(pNS + 1E-300) + 1E-300),
                  x=L)
    print(thresh, X, KL)
    plt.plot(L, constrained_prior)

plt.xlabel(r"$L$")
plt.ylabel("Probability Density")
plt.show()

