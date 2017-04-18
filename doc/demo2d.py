import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng

# 2D example
L1 = np.linspace(-7.0, 7.0, 1001)
L2 = np.linspace(-7.0, 7.0, 1001)
[L1, L2] = np.meshgrid(L1, L2[::-1])

# Prior
pi = np.exp(-0.5*L1**2 - 0.5*(L2 - L1)**2/0.5**2)
pi /= np.sum(pi)

# Image
plt.imshow(pi, extent=[L1.min(), L1.max(), L2.min(), L2.max()],
           cmap="Oranges")
plt.xlabel(r"$L_1$")
plt.ylabel(r"$L_2$")
plt.show()

# X12
X12 = np.zeros(pi.shape)
for i in range(X12.shape[0]):
    for j in range(X12.shape[1]):
        X12[i, j] = np.sum(pi[(L1 > L1[i, j]) & (L2 > L2[i, j])])
    print(i+1)

plt.imshow(X12, extent=[L1.min(), L1.max(), L2.min(), L2.max()],
           cmap="Oranges")
plt.xlabel(r"$L_1$")
plt.ylabel(r"$L_2$")
plt.show()

# Twinpeaks distribution
pNS = np.zeros(pi.shape)
pNS[X12 > 0.0] = pi[X12 > 0.0] / X12[X12 > 0.0]
pNS /= pNS.sum()

def do_threshold(thresh1, thresh2):
    p = pi*((L1 > thresh1) & (L2 > thresh2))
    X = np.sum(p)
    p /= X
    KL = np.sum(p*np.log(p/(pNS + 1E-300) + 1E-300))
    print("KL = {KL}".format(KL=KL))

# Some thresholds
do_threshold(0.3, -0.7)
do_threshold(-0.3, -2.0)
do_threshold(1.1, 0.4)



## Enclosed mass
#X = 1.0 - dL*np.cumsum(pi)

## NS distribution
#pNS = np.zeros(len(L))
#pNS[X > 0.0] = pi[X > 0.0]/X[X > 0.0] # Avoid division by zero
#C = np.trapz(pNS, x=L)
#pNS /= C

## Plot the prior and the NS distribution
#plt.plot(L, pi,  label=r"$\pi$")
#plt.plot(L, pNS, label=r"$p_{\rm NS}$")
#plt.legend()
#plt.xlabel(r"$L$")
#plt.ylabel("Probability Density")
#plt.show()

## Some thresholds to try
#thresholds = [-0.4, 0.7, 2.2]
#for thresh in thresholds:
#    # Constrained prior
#    constrained_prior = pi.copy()
#    constrained_prior[L <= thresh] = 0.0
#    X = np.trapz(constrained_prior, x=L)
#    constrained_prior /= X
#    KL = np.trapz(constrained_prior*\
#                  np.log(constrained_prior/(pNS + 1E-300) + 1E-300),
#                  x=L)
#    print(thresh, X, KL)
#    plt.plot(L, constrained_prior)

#plt.xlabel(r"$L$")
#plt.ylabel("Probability Density")
#plt.show()

