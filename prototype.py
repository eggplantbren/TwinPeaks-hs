import numpy as np
import numpy.random as rng
import matplotlib.pyplot as plt
import scipy.stats

def from_prior(N):
    nx = rng.randn(N)
    ny = -0.7*nx + np.sqrt(1.0 - 0.7**2)*rng.randn(N)
    ux = scipy.stats.norm.cdf(nx)
    uy = scipy.stats.norm.cdf(ny)
    return [ux, uy]

# Initial particles
ux, uy = from_prior(1000)

# Context particles

plt.figure(figsize=(6, 6))
plt.plot(ux, uy, "k.", alpha=0.2)
plt.show()

