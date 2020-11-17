
#!/usr/bin/env python3
import numpy as np
import matplotlib.pyplot as plt

t, ox, oy, oz = np.loadtxt('starvibe.dat', unpack=True)

plt.title('Angular Velocity Components vs. Time')
plt.xlabel('t (s)')
plt.plot(t, ox, label='omega_x')
plt.plot(t, oy, label='omega_y')
plt.plot(t, oz, label='omega_z')
plt.legend()
plt.show()
