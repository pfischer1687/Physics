#!/usr/bin/env python3
import numpy as np
import matplotlib.pyplot as plt

t, x_1, y_1, x_2, y_2, theta_1, theta_2 = np.loadtxt('dubpen.dat', 
usecols=(0,1,2,3,4,5,6), unpack=True)

y2, omega_2 = np.loadtxt('dubpen.dat', usecols=(7,8), unpack=True)

plt.title('Spatial Path of Double Pendulum')
plt.xlabel('x (m)')
plt.plot(x_1, y_1, label='m_1')
plt.plot(x_2, y_2, label='m_2')
plt.legend()
plt.show()

plt.title('theta_1 vs. t')
plt.xlabel('t (s)')
plt.plot(t, theta_1, label='theta_1 (rad)')
plt.legend()
plt.show()

plt.title('theta_2 vs. t')
plt.xlabel('t (s)')
plt.plot(t, theta_2, label='theta_2 (rad)')
plt.legend()
plt.show()

plt.title('Poincare Section')
plt.xlabel('theta_2 (rad)')
plt.scatter(y2, omega_2, label='omega_2 (rad/s)')
plt.legend()
plt.show()
