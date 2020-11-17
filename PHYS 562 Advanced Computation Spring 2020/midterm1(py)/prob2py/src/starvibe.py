
#!/usr/bin/env python3
import numpy as np
import math as math

# set parameters
n_eq = 6
m = 40E30
r = 10E3
nu = 40E3 
eps = 10E-8

def main():

    # set time parameters
    t = 0
    dt = 0.01
    tmax = 20

    # set constants
    w = 2 * math.pi * nu 
    omega = 100 * w
    I0 = 2 / 5 * m * r**2

    # set initial conditions (angular momentums)
    y = np.zeros(n_eq)
    y[3] = I0 * omega / math.sqrt(3) # Lx
    y[4] = I0 * omega / math.sqrt(3) # Ly
    y[5] = I0 * omega / math.sqrt(3) # Lz

    outfile = open('starvibe.dat', 'w')
    outfile.write(f'#{"t":>30}{"ox":>30}{"oy":>30}{"oz":>30}\n')

    while t < tmax:

        # calculate angular velocities
        Ix = I0 * (1 - eps / 2 * math.cos(w * t) )
        Iy = Ix
        Iz = I0 * (1 + eps * math.cos(w * t) )
        o = np.zeros(3)
        o[0] = y[3] / Ix
        o[1] = y[4] / Iy 
        o[2] = y[5] / Iz 
        if t == 0:
            print(o[0], o[1], o[2])

        # plot angular velocities
        outfile.write(f'{t:30.8f}{o[0]:30.8f}{o[1]:30.8f}{o[2]:30.8f}\n')

        t, y = rk4step(t, dt, y)

    outfile.close()

def rk4step(t, dt, y):

    k1 = dt * deriv(t, y)
    k2 = dt * deriv(t + dt/2, y + k1/2)
    k3 = dt * deriv(t + dt/2, y + k2/2)
    k4 = dt * deriv(t + dt, y + k3)

    dy = (k1 + 2*k2 + 2*k3 + k4) / 6

    return t + dt, y + dy

def deriv(t, y):

    f = np.zeros_like(y)

    w = 2 * math.pi * nu 
    I0 = 2 / 5 * m * r**2
    g = eps * math.cos(w * t)

    f[0] = y[3]
    f[1] = y[4]
    f[2] = y[5]

    f[3] = -3 / (2 * I0) * y[4] * y[5] * g / ( (1 + g) * (1 - g / 2) )

    f[4] = 3 / (2 * I0) * y[3] * y[5] * g / ( (1 + g) * (1 - g / 2) )

    f[5] = 0

    return f

if __name__ == '__main__':
    main()
