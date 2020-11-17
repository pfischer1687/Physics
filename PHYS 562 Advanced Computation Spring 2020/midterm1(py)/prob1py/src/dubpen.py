
#!/usr/bin/env python3
import numpy as np
import math as math

# set constants
n_eq = 4
g = 1
l_1 = 2
m_1 = 3
l_2 = 1
m_2 = 1

def main():

    # set time parameters
    t = 0
    dt = 0.1
    tmax = 600

    # set initial conditions (w/ omega_2 kick)
    y = np.zeros(n_eq)
    y[0] = math.pi                               # theta_1
    y[1] = 0                                     # omega_1
    y[2] = math.pi                               # theta_2
    y[3] = math.sqrt(2 * 0.01 / (m_2 * l_2**2) ) # omega_2

    # Calculate energy from initial conditions
    Ei = np.zeros(3)
    Ei[0] = 1 / 2 * m_2 * l_2**2 * y[3]**2
    Ei[1] = m_1 * g * (l_2 + l_1 * (1 - math.cos(y[0]) ) )
    Ei[2] = m_2 * g * (l_1 * (1 - math.cos(y[0]) + l_2 * (1 - math.cos(y[2]) ) ) )
    E = np.sum(Ei)

    # print energy and omega_2 kick
    print("E =", E, "omega_2 =", y[3])

    # open data file
    outfile = open('dubpen.dat', 'w')
    outfile.write(f'#{"time":>15}{"x_1":>16}{"y_1":>16}{"x_2":>16}\
    {"y_2":>16}{"theta_1":>16}{"theta_2":>16}{"y2":>16}{"omega_2":>16}\n')

    while t < tmax:

        # time for plot
        outfile.write(f'{t:16.8f}')

        # plot spatial path of double pedulum
        x_1 = l_1 * math.sin(y[0])
        y_1 = l_2 + l_1 * (1 - math.cos(y[0]) )
        x_2 = x_1 + l_2 * math.sin(y[2])
        y_2 = y_1 - l_2 * math.cos(y[2])
        outfile.write(f'{x_1:16.8f}{y_1:16.8f}{x_2:16.8f}{y_2:16.8f}')

        # plot theta_1 and theta_2 vs. time
        outfile.write(f'{y[0]:16.8f}{y[2]:16.8f}')

        # plot Poincare section
        eps = 0.1
        y0 = y[0]
        y2 = y[2]
        if y0 > math.pi:
            while y0 > math.pi:
                y0 -= 2 * math.pi 
        elif y0 < -math.pi:
            while y0 < -math.pi:
                y0 += 2 * math.pi
        
        if y2 > math.pi:
            while y2 > math.pi:
                y2 -= 2 * math.pi 
        elif y2 < -math.pi:
            while y2 < -math.pi:
                y2 += 2 * math.pi

        # set epsilon for Poincare section tolerance
        yl = y0 - eps 
        yu = y0 + eps 
        if yl < 0 and yu > 0 and y[1] > 0:
            outfile.write(f'{y2:16.8f}{y[3]:16.8f}')
        else:
            outfile.write(f'{0:16.8f}{0:16.8f}')
       
        outfile.write(f'\n')

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

    f[0] = y[1]
    f[2] = y[3]

    # numerator and denominator terms for f[1,3]
    n_1 = np.zeros(4)
    n_1[0] = -g * (2 * m_1 + m_2) * math.sin(y[0])
    n_1[1] = -m_2 * g * math.sin(y[0] - 2 * y[2]) 
    coeff = 2 * math.sin(y[0] - y[2])
    n_1[2] = -coeff * m_2 * y[3]**2 * l_2
    n_1[3] = -coeff * m_2 * y[1]**2 * l_1 * math.cos(y[0] - y[2])
    n_2 = np.zeros(3)
    n_2[0] = y[1]**2 * l_1 * (m_1 + m_2)
    n_2[1] = g * (m_1 + m_2) * math.cos(y[0])
    n_2[2] = y[3]**2 * l_2 * m_2 * math.cos(y[0] - y[2])
    d = 2 * m_1 + m_2 - m_2 * math.cos(2 * (y[0] - y[2]) )

    f[1] = np.sum(n_1) / (l_1 * d) 
    f[3] = 2 * math.sin(y[0] - y[2]) * np.sum(n_2) / (l_2 * d)

    return f

if __name__ == '__main__':
    main()
