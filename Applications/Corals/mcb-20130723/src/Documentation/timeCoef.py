
from math import *

kmn = 1.0e-5
omega = 1.0e-5
phi = pi/2.0
t = 2.0

###################

def Power(x, m):
        return x**m

###################

def tCoef():

    val = (0.5 - 0.5/Power(e,kmn*t))/kmn + (0.5*(omega*cos(phi) - Power(e,kmn*t)*omega*cos(phi + omega*t) - kmn*sin(phi) + Power(e,kmn*t)*kmn*sin(phi + omega*t)))/(Power(e,kmn*t)*(Power(kmn,2) + Power(omega,2)))
    

    return val

###################

def tCoefInC():
  expKmnt = exp( kmn * t );
  kmn2 = kmn*kmn
  omega2 = omega*omega
  expmk = exp( -kmn * t );

  term1 = (0.5 - 0.5*expmk)/kmn

  term2a = omega * cos(phi) * expmk
  term2b = -omega*cos(phi + omega*t)
  term2c = -kmn*sin(phi) * expmk
  term2d = kmn*sin(phi + omega*t)
  denom = kmn2 + omega2
  
  term2 = (term2a + term2b + term2c + term2d)/denom
  val = term1 + 0.5*term2

  return val

###################

coef1 = tCoef()
coef2 = tCoefInC()
error = (coef2 - coef1)/(coef1 + 1.0e-10)

print  coef1, coef2, error
