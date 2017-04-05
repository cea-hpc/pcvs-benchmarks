
from math import *

y0 = 0.5
yBC = 1.0
yNorm = sqrt(2.0/yBC)

###################

def Power(x, m):
        return x**m

###################

def yCoef(m):

    if m == 0:
        return -16 * pow(y0,7.0) / (35.0 * yBC)

    val = (-48*Power(yBC,4)*(m*pi*y0*(Power(m,2)*Power(pi,2)*Power(y0,2) - 15*Power(yBC,2))*
        cos((m*pi*y0)/yBC) + 3*yBC*
        (-2*Power(m,2)*Power(pi,2)*Power(y0,2) + 5*Power(yBC,2))*
        sin((m*pi*y0)/yBC)))/(Power(m,7)*Power(pi,7))
    
    return yNorm * val

###################

def yCoefInC(m):

    yRoot = y0

    if m == 0:
        return -16 * pow(yRoot,7.0) / (35.0 * yBC )

    m2 = m*m
    pi2 = pi*pi
    yBC4 = yBC*yBC*yBC*yBC
    yRoot2 = yRoot*yRoot
    yBC2 = yBC*yBC

    val = (-48*yBC4*(m*pi*yRoot*(m2*pi2*yRoot2 - 15*yBC2)*
        cos((m*pi*yRoot)/yBC) + 3*yBC*
        (-2*m2*pi2*yRoot2 + 5*yBC2)*
        sin((m*pi*yRoot)/yBC)))/(pow(m*pi,7))
    
    return yNorm * val

###################

def function( y, nTerms = 100 ):

    value = yCoef(0) * (1.0/yBC)

    for m in range(1, nTerms):
        arg = m * pi *y/yBC
        value += yNorm * cos( arg ) * yCoef(m)

    return value

###################

def printCoefFile( nTerms = 100 ):

    outFileName = "yCoefs.dat"
    outFile = open(outFileName, "w" )
 
    for m in range(nTerms):
        str = "%d  %e" % ( m, yCoef(m) )
        outFile.write( str + "\n" )

###################

def testCoefValues( nTerms = 100 ):
 
    for m in range(nTerms):
        coef1 = yCoef(m)
        coef2 = yCoefInC(m)
        error = (coef2 - coef1)/(coef1 + 1.0e-10)

        str = "%d  %e  %e   %e" % ( m, coef1, coef2, error )
        print str

###################

def printFunctionFile( yMin = 0.0, yMax = 1.0,
                       nYPoints = 100,
                       nTerms = 100 ):

    outFileName = "yFunction.dat"
    outFile = open( outFileName, "w" )

    dy = (yMax - yMin)/nYPoints

    for nY in xrange(nYPoints):
        y = yMin + nY*dy
        f = function(y, nTerms)

        str = "%f  %e" % ( y, f )
        outFile.write( str + "\n" )


###################

#printCoefFile()


printFunctionFile(0.0, 1.0, 100, 100)

#testCoefValues()
