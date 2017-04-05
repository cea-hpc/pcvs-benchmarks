
from math import *

x0 = 0.5
xBC = 1.0
xNorm = sqrt(2.0/xBC)

###################

def Power(x, n):
        return x**n

###################

def xCoef(n):

    val = (2*xBC*(12*(1 + 2*n)*pi*x0*xBC*(1920*Power(xBC,4) - 
                                          32*Power(pi,2)*Power(x0,2)*Power(xBC + 2*n*xBC,2))*
                  cos(((1 + 2*n)*pi*x0)/(2.*xBC)) + 
                  4608*Power(xBC,4)*(Power(pi,2)*Power(x0 + 2*n*x0,2) - 10*Power(xBC,2))*
                  sin(((1 + 2*n)*pi*x0)/(2.*xBC))))/Power(pi + 2*n*pi,7)
    
    return xNorm * val

###################

def function( x, nTerms = 100 ):

    value = 0.0

    #if x > x0:
    #    return value

    for n in range(nTerms):
        arg = ( 2*n + 1 ) * pi/2.0 *x/xBC
        value += cos( arg ) *xCoef(n)

    return xNorm * value


###################

def printCoefFile( nTerms = 100 ):

    outFileName = "xCoefs.dat"
    outFile = open(outFileName, "w" )
 
    for n in range(nTerms):
        str = "%d  %e" % ( n, xCoef(n) )
        outFile.write( str + "\n" )

###################

def printFunctionFile( xMin = 0.0, xMax = 1.0,
                       nXPoints = 100,
                       nTerms = 100 ):

    outFileName = "xFunction.dat"
    outFile = open( outFileName, "w" )

    dx = (xMax - xMin)/nXPoints

    for nX in xrange(nXPoints):
        x = xMin + nX*dx
        f = function(x, nTerms)

        str = "%f  %e" % ( x, f )
        outFile.write( str + "\n" )


###################

#printCoefFile()


printFunctionFile(0.0, 1.0, 100, 100)

