#!/bin/bash
#
diff -i 1.0.dat 1.1.dat > 10.11.diff
diff -i 1.1.dat 1.3.dat > 11.13.diff
diff -i 1.3.dat 2.0.dat > 13.20.diff
diff -i 2.0.dat 2.1.dat > 20.21.diff
diff -i 2.1.dat 2.2.dat > 21.22.diff
diff -i 2.2.dat 3.0.dat > 22.30.diff
diff -i 3.0.dat 3.1.dat > 30.31.diff
diff -i 3.1.dat 4.0.dat > 31.40.diff
