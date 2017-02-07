#!/bin/sh
export OMP_NUM_THREADS=3
echo "Running AMG2013..."
echo "------------------"
echo "Using solver 1 -n=8 -p=8"
echo "------------------"
echo mpcrun -n=8 -p=8 ./amg2013 -laplace -P 2 2 2 -n 150 150 150 -solver 1 
time mpcrun -n=8 -p=8 ./amg2013 -laplace -P 2 2 2 -n 150 150 150 -solver 1 

echo ""
echo "------------------"
echo "Using solver 1 -n=8 -p=1"
echo "------------------"
echo mpcrun -n=8 -p=1 ./amg2013 -laplace -P 2 2 2 -n 150 150 150 -solver 1 
time mpcrun -n=8 -p=8 ./amg2013 -laplace -P 2 2 2 -n 150 150 150 -solver 1 

echo ""
echo "------------------"
echo "Using solver 2 -n=8 -p=8"
echo mpcrun -n=8 -p=8 ./amg2013 -laplace -P 2 2 2 -n 150 150 150 -solver 2 
time mpcrun -n=8 -p=8 ./amg2013 -laplace -P 2 2 2 -n 150 150 150 -solver 2 

echo ""
echo "------------------"
echo "Using solver 2 -n=8 -p=1"
echo mpcrun -n=8 -p=1 ./amg2013 -laplace -P 2 2 2 -n 150 150 150 -solver 2 
time mpcrun -n=8 -p=1 ./amg2013 -laplace -P 2 2 2 -n 150 150 150 -solver 2 

echo ""
echo "------------------"
echo "Using solver 3 -n=8 -p=8"
echo "------------------"
echo mpcrun -n=8 -p=8 ./amg2013 -laplace -P 2 2 2 -n 150 150 150 -solver 3 
time mpcrun -n=8 -p=8 ./amg2013 -laplace -P 2 2 2 -n 150 150 150 -solver 3 

echo ""
echo "------------------"
echo "Using solver 3 -n=8 -p=1"
echo "------------------"
echo mpcrun -n=8 -p=1 ./amg2013 -laplace -P 2 2 2 -n 150 150 150 -solver 3 
time mpcrun -n=8 -p=1 ./amg2013 -laplace -P 2 2 2 -n 150 150 150 -solver 3 

