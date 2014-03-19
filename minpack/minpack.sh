#!/bin/bash
#
mkdir temp
cd temp
rm *
../f90split ../minpack.f90
#
for FILE in `ls -1 *.f90`;
do
  gfortran -c -g $FILE >& compiler.txt
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
  rm compiler.txt
done
rm *.f90
#
ar qc libminpack.a *.o
rm *.o
#
mv libminpack.a ..
cd ..
rmdir temp
#
echo "Library installed as libminpack.a"
