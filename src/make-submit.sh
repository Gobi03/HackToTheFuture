#!/bin/sh

file=Submit.scala

rm -f ${file}
touch ${file}

# generate file
cat Entities.scala >> ${file}
cat CommonPackage.scala >> ${file}

cat Main.scala >> ${file}
