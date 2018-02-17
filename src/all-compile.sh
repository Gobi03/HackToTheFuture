#!/bin/sh

rm -f *.class

fsc Entities.scala
fsc CommonPackage.scala

fsc Main.scala
