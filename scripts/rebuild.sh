#!/bin/bash
export PATH=$PATH:$(dirname $0)/aux
fpm install
fpm test
fpm docs
fpm standalone
mv ffpm.f90 standalone/prep.f90
rm -f ffpm
ford ford.md
