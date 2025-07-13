#!/bin/bash
(
exec 2>&1
set -x
export PATH=$PATH:$(dirname $0)/../aux
export PREP_DOCUMENT_DIR=/home/urbanjs/venus/V600/github/APPS/prep
fpm install
fpm test
fpm docs
fpm standalone
mv standalone.f90 $HOME/github/lockstockandbarrel/mars/bootstrap/prep.F90
rm -f standalone
ford ford.md
)|tee /tmp/prep.log
