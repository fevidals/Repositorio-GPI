#!/bin/bash

rm -Rf data/out
mkdir data/out

# run data cleaning and estimation country-by-country
bash -x code/brazil/run-brazil.sh || exit 1
bash -x code/colombia/run-colombia.sh || exit 1
bash -x code/liberia/run-liberia.sh || exit 1
bash -x code/pakistan/run-pakistan.sh || exit 1
bash -x code/philippines/run-philippines.sh || exit 1
bash -x code/uganda/run-uganda.sh || exit 1
