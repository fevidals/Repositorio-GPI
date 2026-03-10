#!/bin/bash

Rscript "code/pakistan/0-clean-units.R" || exit 1
Rscript "code/pakistan/1-clean-admin.R" || exit 1
Rscript "code/pakistan/2-clean-citizen.R" || exit 1
Rscript "code/pakistan/3-clean-officer.R" || exit 1
Rscript "code/pakistan/4-construction.R" || exit 1
Rscript "code/pakistan/5-estimation.R" || exit 1
Rscript "code/pakistan/6-het-effects.R" || exit 1
Rscript "code/pakistan/7-balance-tests.R" || exit 1
Rscript "code/pakistan/8-het-effects-test.R" || exit 1

