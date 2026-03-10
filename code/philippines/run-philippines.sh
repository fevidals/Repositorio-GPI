#!/bin/bash

Rscript "code/philippines/0-clean-units.R" || exit 1
Rscript "code/philippines/1-clean-admin.R" || exit 1
Rscript "code/philippines/2-clean-citizen.R" || exit 1
Rscript "code/philippines/3-clean-officer.R" || exit 1
Rscript "code/philippines/4-construction.R" || exit 1
Rscript "code/philippines/5-estimation.R" || exit 1
Rscript "code/philippines/6-balance-tests.R" || exit 1
Rscript "code/philippines/7-het-effects-test.R" || exit 1

  