#!/bin/bash

Rscript "code/uganda/0-clean-units.R" || exit 1
Rscript "code/uganda/1-clean-admin.R" || exit 1
Rscript "code/uganda/2-clean-citizen.R" || exit 1
Rscript "code/uganda/3-clean-officer.R" || exit 1
Rscript "code/uganda/4-construction.R" || exit 1
Rscript "code/uganda/5-estimation.R" || exit 1
Rscript "code/uganda/6-het-effects.R" || exit 1
Rscript "code/uganda/7-balance-tests.R" || exit 1
Rscript "code/uganda/8-het-effects-test.R" || exit 1
