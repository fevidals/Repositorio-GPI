#!/bin/bash

rm -f data/out/meta-*
rm -f data/out/study-*

Rscript "code/meta-analysis/1-prep-estimates.R" || exit 1

Rscript "code/meta-analysis/2-meta-analysis.R" || exit 1

Rscript "code/meta-analysis/3-prep-estimates-het.R" || exit 1

Rscript "code/meta-analysis/4-meta-analysis-het.R" || exit 1
