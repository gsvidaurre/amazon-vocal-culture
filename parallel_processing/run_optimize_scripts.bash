#!/bin/bash

# G. Smith-Vidaurre
# 15 and 16 May 2023: Execute the R scripts to run MDS optimization using parallel multicore processing

# R CMD BATCH ./optimize_MDS_SPCC_localParallel.R
R CMD BATCH ./optimize_MDS_RF_localParallel.R
