# G. Smith-Vidaurre
# Created 16 May 2023

# Purpose: Optimize MDS by performing this over many dimensions on the random forests (RF) similarity matrix for yellow-naped amazon calls recorded over time.

rm(list = ls())

X <- c("MASS", "future.apply")
invisible(lapply(X, library, character.only = TRUE))
path <- "/Users/gracesmith-vidaurre/Desktop/amazon_temporal_ms"

# Set up parallel processing for certain applications below
cores <- parallel::detectCores() - 2
cores

plan("multicore", workers = cores)

rf_prox <- readRDS(file.path(path, "rf_unsup_prox.RDS"))
# str(rf_prox)

# Initialize dimensions used to perform MDS
dims <- seq(2, 20, 1)

# Loop over matrices the number of dimensions used to perform MDS

# Convert the similarity matrix to a distance matrix and dist object
dist_mat <- stats::as.dist(1 - rf_prox, diag = TRUE, upper = TRUE) 

# Iterate over dimension numbers
stress_list <- invisible(future_lapply(1:length(dims), function(j){
  
  cat(paste("j = ", j, "\n")) 
  
  # Perform MDS
  iso <- invisible(isoMDS(dist_mat, k = dims[j], maxit = 1000, trace = FALSE))
  
  # isoMDS returns stress as a percentage
  # Return the full set of results
  return(iso)
  
}))

names(stress_list) <- dims

saveRDS(stress_list, file.path(path, "MDS_stress_RF_list.RDS"))
