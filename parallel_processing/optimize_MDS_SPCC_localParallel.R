# G. Smith-Vidaurre
# Created 15 May 2023

# Purpose: Optimize MDS by performing this over many dimensions on the SPCC similarity matrix for yellow-naped amazon calls recorded over time.

rm(list = ls())

X <- c("MASS", "future.apply")
invisible(lapply(X, library, character.only = TRUE))
path <- "/Users/gracesmith-vidaurre/Desktop/amazon_temporal_ms"

# Set up parallel processing for certain applications below
cores <- parallel::detectCores() - 2
cores

plan("multicore", workers = cores)

xc_mat <- readRDS(file.path(path, "xc_mat_allCalls.RDS"))
# str(xc_mat)

# Initialize dimensions used to perform MDS
dims <- seq(2, 20, 1)

# Loop over matrices the number of dimensions used to perform MDS

# Convert the similarity matrix to a distance matrix and dist object
dist_mat <- stats::as.dist(1 - xc_mat, diag = TRUE, upper = TRUE) 

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

saveRDS(stress_list, file.path(path, "MDS_stress_SPCC_list.RDS"))
