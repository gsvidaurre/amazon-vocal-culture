# G.Smith-Vidaurre
# Created 26 January 2022
# Code modified from https://mhallwor.github.io/_pages/activities_GenerateTerritories

# Function for 95% kernel density contour. Get the 95% kernel density estimate. Determine the 95% contour value, then set all raster values that are less than that value to NA. Convert the raster of the 95% kernel density estimate into a polygon to get area.

# kde = kernel density estimate
# prob = probability: default is 0.95

getKDEContour <- function(kde, prob = 0.95){
  
  # Set all values 0 to NA
  kde[kde == 0] <- NA
  
  # Create a vector of raster values
  kde_values <- raster::getValues(kde)
  
  # Sort values 
  sortedValues <- sort(kde_values[!is.na(kde_values)], decreasing = TRUE)
  
  # find cumulative sum up to ith location
  sums <- cumsum(as.numeric(sortedValues))
  
  # Use cumulative sums to identify raster cells less than or equal to the 95% KDE probability (or 95% of all points per call type)
  # then p is the total number of values inside the contour and can be used as an index to filter the vector above of sorted values which provides the actual KDE 95% threshold value to filter by points in or out of the 95% contour
  p <- sum(sums <= prob * sums[length(sums)])
  
  # Set values in raster to 1 or 0
  kdeprob <- raster::setValues(kde, kde_values >= sortedValues[p])
  
  # Return new kde
  return(kdeprob)
}

# Function to get convert contours of 95% kernel density estimate to polygons by groups of interest:
# df: must be a data frame with the first 2 dimensions of MDS or other low-dimensional coordinates
# X_col: the name of the column with the first dimension of low-dimensional coordinates
# Y_col: the name of the column with the second dimension of low-dimensional coordinates
# group_col: the name of the column with groups of interest, so as to return a separate polygon per group. This column must be a factor for the function to run

get95polys <- function(df, X_col, Y_col, group_col){
  
  # Create a SpatialPoints object for calls using MDS coordinates
  # crs = WGS84 
  call_coords <- sp::SpatialPoints(coords = cbind(df[[X_col]], df[[Y_col]]),
                                   proj4string = sp::CRS("+init=epsg:4326"))
  
  head(call_coords)
  # proj4string(call_coords)
  
  # Create a SpatialPointsDataFrame
  call_coords_spdf <- sp::SpatialPointsDataFrame(call_coords, df)
  # head(call_coords_spdf)
  
  # Split data by call type and year
  call_coords_sep <- split(x = call_coords_spdf, f = call_coords_spdf[[group_col]], drop = FALSE)
  # length(call_coords_sep)
  
  base::names(call_coords_sep) <- base::levels(df[[group_col]])
  # names(call_coords_sep)
  
  # Estimate the bandwidth for each call type and year
  bw <- lapply(call_coords_sep, FUN = function(x){
    ks::Hlscv(x@coords)
  })
  # bw
  
  # Get the kernel density estimate
  # ?mapply
  
  call_kde <- mapply(call_coords_sep, bw, SIMPLIFY = FALSE, FUN = function(x, y){ 
    raster(kde(x@coords, h = y))
  })
  # str(call_kde)
  
  # Get the 95% kernel density estimate
  call_95kde <- pblapply(call_kde, FUN = getKDEContour, prob = 0.95)
  # str(call_95kde)
  # names(call_95kde)
  
  # Make a polygon per call type. Change all the values in the 95% KDE to 1 first, to make a single polygon that represents the 95% KDE
  # x <- call_95kde[[2]] # testing
  # str(x)
  
  call_95poly <- pblapply(call_95kde, FUN = function(x){
  
    x[x == 0] <- NA
    
    y <- raster::rasterToPolygons(x, dissolve = TRUE)
    
    # Convert to an sf object
    tt <- sf::st_as_sf(y, coords = c("x", "y"), crs = CRS(y))
    
    # Remove holes from sf object with nngeo
    tt2 <- nngeo::st_remove_holes(tt, max_area = 0)
    
    # Then smooth the polygons with smoothr package, values of smoothness > 1 lead to smoother edges
    sy <- smoothr::smooth(tt2, method = "ksmooth", smoothness = 5)
    
    # Convert back to an sp object
    sy <- sf::as_Spatial(sy)
    # plot(y)
    # dev.off()
    return(sy)
    
  })
  
  # length(call_95poly)
  # names(call_95poly)
  
  # Change the polygon ID field to a unique ID per polygon per call type before merging the 95% KDE together into a single SpatialPolygonsDataFrame
  call_95poly <- mapply(call_95poly, names(call_95poly), SIMPLIFY = FALSE, FUN = function(x, y){
    x@polygons[[1]]@ID <- y
    return(x)
  })
  # str(call_95poly)
  # names(call_95poly)
  
  return(call_95poly)
  
}