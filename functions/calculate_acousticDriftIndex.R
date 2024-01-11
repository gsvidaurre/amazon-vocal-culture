# G. Smith-Vidaurre
# Created 26 January 2022

# polys: A SpatialPolyGonsDataFrame generated from get95polys() containing the 95% kernel density polygons per groupin each year (the length should be the multiple of the length of group * years)

# groups: The groups of interest (like call types)

# years: The years of interest. This argument should be a named list of lists, such that each element is in order by group, and each element of the list contains the years that you want to compare per group. The names of the list should be the groups, otherwise the function will fail. The minimum length per element should be 2 (e.g. compare the given group one year to another)

# More info: Groups can be regions or years. The comparisons list should have a vector of categories that will be compared to each other in each group (e.g. two regions compared in each year, or two years compared for each call type) 

AcousticDriftIndex <- function(polys, comparisons_list, groups, type){
  
  # Iterate over groups to calculate overlap among polygons for categories in each comparison
  aci_df <- rbindlist(pblapply(1:length(groups), function(x){
    
    # Get all comparisons for the given group
    comp_tmp <- comparisons_list[[x]] %>% 
      dplyr::mutate(
        Var1 = as.character(Var1),
        Var2 = as.character(Var2)
      )
    
    # Iterate over comparisons
    ovlp_list <- data.table::rbindlist(lapply(1:nrow(comp_tmp), function(y){
      
      if(type == "temporal"){
        
        cur <- paste(groups[x], comp_tmp[y, "Var1"], sep = "_")
        nex <- paste(groups[x], comp_tmp[y, "Var2"], sep = "_")
        
      } else if(type == "spatial"){
        
        cur <- paste(comp_tmp[y, "Var1"], groups[x], sep = "_")
        nex <- paste(comp_tmp[y, "Var2"], groups[x], sep = "_")
        
      } else if(type == "variants"){
        
        cur <- comp_tmp[y, "Var1"]
        nex <- comp_tmp[y, "Var2"]
        
      }
      
      # Get the area of the polygon for the first group in the comparison
      curr_area <- raster::area(polys[[cur]])
      
      # Get the area of the polygon for the second group in the comparison
      next_area <- raster::area(polys[[nex]])
      
      # Get the intersection polygon between polygons if these overlap
      if(class(try(raster::intersect(polys[[cur]], polys[[nex]]))) != "try-error"){
        
        intersect_poly <- raster::intersect(polys[[cur]], polys[[nex]])
        
        # Area of the intersection polygon
        intersect_area <- raster::area(intersect_poly)
        
      } else {
        
        intersect_area <- 0
        
      }
      
      # Get the polygon area for the first group that does not overlap with second group (e.g. the non-intersection area of the first group)
      ni_area_curr <- curr_area - intersect_area
      
      # Get the polygon area for the second group that does not overlap with first group (e.g. the non-intersection area of the second group)
      ni_area_next <- next_area - intersect_area
      
      # Merge these calculations together to get the total area across both polygons (without double-counting the intersection area)
      comb_area <- ni_area_curr + ni_area_next + intersect_area
      
      # A symmetric index of acoustic overlap (e.g. this index will be the same regardless of which polygon is used as the baseline). When a polygon is compared to itself, then this index is 0. When the intersection area is 0, meaning that there is no overlap between the polygons, then this index goes to 1. So values closer to 0 mean more overlap in acoustic space, and values closer to 1 mean more drift in acoustic space
      aci <- round((comb_area - intersect_area) / comb_area, 2)

      ovlp_df <- data.frame(group = groups[x], category_1 = comp_tmp[y, "Var1"], category_2 = comp_tmp[y, "Var2"], acoustic_drift = aci)
      
      return(ovlp_df)
      
    }))
    
    return(ovlp_list)
    
  }))
  
  return(aci_df)

}
