# G. Smith-Vidaurre
# Created 26 January 2022

# df: must be a data frame with the first 2 dimensions of MDS or other low-dimensional coordinates

# comparisons_list: A list of lists of the comparisons that will be used to compare polygons against one another. This must have a list for each group specified

# polys: A SpatialPolyGonsDataFrame generated from get95polys() containing the 95% kernel density polygons per groupin each year (the length should be the multiple of the length of group * years)

# groups: The groups of interest (like call types)

# years: The years of interest. This argument should be a named list of lists, such that each element is in order by group, and each element of the list contains the years that you want to compare per group. The names of the list should be the groups, otherwise the function will fail. The minimum length per element should be 2 (e.g. compare the given group one year to another)

# cols: A list of lists by groups, in which each nested list is a vector of colors, and each element corresponds to a different year per group
# North, South, Nicaragua
# 1994, 2005, 2016 for North and South; 1994 and 2005 for Nicaragua

# group_col: The name of the column in the object df with groups of interest, so as to return a separate polygon per group. This column must be a factor for the function to run

# figure_type: A character string specifying the type of figures that should be output. Use "composite" to save all panels (temporal comparisons) in a single figure, and use "separate" to save a sseparate figure per temporal comparison

# path: A path where image files will be saved 

# img_file_nm: The file name (plus extension) of the main image file (no legend) when figure_type is "composite". TIFF format only. The default is NULL

# legnd_file_nm: The file name (plus extension) of a separate image file for the figure legend. Writing this out separately allows for more flexibility in arranging the final figure in downstream programs. TIFF format only

# img_width: Width of the main image file in inches. This should be about 8 - 9 in for composite figures

# img_height: Height of the main image file in inches. This should be about 6 inches for composite figures

make95PolyPlots_temporal <- function(df, comparisons_list, polys, groups, years, cols, group_col, figure_type, path, img_file_nm = NULL, legnd_file_nm, img_width, img_height){
  
  # Convert the list of polygons to a SpatialPolygonsDataFrame
  # First merge the polygons together into a single SpatialPolygons object
  polys_spdf <- do.call(rbind, polys)
  # str(polys_spdf)
  
  # Add back the group column to the SpatialPolygonsDataFrame as an ID slot
  polys_spdf[[group_col]] <- getSpPPolygonsIDSlots(polys_spdf)
  
  yrs_ord <- as.character(unique(unlist(years)[order(unlist(years), decreasing = FALSE)]))
  # yrs_ord
  
  # Also use the fortified data frame to make plots
  ggpoly <- fortify(polys_spdf) %>%
    dplyr::mutate(
      id = factor(id, levels = levels(df[[group_col]])),
      groups = gsub("_([0-9]+)", "", id),
      year = factor(gsub("([A-Z]+[a-z]+)_", "", id), levels = yrs_ord)
    )
  # glimpse(ggpoly)
  
  # Set the names of the overall list of colors
  names(cols) <- groups
  
  # Iterate over groups to make a separate plot for each temporal comparison within the historical call types. Arrange these plots into a single composite figure or generate separate image files
  
  # Initialize an empty list outside of the loop for saving lists of plots
  gg_list_all <- list()
  
  invisible(pblapply(1:length(groups), function(x){
    
    # Initialize an empty list inside of the loop for saving separate plots together in a list
    gg_list <- list()
    
    # Get all comparisons for the given group
    comp_tmp <- comparisons_list[[x]] %>% 
      dplyr::mutate(
        Var1 = as.character(Var1),
        Var2 = as.character(Var2)
      ) %>% 
      # Filter by the comparisons unique to each historical call type over time
      dplyr::mutate(
        Vars = paste(Var1, Var2, sep = "-")
      ) %>% 
      # Arrange rows by the first group (year) in ascending order
      dplyr::arrange(-desc(Var1))
    
    if(groups[x] %in% c("North", "South")){
      
      comp_tmp <- comp_tmp %>% 
        dplyr::filter(Vars %in% c("1994-2005", "2005-2016", "2016-1994"))
      
    } else if(groups[x] == "Nicaragua"){
      
      comp_tmp <- comp_tmp %>% 
        dplyr::filter(Vars == "1994-2005")
      
    }
    
    # glimpse(comp_tmp)
    
    # Set the names of the list of colors for the given call type. These should be the years of sampling in increasing order (the earlier year should come first)
    if(nrow(comp_tmp) > 1){
      
      names(cols[[x]]) <- comp_tmp$Var1[order(as.numeric(comp_tmp$Var1), decreasing = FALSE)]
      
    } else if(nrow(comp_tmp) == 1){
      
      names(cols[[x]]) <- c(comp_tmp$Var1, comp_tmp$Var2)
      
    }
    
    # Iterate over comparisons
    invisible(lapply(1:nrow(comp_tmp), function(y){
      
      cur <- paste(groups[x], comp_tmp[y, "Var1"], sep = "_")
      nex <- paste(groups[x], comp_tmp[y, "Var2"], sep = "_")
      
      # Add a new column to customize strip label
      tmp_grp <- groups[x]
      
      ggpoly_tmp <- ggpoly %>%
        dplyr::filter(grepl(tmp_grp, id)) %>%
        dplyr::mutate(
          year_comp = paste(comp_tmp[y, "Var1"], comp_tmp[y, "Var2"], sep = " & "),
          year_comp = factor(year_comp)
        )
      
      # glimpse(ggpoly_tmp)
      
      # Coordinates and settings for axis limits
      
      if(figure_type == "composite"){
        
        # Coordinates and settings for axis limits
        min_x <- round(min(ggpoly$long), 2) # previously ggpoly_tmp was used to set this limit and those below
        max_x <- round(max(ggpoly$long), 2)
        bufx <- max_x/5
        breaks_x <- round(seq(min_x - bufx, max_x + bufx, (max_x + bufx)/4), 2)
        # breaks_x
        
        min_y <- round(min(ggpoly$lat), 2)
        max_y <- round(max(ggpoly$lat), 2)
        bufy <- max_y/5
        breaks_y <- round(seq(min_y - bufy, max_y + bufy, (max_y + bufy)/4), 2)
        # breaks_y
        
      } else if(figure_type == "separate") {
        
        # Coordinates and settings for axis limits
        min_x <- round(min(ggpoly$long), 2) # previously ggpoly_tmp was used to set this limit and those below
        max_x <- round(max(ggpoly$long), 2)
        bufx <- round(max_x/5, 2)
        breaks_x <- round(seq(round(min_x - bufx, 2), round(max_x + bufx, 2), round((max_x + bufx)/3, 2)), 2)
        # breaks_x
        
        min_y <- round(min(ggpoly$lat), 2)
        max_y <- round(max(ggpoly$lat), 2)
        bufy <- round(max_y/5, 2)
        breaks_y <- round(seq(round(min_y - bufy/2, 2), round(max_y + bufy, 2), round((max_y + bufy)/3, 2)), 2)
        # breaks_y
        
      }
      
      gg_tmp <- ggplot(data = ggpoly_tmp, aes(x = long, y = lat, group = group)) +
        # Get the right colors for these two years and the given group
        geom_polygon(data = ggpoly_tmp %>%
                       dplyr::filter(id == cur) %>%
                       droplevels(), color = cols[[x]][[comp_tmp[y, "Var1"]]], fill = cols[[x]][[comp_tmp[y, "Var1"]]]) +
        geom_polygon(data = ggpoly_tmp %>%
                       dplyr::filter(id == nex) %>%
                       droplevels(), color = cols[[x]][[comp_tmp[y, "Var2"]]], fill = cols[[x]][[comp_tmp[y, "Var2"]]]) +
        scale_x_continuous(limits = c(min_x - bufx, max_x + bufx), breaks = breaks_x, labels = breaks_x) +
        scale_y_continuous(limits = c(min_y - bufy, max_y + bufy), breaks = breaks_y, labels = breaks_y) +
        xlab("") +
        ylab("") +
        guides(color = "none", fill = "none") +
        theme_bw() +
        theme(
          axis.text.x = element_text(size = 10),
          strip.text.x = element_text(size = 12, margin = ggplot2::margin(0.25, 0, 0.25, 0, "line")),
          strip.text.y = element_text(size = 12, margin = ggplot2::margin(0, 0.25, 0, 0.25, "line")),
          axis.title = element_text(size = 11),
          plot.margin = unit(rep(0.1, 4), "line"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
      
      # Add x and y strip labels with the call type and temporal comparison 
      if(figure_type == "composite"){
        
        gg_tmp <- gg_tmp +
          facet_grid(groups ~ year_comp, switch = "y") +
          theme(
            axis.text.y = element_text(size = 10)
          )
        
      } else if(figure_type == "separate"){
        
        gg_tmp <- gg_tmp +
          theme(
            axis.text.y = element_text(size = 10)
          )
        
      }
      
      gg_list[[y]] <<- gg_tmp
      
      
    }))
    
    names(gg_list) <- comp_tmp$Vars
    
    gg_list_all[[x]] <<- gg_list
    
  }))
  
  names(gg_list_all) <- unlist(groups)
  
  #### Get the legend as a separate image file
  
  # Make a fake data frame for a customized legend. This is not very generalized
  levs <- paste(rep(unlist(groups), each = length(yrs)), yrs, sep = " - ")
  
  if(("Nicaragua" %in% groups) & ("2016" %in% yrs)){
    levs <- levs[-grep("Nicaragua - 2016", levs)]
    # levs
  }
  
  tmp_df <- ggpoly %>%
    dplyr::mutate(
      groups_year = paste(groups, year, sep = " - "),
      groups_year = factor(groups_year, levels = levs)
    )
  
  tmp_cols <- as.vector(unlist(cols))
  # tmp_cols
  
  # Get the legend across years
  gg_leg <- gtable::gtable_filter(ggplot_gtable(ggplot_build(
    ggplot(tmp_df, aes(x = long, y = lat, color = groups_year)) +
      geom_line(linewidth = 3) +
      scale_color_manual(values = tmp_cols) +
      theme_bw() +
      guides(color = guide_legend(title = "Call type & Year", ncol = 3, byrow = TRUE)) +
      theme(legend.direction = "vertical", legend.title = element_text(size = 12), legend.text = element_text(size = 10), legend.key.width = unit(1, "lines"), legend.justification = "right")
  )), "guide-box")
  
  # class(as.ggplot(gg_leg))
  # grid::grid.draw(gg_leg)
  dev.off()
  
  # Save the legend in a separate file
  tiff(file.path(path, legnd_file_nm), width = 4, height = 2, units = "in", res = 300)
  grid::grid.draw(gg_leg)
  dev.off()
  
  ##### Save the plots in either a single composite image file or separate image files
  
  if(figure_type == "composite"){
    
    # Arrange plots in a single image file depending on how many plots there are
    if(length(groups) * length(yrs) > 6){
      
      tiff(file.path(path, img_file_nm), width = img_width, height = img_height, units = "in", res = 300)
      ggarrange(
        gg_list_all[[groups[1]]][[1]],
        gg_list_all[[groups[1]]][[2]],
        gg_list_all[[groups[1]]][[3]],
        gg_list_all[[groups[2]]][[1]],
        gg_list_all[[groups[2]]][[2]],
        gg_list_all[[groups[2]]][[3]],
        gg_list_all[[groups[3]]][[1]],
        nrow = 3,
        ncol = 3,
        widths = rep(floor(img_width/3), 3),
        heights = rep(floor(img_height/3), 3),
        left = textGrob(
          "Dimension 2",
          gp = gpar(fontsize = 19),
          rot = 90
        ),
        bottom = textGrob(
          "Dimension 1",
          gp = gpar(fontsize = 19)
        )
      )
      dev.off()
      
    } else if(length(groups) * length(yrs) == 6){
      
      tiff(file.path(path, img_file_nm), width = img_width, height = img_height, units = "in", res = 300)
      ggarrange(
        gg_list_all[[groups[1]]][[1]],
        gg_list_all[[groups[1]]][[2]],
        gg_list_all[[groups[1]]][[3]],
        gg_list_all[[groups[2]]][[1]],
        gg_list_all[[groups[2]]][[2]],
        gg_list_all[[groups[2]]][[3]],
        nrow = 3, # to keep panel proportion the same for visual comparisons
        ncol = 3,
        widths = rep(floor(img_width/3), 3),
        heights = rep(floor(img_height/3), 3),
        left = textGrob(
          "Dimension 2",
          gp = gpar(fontsize = 19),
          rot = 90
        ),
        bottom = textGrob(
          "Dimension 1",
          gp = gpar(fontsize = 19)
        )
      )
      dev.off()
      
    }
    
  } else if(figure_type == "separate"){
    
    invisible(lapply(1:length(gg_list_all), function(i){
      
      invisible(lapply(1:length(gg_list_all[[i]]), function(j){
        
        tiff(file.path(path, paste(paste("SeparatePanels_AcousticPolygons", names(gg_list_all)[i], names(gg_list_all[[i]])[j], sep = "_"), ".tiff", sep = "")), width = img_width, height = img_height, units = "in", res = 300)
        print(gg_list_all[[i]][[j]])
        dev.off()
        
      }))
      
    }))
    
    
  }
  
  
}


### A generalized function for spatial comparisons

make95PolyPlots_spatial <- function(df, polys, years, cols, group_col, pttrn = c("circle", "crosshatch"), dens = c(0.4, 0.05), pszs = c(0.5, 0.20), fills = c(alpha('white', 0), alpha('gray', 0.5)), path, img_file_nm, img_width, img_height){
  
  # Convert the list of polygons to a SpatialPolygonsDataFrame
  # First merge the polygons together into a single SpatialPolygons object
  polys_spdf <- do.call(rbind, polys)
  # str(polys_spdf)
  
  # Add back the group column to the SpatialPolygonsDataFrame as an ID slot
  polys_spdf[[group_col]] <- getSpPPolygonsIDSlots(polys_spdf)
  
  yrs_ord <- as.character(unique(unlist(years)[order(unlist(years), decreasing = FALSE)]))
  # yrs_ord
  
  # Also use the fortified data frame to make plots
  ggpoly <- fortify(polys_spdf) %>%
    dplyr::mutate(
      id = factor(id, levels = levels(df[[group_col]])),
      spatial_region = gsub("_([0-9]+)", "", id),
      year = factor(gsub("([A-Z]+[a-z]+)_", "", id), levels = yrs_ord)
    )
  
  # Spatial coordinates
  bufx <- -min(ggpoly$long)
  bufy <- 0.07
  coord.x <- min(ggpoly$long) + bufx
  coord.y <- max(ggpoly$lat) + bufy
  
  # Make patterned polygons by geographic region
  ggplot() + 
    geom_polygon_pattern(data = ggpoly, aes(x = long, y = lat, group = group, pattern = spatial_region, pattern_density = spatial_region, pattern_size = spatial_region, fill = spatial_region), stat = "identity", colour = 'black', pattern_aspect_ratio = 1, pattern_angle = 30, linewidth = 0.45) +
    scale_pattern_manual(values = pttrn) +
    scale_pattern_density_manual(values = dens) +
    scale_pattern_size_manual(values = pszs) +
    scale_fill_manual(values = fills) +
    scale_y_continuous(limits = c(min(ggpoly$lat), max(ggpoly$lat) + 0.05)) +
    xlab("MDS Dimension 1") +
    ylab("MDS Dimension 2") +
    guides(
      fill = guide_legend(title = "Historical Dialect Region", nrow = 1), 
      pattern = guide_legend(title = "Historical Dialect Region", nrow = 1), 
      pattern_size = guide_legend(title = "Historical Dialect Region", nrow = 1),
      pattern_density = guide_legend(title = "Historical Dialect Region", nrow = 1, override.aes = list(pattern_spacing = 0.15))
    ) +
    facet_wrap(~ year, scales = "fixed", nrow = 1) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 10), 
      axis.text.x = element_text(size = 9.5), 
      strip.text = element_text(size = 12, margin = ggplot2::margin(0.25, 0, 0.25, 0, "line"), face = "bold"), 
      axis.title = element_text(size = 12), 
      plot.margin = unit(rep(0.25, 4), "line"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.direction = "horizontal",
      legend.position = "top",
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(3, "lines")
    )
  
  ggsave(file.path(path, img_file_nm), width = img_width, height = img_height, units = "in", dpi = 300)
  
}
