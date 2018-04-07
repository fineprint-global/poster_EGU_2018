#' @title Rasterize SNL grid 
#' 
#' @description This function rasterize the grid to raster stack 
#'  
#' @param grid a sf grid polygons object 
#' 
#' @param r r Raster* object, for details see raster::rasterize 
#' 
#' @param fun a function to aggregate grid attributes , for details see raster::rasterize 
#' 
#' @param na.rm remove NA values, for details see raster::rasterize 
#' 
#' @param progress include progress bar, for details see raster::rasterize 
#' 
#' @return The function returns a raster stack including attributes from grid as layers 
#' 
#' @author Victor Maus, \email{victor.maus@@wu.ac.at}
#' 
snl_to_raster <- function(grid, r, attr, fun = 'first', na.rm = TRUE, progress = "text", ...){
  
  # Rasterize grid 
  r_grid <- lapply(attr, function(i){
    cat("\nRasterizing ", i, "layer\n")
    raster::rasterize(x = as(grid, "Spatial"), y = r, field = i, fun = fun, na.rm = na.rm, progress = progress, ...)
  }) %>% raster::brick()
  
  # Set names to raster layers 
  names(r_grid) <- attr
  
  return(r_grid)
  
}
