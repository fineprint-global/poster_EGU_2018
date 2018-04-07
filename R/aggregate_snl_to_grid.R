#' @title Aggregate SNL points to grid cells 
#' 
#' @description This function aggregates SNL data to grid cells 
#' based on a given raster grid
#' 
#' @param sf_tbl a simple feature (sf) data tibble with the SNL data. 
#' The sf_tbl object must have at least two attributes, one named 
#' 'commodity' and the second named 'production'
#' 
#' @param r a raster object
#' 
#' @return The function returns a simple feature object with 
#' attributes aggregated by grid cell
#' 
#' @author Victor Maus, \email{victor.maus@@wu.ac.at}
#' 
aggregate_snl_to_grid <- function(sf_tbl, r){
  
  # Create grid for SNL coordinates 
  grid_cells <- create_grid_cell(sf_tbl, r) 
  
  # Aggregate SNL points to grid SPAM grid 
  res <- sf_tbl %>% 
    dplyr::select(commodity, production) %>% 
    dplyr::mutate(ID = row_number()) %>%
    tidyr::spread(commodity, production, fill = 0) %>% 
    stats::aggregate(by = grid_cells, FUN = sum, na.rm = TRUE, join = sf::st_intersects)
  
  return(res)
  
}