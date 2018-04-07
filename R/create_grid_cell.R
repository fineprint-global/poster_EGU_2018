#' @title Create grid cells 
#' 
#' @description This function creates grid cells exclusively over existing data points
#' 
#' @param sf_tbl a simple feature (sf) data tibble points
#' 
#' @param r a raster object 
#' 
#' @return The function returns a simple feature object with 
#' grid cells as polygons
#' 
#' @author Victor Maus, \email{victor.maus@@wu.ac.at}
#' 
create_grid_cell <- function(sf_tbl, r){
  
  # TODO: generalise to regional grid 
  
  # Get grid parameters from raster 
  dx <- res(r)[1]
  dy <- res(r)[2]
  
  # Get points coordinates 
  pto_coord <- sf_tbl %>% 
    sf::st_coordinates() %>% 
    tibble::as_tibble() 
  
  create_grid_pol <- function(x, y, dx, dy, .pb = NULL){
    
    if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
    
    sf::st_polygon(list(rbind(c(x     , y     ), 
                              c(x     , y + dy),
                              c(x + dx, y + dy),
                              c(x + dx, y     ),
                              c(x     , y     ))))
    
  }
  
  # Compute grid col and row (Integer division) and grid coordinates 
  cat("\nCreating grid from SNL points")
  grid_cells <- pto_coord %>% 
    dplyr::transmute(nx = X%/%dx, ny = Y%/%dy) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(gx = nx*dx, gy = ny*dy) %>% 
    dplyr::mutate(pol = purrr::map2(.x = .$gx, .y = .$gy, .f = create_grid_pol, dx = dx, dy = dy, .pb = dplyr::progress_estimated(length(.$gx)))) 
  
  res <- sf::st_sfc(grid_cells$pol, crs = sf::st_crs(sf_tbl)) %>% 
    sf::st_sf()

  return(res)
  
}

