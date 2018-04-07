#' @title Find the cutting value over which the top percentage is 
#' 
#' @description This function calculates a cutting value over which the top percentage is 
#'  
#' @param x a numeric vector 
#' 
#' @param p top cutting percentage 
#' 
#' @return This function returns a value from x 
#' 
#' @author Victor Maus, \email{victor.maus@@wu.ac.at}
#' 
get_cutting_value <- function(x, p, .pb = NULL, return_mask = TRUE, ...){
  
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  r <- raster::raster(x)

  # Get cutting value 
  v <- tibble::tibble(x = r[]) %>%
    dplyr::filter(!is.na(x)) %>%
    dplyr::arrange(desc(x)) %>%
    dplyr::mutate(acc.p = cumsum(x) / sum(x)) %>%
    dplyr::filter(acc.p <= p) %>%
    dplyr::slice(length(x)) %>%
    .$x
  
  return(v)
  
}



