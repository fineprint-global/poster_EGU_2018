#' @title Tidy SNL Excel
#' 
#' @description This function brings SNL Excel as obtained from screener tool download
#' into tidy format
#' 
#' @param input a data.frame object obtained by importing SNL excel
#' 
#' @param type a character string with the variable of interest, which should be rearranged
#' into long format.
#' Only useful value up to now (and hence default value) is "production"
#' Can be extended by e.g. ore grades
#' 
#' @param meas.unit A character. Denotes which unit of measurement shall be added in the 
#' additional column "production_unit".
#' Useful values are "tons", "oz" (to be extended if needed). 
#' Default is "none" setting the unit as NA but returning a warning message.
#' 
#' @param freq A character. Setting the periodicity of the imported data set. 
#' Only useful value up to now (and hence default value) is "annual". 
#' To be extended by "quarterly", "daily", etc. if needed.
#' 
#' @return The function returns a tidy data.frame object
#' 
#' @example 
#' data_raw <- readxl::read_excel("test_data.xls", na = c("na", "Na", "NA"))
#' snl_tidy(data_raw, meas.unit = "tons")
#' 
#' @author Sebastian Luckeneder, \email{sebastian.luckeneder@@wu.ac.at}
#' 
snl_tidy <- function(input, type="production", meas.unit = "none", freq="annual"){
  
  require(tidyverse)
  
  if (type == "production" & freq == "annual"){
    
    # prepare column names
    col <- input[c(2,3),] %>% select_if(~sum(!is.na(.)) > 0)
    yr <- unlist(col[1,]) %>% substr(1, 4)
    com <- unlist(col[2,])
    com <- gsub("^.*\\|","", com)
    col <- paste(com, yr, sep = "_")
    colnames(input)[grep(pattern = "Production", x = colnames(input))] <- col
    
    # delete first rows
    input <- input[-c(1:3),]
    
    # tidy data frame
    production_col <- paste("_", 1900:2050, sep = "", collapse = "|")
    input <- input %>%  
      tidyr::gather(commodity_and_year, production, dplyr::matches(production_col)) %>% 
      tidyr::separate(commodity_and_year, c("commodity", "year"), "_")
    
    # production as.numeric
    input$production <- as.numeric(input$production)
    
    # add further variables
    if(meas.unit == "none") {
      
      input$production_unit <- NA
      warning("unit of commodity measurement missing")
      
    } else {
      
      if(meas.unit == "tons") {
        
        input$production_unit <- "tons"
        
      } else if (meas.unit == "oz") {
        
        input$production_unit <- "oz"
        
      } else {
        
        input$production_unit <- meas.unit
        warning("unknown unit of commodity measurement")
        
      }
      
    }
    
    # apply concordance tables on variable names
    country <- "SNL"
    filename <- "concordance_variables.csv"
    path <- "./input"
    concordance_variables <- read.csv(file.path(path, filename), sep = ";")
    concordance_variables <- concordance_variables[concordance_variables$country %in% country,]
    concordance_variables$in_variable <- gsub("\\\\r\\\\n", "\\\r\\\n", concordance_variables$in_variable)
    
    mm <- match(colnames(input), concordance_variables$in_variable)
    colnames(input)[!is.na(mm)] <- as.character(concordance_variables$out_variable[na.omit(mm)])
    
    input$lat <- as.numeric(input$lat)
    input$long <- as.numeric(input$long)
    
    
    return(input)

  } else { cat("\nNOT YET IMPLEMENTED\n") }
  
}


