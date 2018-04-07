

#input <- data

snl_summary <- function(input, type = "overall", exclude.inactive = FALSE){
  
  if (type == "overall"){
    
    nrow <- unique(data$commodity) %>% length()
    ncol <- unique(data$year) %>% length()*2 + 2
    
    S <- matrix(rep(NA, nrow*ncol), 
                nrow = nrow,
                ncol = ncol, 
                byrow = TRUE)
    
    S_yrs <- sort(unique(data$year))
    S_sum_rep <- c("perc", "sum")
    S_sum_rep_yrs <- apply(expand.grid(S_sum_rep, S_yrs), 1, paste, collapse = "_reported_")
    S_names <- c("commodity", "nr_mines", S_sum_rep_yrs)
    
    if (exclude.inactive == FALSE){
      
      for (com in 1: (unique(input$commodity) %>% length())){
        
        S[com, 1] <- unique(input$commodity)[com]

        for (yr in 1: (unique(data$year) %>% length())) {
          
          dat_yr <- base::subset(input, year == sort(unique(data$year))[yr] & commodity == unique(input$commodity)[com])
          dat_yr <- dat_yr[grep(unique(input$commodity)[com], dat_yr$list_of_commodities),]
          
          # number of mines is not changing over the years, as SNL does only provide one full list of registered mines
          if (yr == 1) {S[com, 2] <- length(dat_yr$mine)} 
          
          perc <- base::subset(dat_yr, commodity == unique(input$commodity)[com] & production > 0)$mine %>% length() / length(dat_yr$mine) * 100
          perc <- paste0(round(perc, 0), "%")
          sum_rep <- sum(dat_yr$production, na.rm = TRUE)
          
          S[com, 2 + (2*yr - 1)] <- perc
          S[com, 3 + (2*yr - 1)] <- sum_rep
          
        }
          
        }

      output <- data.frame(S)
      colnames(output) <- S_names
      return(output)
      
    } else {
      
    }
    
  } else if (type == "operating_status") {
    
  } else if (type == "primary_commodity") {
    
  } else if (type == "country") {
    
  }
    
  
}



# # ### summarize obtained SNL data
# 
# # all mines
# commodity_name <- c()
# nr_mines <- c()
# perc_reported_2005 <- c()
# sum_reported_2005 <- c()
# perc_reported_2010 <- c()
# sum_reported_2010 <- c()
# perc_reported_2017 <- c()
# sum_reported_2017 <- c()
# 
# for (com in unique(data$commodity)){
#   commodity_name <- c(commodity_name, com)
# 
#   dat2005 <- base::subset(data, year == "2005" & commodity == com)
#   dat2005 <- dat2005[grep(com, dat2005$list_of_commodities),]
#   nr_mines <- c(nr_mines, length(dat2005$mine)) # number of mines is not changing over the years, as SNL does only provide one full list of registered mines
#   perc <- base::subset(dat2005, commodity == com & production > 0)$mine %>% length() / length(dat2005$mine) * 100
#   perc <- paste0(round(perc, 0), "%")
#   perc_reported_2005 <- c(perc_reported_2005, perc)
#   sum_reported_2005 <- c(sum_reported_2005, sum(dat2005$production, na.rm = TRUE))
# 
#   dat2010 <- base::subset(data, year == "2010" & commodity == com)
#   dat2010 <- dat2010[grep(com, dat2010$list_of_commodities),]
#   perc <- base::subset(dat2010, commodity == com & production > 0)$mine %>% length() / length(dat2005$mine) * 100
#   perc <- paste0(round(perc, 0), "%")
#   perc_reported_2010 <- c(perc_reported_2010, perc)
#   sum_reported_2010 <- c(sum_reported_2010, sum(dat2010[dat2010$commodity == com,]$production, na.rm = TRUE))
# 
#   dat2017 <- base::subset(data, year == "2017" & commodity == com)
#   dat2017 <- dat2017[grep(com, dat2017$list_of_commodities),]
#   perc <- base::subset(dat2017, commodity == com & production > 0)$mine %>% length() / length(dat2005$mine) * 100
#   perc <- paste0(round(perc, 0), "%")
#   perc_reported_2017 <- c(perc_reported_2017, perc)
#   sum_reported_2017 <- c(sum_reported_2017, sum(dat2017[dat2017$commodity == com,]$production, na.rm = TRUE))
# 
# }
# 
# sum_production <- data.frame(commodity_name,
#                              nr_mines,perc_reported_2005,sum_reported_2005,
#                              perc_reported_2010,sum_reported_2010,
#                              perc_reported_2017,sum_reported_2017)
# 
# 
# 
# # excluding inactive mines
# commodity_name <- c()
# nr_mines <- c()
# perc_reported_2005 <- c()
# sum_reported_2005 <- c()
# perc_reported_2010 <- c()
# sum_reported_2010 <- c()
# perc_reported_2017 <- c()
# sum_reported_2017 <- c()
# 
# for (com in unique(data$commodity)){
#   commodity_name <- c(commodity_name, com)
# 
#   dat2005 <- base::subset(data, year == "2005" & commodity == com & operating_status != "Inactive")
#   dat2005 <- dat2005[grep(com, dat2005$list_of_commodities),]
#   nr_mines <- c(nr_mines, length(dat2005$mine)) # number of mines is not changing over the years, as SNL does only provide one full list of registered mines
#   perc <- base::subset(dat2005, commodity == com & production > 0)$mine %>% length() / length(dat2005$mine) * 100
#   perc <- paste0(round(perc, 0), "%")
#   perc_reported_2005 <- c(perc_reported_2005, perc)
#   sum_reported_2005 <- c(sum_reported_2005, sum(dat2005$production, na.rm = TRUE))
# 
#   dat2010 <- base::subset(data, year == "2010" & commodity == com & operating_status != "Inactive")
#   dat2010 <- dat2010[grep(com, dat2010$list_of_commodities),]
#   perc <- base::subset(dat2010, commodity == com & production > 0)$mine %>% length() / length(dat2005$mine) * 100
#   perc <- paste0(round(perc, 0), "%")
#   perc_reported_2010 <- c(perc_reported_2010, perc)
#   sum_reported_2010 <- c(sum_reported_2010, sum(dat2010[dat2010$commodity == com,]$production, na.rm = TRUE))
# 
#   dat2017 <- base::subset(data, year == "2017" & commodity == com & operating_status != "Inactive")
#   dat2017 <- dat2017[grep(com, dat2017$list_of_commodities),]
#   perc <- base::subset(dat2017, commodity == com & production > 0)$mine %>% length() / length(dat2005$mine) * 100
#   perc <- paste0(round(perc, 0), "%")
#   perc_reported_2017 <- c(perc_reported_2017, perc)
#   sum_reported_2017 <- c(sum_reported_2017, sum(dat2017[dat2017$commodity == com,]$production, na.rm = TRUE))
# 
# }
# 
# sum_production_ex_inactive <- data.frame(commodity_name,
#                                          nr_mines,perc_reported_2005,sum_reported_2005,
#                                          perc_reported_2010,sum_reported_2010,
#                                          perc_reported_2017,sum_reported_2017)
# 
# 
# ### some more summary tables, each commodity separately
# 
# # reduce data to cases where reported commodity is actually among the mines' list of commodities
# ldata <- split(data, data$commodity)
# for (com in unique(data$commodity)){
#   ldata[[com]] <- ldata[[com]][grep(com, ldata[[com]]$list_of_commodities),]
# }
# data <- do.call("rbind", ldata)
# 
# # operating_status
# lst <- lapply(split(data, data$commodity), function(x) count(x, year, operating_status) %>% ungroup())
# lst <- lapply(lst, function(y) y = as.data.frame(y)) # convert data into data.frames
# lst <- lapply(lst, base::subset, year == "2005") # dplyr::select only one year (as same numbers are reported for all years)
# lst <- lapply(lst, dplyr::select, -year)
# for (nm in names(lst)){
#   colnames(lst[[nm]]) <- c("operating_status", nm)
# }
# sum_operating_status <- lst %>%
#   Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="operating_status"), .)
# 
# # primary_commodity
# lst <- lapply(split(data, data$commodity), function(x) count(x, year, primary_commodity) %>% ungroup())
# lst <- lapply(lst, function(y) y = as.data.frame(y)) # convert data into data.frames
# lst <- lapply(lst, base::subset, year == "2005") # dplyr::select only one year (as same numbers are reported for all years)
# lst <- lapply(lst, dplyr::select, -year)
# for (nm in names(lst)){
#   colnames(lst[[nm]]) <- c("primary_commodity", nm)
# }
# sum_primary_commodity <- lst %>%
#   Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="primary_commodity"), .)
# 
# # country
# lst <- lapply(split(data, data$commodity), function(x) count(x, year, country) %>% ungroup())
# lst <- lapply(lst, function(y) y = as.data.frame(y)) # convert data into data.frames
# lst <- lapply(lst, base::subset, year == "2005") # dplyr::select only one year (as same numbers are reported for all years)
# lst <- lapply(lst, dplyr::select, -year)
# for (nm in names(lst)){
#   colnames(lst[[nm]]) <- c("country", nm)
# }
# sum_country <- lst %>%
#   Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="country"), .)
# 
# 
