library(dplyr)
library(purrr)


readrun_matrices <- function(country, years, bs){
  source("./code/1_1_nestings_CCDFs.R")
  
  source("./code/1_2_estimations_outdegree.R")
  
  
  if (country == "US"){
    setwd("./data/US")
    
    CC <- c("CC72.csv", "CC77.csv", "CC82.csv", "CC87.csv", "CC92.csv", "CC97.csv", "CC02.csv")%>% 
      map(read.table, header=FALSE, sep=",")
    
    names(CC) <- c(1972, 1977, 1982, 1987, 1992, 1997, 2002)
    
    OutdegreesCCDF_nested <- CC %>% nesting_normalisingCCDFs(0,1,0, "indegree")
    
    #OutdegreesCCDF_nested_0297 <- OutdegreesCCDF_nested %>% filter(year %in% c(1997,2002))
    #OutdegreesCCDF_nested_02 <- OutdegreesCCDF_nested %>% filter(year==years)
    
    # bs = TRUE activates bootstrapping GoF p-values of distributions 
    #Result_tables <- estimate_outdegree(OutdegreesCCDF_nested_02, bs)
  }
  
  if (country == "DE"){
    setwd("./data/German")
    
    W <- c("W_nonnormalized2015.csv", "W_nonnormalized2016.csv", "W_nonnormalized2017.csv")%>% 
      map(read.table, header=FALSE, sep=";")
    
    names(W) <- c(2015, 2016, 2017)
    
    OutdegreesCCDF_nested <- W %>% nesting_normalisingCCDFs(0,0,1, "indegree")
    
    #OutdegreesCCDF_nested_subperiod <- OutdegreesCCDF_nested %>% filter(year==years)
    
    # bs = TRUE activates bootstrapping GoF p-values of distributions 
    #Result_tables <- estimate_outdegree(OutdegreesCCDF_nested_subperiod, bs)
    
  }
  
  return(OutdegreesCCDF_nested)
  
} 


