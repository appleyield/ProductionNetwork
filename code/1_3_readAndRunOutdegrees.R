library(dplyr)
library(purrr)


readrun_matrices <- function(country, years, bs){
  source("/media/lukas/77F9-5B63/Studium/Biophysical Production Network/Daten/IO-Deutschland/1_1_nestings_CCDFs.R")
  
  source("/media/lukas/77F9-5B63/Studium/Biophysical Production Network/Daten/IO-Deutschland/1_2_estimations_outdegree_smaller.R")
  
  
  if (country == "US"){
    setwd("/media/lukas/77F9-5B63/Studium/Biophysical Production Network/Daten/ReplicatingAcemoglu2012")
    
    CC <- c("CC72.csv", "CC77.csv", "CC82.csv", "CC87.csv", "CC92.csv", "CC97.csv", "CC02.csv")%>% 
      map(read.table, header=FALSE, sep=",")
    
    names(CC) <- c(1972, 1977, 1982, 1987, 1992, 1997, 2002)
    
    OutdegreesCCDF_nested <- CC %>% nesting_normalisingCCDFs(1,1,0, "outdegree")
    
    #OutdegreesCCDF_nested_0297 <- OutdegreesCCDF_nested %>% filter(year %in% c(1997,2002))
    OutdegreesCCDF_nested_02 <- OutdegreesCCDF_nested %>% filter(year==years)
    
    # bs = TRUE activates bootstrapping GoF p-values of distributions 
    Result_tables <- estimate_outdegree(OutdegreesCCDF_nested_02, bs)
  }
  
  if (country == "DE"){
    setwd("/media/lukas/77F9-5B63/Studium/Biophysical Production Network/Daten/IO-Deutschland")
    
    W <- c("W2015.csv", "W2016.csv", "W2017.csv")%>% 
      map(read.table, header=FALSE, sep=";")
    
    names(W) <- c(2015, 2016, 2017)
    
    OutdegreesCCDF_nested <- W %>% nesting_normalisingCCDFs(0,0,1, "outdegree")
    
    OutdegreesCCDF_nested_subperiod <- OutdegreesCCDF_nested %>% filter(year==years)
    
    # bs = TRUE activates bootstrapping GoF p-values of distributions 
    Result_tables <- estimate_outdegree(OutdegreesCCDF_nested_subperiod, bs)
    
  }
  
  return(list(Result_tables, OutdegreesCCDF_nested))
  
} 


