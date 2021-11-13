library(dplyr)
library(purrr)


readrun_matrices <- function(country, distribution){
  source("/media/lukas/77F9-5B63/Studium/Biophysical Production Network/Daten/IO-Deutschland/1_1_nestings_CCDFs.R")
  
  
  if (country == "US"){
    setwd("/media/lukas/77F9-5B63/Studium/Biophysical Production Network/Daten/ReplicatingAcemoglu2012")
    
    CC <- c("CC72.csv", "CC77.csv", "CC82.csv", "CC87.csv", "CC92.csv", "CC97.csv", "CC02.csv")%>% 
      map(read.table, header=FALSE, sep=",")
    
    names(CC) <- c(1972, 1977, 1982, 1987, 1992, 1997, 2002)
    
    OutdegreesCCDF_nested <- CC %>% nesting_normalisingCCDFs(1,1,0, distribution)
    
    if (distribution == "shock" | "forwardlinkage"){
      library(readr)
      
      # also read US and German output shares here
      # for Germany needs total output in data
      X2015 <- read_delim("./data/German/X2015.csv", delim = ";", col_names = FALSE)
    }
    
    
  }
  
  if (country == "DE"){
    setwd("/media/lukas/77F9-5B63/Studium/Biophysical Production Network/Daten/IO-Deutschland")
    
    W <- c("W2015.csv", "W2016.csv", "W2017.csv")%>% 
      map(read.table, header=FALSE, sep=";")
    
    names(W) <- c(2015, 2016, 2017)
    
    OutdegreesCCDF_nested <- W %>% nesting_normalisingCCDFs(0,0,1, distribution)
    
    if (distribution == "shock" | "forwardlinkage"){
      library(readr)
      
      # also read US and German output shares here
      # for Germany needs total output in data
      X2015 <- read_delim("./data/German/X2015.csv", delim = ";", col_names = FALSE)
      
      OutdegreesCCDF_nested <- lst(OutdegreesCCDF_nested, X2015)
    }
    
    
  }
  
  return(OutdegreesCCDF_nested)
  
} 


