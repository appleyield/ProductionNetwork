library(dplyr)
library(purrr)


readrun_matrices <- function(country, distribution, order, years){
  source("./code/1_1_nestings_CCDFs.R")
  
  
  if (country == "US"){
    orig_wd <- getwd()
    on.exit(setwd(orig_wd))
    setwd("./data/US")
    
    CC <- c("CC72.csv", "CC77.csv", "CC82.csv", "CC87.csv", "CC92.csv", "CC97.csv", "CC02.csv")%>% 
      map(read.table, header=FALSE, sep=",")
    
    #names(CC) <- c(1972, 1977, 1982, 1987, 1992, 1997, 2002)
    names(CC) <- years
    
    OutdegreesCCDF_nested <- CC %>% nesting_normalisingCCDFs(1,1,0, distribution)
    
    'if (distribution == "shock" | distribution == "forwardlinkage"){
      library(readr)
      
      # also read US and German output shares here
      # for Germany needs total output in data
      X2015 <- read_delim("X2015.csv", delim = ";", col_names = FALSE)
    }'
    
    
  }
  
  if (country == "DE"){
    orig_wd <- getwd()
    on.exit(setwd(orig_wd))
    setwd("./data/German")
    
    W <- c("W2015.csv", "W2016.csv", "W2017.csv")%>% 
      map(read.table, header=FALSE, sep=";")
    
    if (order==2){
      for (i in 1:length(W)){
        W[[i]] <- as.matrix(W[[i]]) %*% as.matrix(W[[i]])
      }
    }
    
    names(W) <- c(2015, 2016, 2017)
    
    OutdegreesCCDF_nested <- W %>% nesting_normalisingCCDFs(0,0,1, distribution)
    
    if (distribution == "shock" | distribution == "forwardlinkage"){
      library(readr)
      
      # also read US and German output shares here
      # for Germany needs total output in data
      X2015 <- read_delim("X2015.csv", delim = ";", col_names = FALSE)
      
      OutdegreesCCDF_nested <- lst(OutdegreesCCDF_nested, X2015)
    }
    
    
  }
  
  return(OutdegreesCCDF_nested)
  
} 


