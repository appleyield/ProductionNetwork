### functions for creating nested dataframe with CCDFs

# version: 2021-10-24


library(tidyr)
library(dplyr)
library(purrr)

# Input: list of matrices W, normalisation: Yes/No = 1/0, delete zeros: Y/N = 1/0,
# orientationjTOi = j to i (1), i to j (0) 
# Orientation of W is sales from j to i, thus outdegree is colSums
# US CC would have to be transposed first
# Output: nested dataframe with CCDFs for every year
nesting_normalisingCCDFs <- function(W, normalisationYN, deleteZerosYN, orientationjTOi, degreetype){
  
  if(orientationjTOi == 0){
    W_oriented <- W %>% map(t)
  }else{W_oriented <- W}
  
  if(deleteZerosYN == 1){
    W_noZ <- W_oriented %>% map(deleteZeros) %>% map(.x =., .f = ~.x[[1]])
  }else{W_noZ <- W}
  
  
 if(normalisationYN == 1){
    W_norm <- W_noZ %>% map(normalise_W)
 }else{W_norm <- W_noZ}
  
  
  if (degreetype == "outdegree"){
    # calculate outdegrees and put the in nested tibble
    CCDF_nested <- W_norm %>% map(colSums) %>% nestingCCDFs()
  } else if (degreetype == "indegree"){
    CCDF_nested <- W_norm %>% map(rowSums) %>% map(unname)
  } else if (degreetype == "shock" | degreetype == "forwardlinkage"){
    CCDF_nested <- W_norm
  }
  
    
  return(CCDF_nested)
  
}




# only delete industries which have neither outgoing nor incoming edges
deleteZeros <- function(z){
  i <- which(colSums(z) == 0)
  j <- which(rowSums(z) == 0)
  if (length(i)>0 & length(j)>0){
    o <- which(i %in% j)
    if (length(o)>0){
      r <- i[o]
      z_new <- z[-r,-r]
    }else{
      r <- 0
      z_new <- z
    }
    
    
  }else{
    z_new <- z
    r = 0
    }
  
  return(list(z_new, r))
}




normalise_W <- function(W_nonnormalized){
  
  dimensions <- dim(W_nonnormalized)
  W_normalised <- matrix(NA, dimensions[1], dimensions[2])
  RS <- rowSums(W_nonnormalized) 
  for (i in 1:length(RS)){
    if (RS[i] > 0){
      W_normalised[i,] <- W_nonnormalized[i,] / RS[i]
    }
    else if (RS[i] == 0){
      W_normalised[i,] <- W_nonnormalized[i,]
    }
  }
  
  return(W_normalised)
}




nestingCCDFs <- function(outdegrees){
  
  WO <- outdegrees %>% map2(names(.), ., ~tibble(year=.x, outdegree = .y )) %>% bind_rows()
  
  WO_nested <- WO %>% group_by(year)  %>% nest(outdCCDF =c(outdegree)) 
  
  # add column for count of outdegrees/sectors
  WO_nested <- WO_nested %>% ungroup() %>% 
    map(.x = .$outdCCDF, .f = ~length(.x$outdegree)) %>%
    unlist() %>% mutate(ungroup(WO_nested), N=.)
  
  # sort outdegrees in decreasing order (COUNTERcumulative density)
  WO_nested <- WO_nested %>% 
    map(.x = .$outdCCDF, .f = ~ mutate(.x, outdegree = sort(.x$outdegree, decreasing=TRUE))) %>% mutate(WO_nested, outdCCDF = .)
  
  # good to know mutate with existing column name overwrites column
  
  # add column with CCDF in nested dataframe
  WO_nested <- WO_nested %>% 
    map2(.x = .$outdCCDF, .y = .$N, .f = ~ mutate(.x, CCDF = calcCCDF(.y))) %>%
    mutate(WO_nested, outdCCDF = .)
  
}


# CCDF is calculated as rank(k)/n
calcCCDF <- function(N){
  return(1:N/N)
}