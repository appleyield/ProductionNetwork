#----------------------------------------
#Calculate technical coefficient matrix A
#----------------------------------------

setwd("./data/German")

## 1) Read data.
#---------------
## Input: Excel IO-tables 2015-2017; Output: IO matrix Z 2015-2017, total output X 2015-2017, sector names


# how to verify that reading is correct?
# read some checkdata with a different function than data
library(readxl)
checkdata1_rowsums = list()
checkdata1_rowsums[[1]] <- read_xlsx("CheckdataRowsums2015.xlsx", col_names = FALSE)
checkdata1_rowsums[[2]] <- read_xlsx("CheckdataRowsums2016.xlsx", col_names = FALSE)
checkdata1_rowsums[[3]] <- read_xlsx("CheckdataRowsums2017.xlsx", col_names = FALSE)

## In: IO-tables 2015-2017
## Out: lists 2015-2017 of: Z[ij] oriented j --> i, without zero rows and columns;
# X (total outputs) ; Names_sectors 
library(readr)
year0 = 2015
Z <- list()
X <- list()
Names_sectors <- list()

source("/media/lukas/77F9-5B63/Studium/Biophysical Production Network/Daten/IO-Deutschland/1_1_nestings_CCDFs.R")
# external function deleteZeros is used instead of Zeros
Zeros <- function(Z){
  z = Z
  j = which(colSums(z) == 0)
  i = which(rowSums(z) == 0)
  z = z[,-j]
  z = z[-i,]
  return(list(z, j))
}

# following sums are calculated from the CSV data in Excel (Libre Office Calc)
Z_Excelsums <- c(2336021, 2386666, 2514255)
Z_Excelmeans <- c(676, 693, 728)
X_sums = c(5718361, 5872792, 6164689)
for (i in 1:3){
  year = year0 + (i-1)
  Z_raw <- read_csv2(paste("IO_", paste(year, "_Revision2019-ohneTitelzeile.csv", sep = ""), sep = ""))
  
  # extract sector names
  # column 81 is "Input der Produktionsbereiche zusammen" 
  # which is the sum of each industries sales to other industries, matrix Z until column 80 
  Z_raw[7,75]
  Names_sectors[[i]] = Z_raw[9:80,2]
  
  Z_df <- Z_raw[9:80, 3:74]
  
  Z_df_noNA <- data.frame(lapply(Z_df, function(x) {gsub("-", 0, x)}), stringsAsFactors = FALSE) 

  # Check whether variables in Z_df_noNA are all character 
  if(all(sapply(Z_df_noNA , is.character))){
  }
  else{
    print("Z_df_noNA not all character")
    break
  }
  
  # Two ways to make Z numeric
  # 1
  Z_dfc <- sapply(Z_df_noNA , as.character) # data.matrix needs this step! wrong numbers otherwise
  Z_matrix <- data.matrix(Z_dfc) 
  zet1 <- matrix(as.numeric(Z_matrix), 72,72) 
  
  #2
  Z_df_numeric <- sapply(Z_df_noNA, as.numeric)
  zet2 <- as.matrix(Z_df_numeric)
  
  # check whether rowSums(zet2) equal Checkdata1_rowsums and zet2 sum and mean equal excel calculated sum and mean
  if(all(zet1==zet2)){
    if(isTRUE(all.equal(Z_Excelsums[i], sum(zet2)) & isTRUE(all.equal(Z_Excelmeans[i], round(mean(zet2[-which(zet2==0)])))) & isTRUE(all.equal(rowSums(zet2), unname(unlist(checkdata1_rowsums[[i]])))))){
      zet <- t(zet2)  # Z is transposed here, such that sales run from columns to rows
    }else{
      print("Excel check values and data values not equal")
      break
    }
  }else{print("zet1 != zet2")}
  
  #z = Zeros(zet) # throw out zero rows and columns
  z = deleteZeros(zet)
  if(isTRUE(all.equal(Z_Excelsums[i], sum(z[[1]])))){
    Z[[i]] = z[[1]]
  }else{
    print("Excelsums and sum(z) not equal")
  }
  
  
  # column 88 is "Gesamte Verwendung von Gütern"
  # which is the total output y of each sector
  Z_raw[7,88]
  x = as.numeric(as.character(Z_raw[[88]][9:80])) # how to subset tibble: https://github.com/tidyverse/dplyr/issues/2625
  
  if (isTRUE(all.equal(sum(x), X_sums[i]))){
    X[[i]] = x[-z[[2]]]
  }else{
    print("Excelsum != sum(X)")
    break
  }
  
  rm(zet1,zet2,Z_raw,Z_df,Z_df_noNA, Z_df_numeric, Z_dfc,Z_matrix)
}


setwd("./data/German")
for (i in 1:3){
  year = year0 + (i-1)
  write.table(X[[i]], paste("X", paste(year, ".csv", sep = ""), sep = ""), sep = ";", row.names = FALSE, col.names = FALSE)
}



##-----------------------------------------------
## sector 72 has no connection to rest of network
Names_sectors[[1]][72,]
# sector 72: Waren u.Dienstleistungen privater Haushalte o.a.S.
X[[1]][72]
# But it has total output 6827 in 2015
X[[1]][72] / sum(X[[1]])
# output share of sector 72
min(X[[1]] / sum(X[[1]]))
max(X[[1]] / sum(X[[1]]))
sort(X[[1]] / sum(X[[1]]))
# output share of sectr 72 is 6th smallest 
quantile(X[[1]] / sum(X[[1]]))
#------------------------------------------------


## self-loops
length(which(diag(Z[[1]]) > 0))



## 2) Compute technical coefficient matrix W
#-------------------------------------------
#### Z[ij] is oriented i --> j
#### this is orientation W[ij], from j to i, j is input supplier to i. Same orientation as in Acemoglu 2012
#### and this is downstream propagation

######## 
#test how R calculates matrix division  
#m = matrix(c(1,2,3,4), 2,2)
#t(m)
#v = c(3,5)
#m / v
#t(m) / v
# m[i,j] / v[i]

#a_t = t(m) /v
#a = t(a_t)
########

###
# Notation follows Acemoglu (2012)
# W is the Acemoglus downstream direct requirements matrix and the transposed usual 
# upstream direct requirements matrix (Torres-Gonzales)
# A = t(W)  (Torres-Gonzales)


# normalization and shorter way lead to same result!
# to check with small matrix:
#Z[[1]] = matrix(c(3, 4, 7, 5), 2, 2)
#X[[1]] = c(12, 11)

W <- list()
A <- list()
for (i in 1:3){
  w_longway = Z[[i]] / X[[i]] # Z_ij from j to i.  Z[j-->i] / X[i]
  
  # Acemoglus normalisation:
  dimension = dim(Z[[i]])
  w_longway_normalised <- matrix(NA, dimension[1], dimension[2])
  RS <- rowSums(w_longway) 
  for (j in 1:length(RS)){
    if (RS[j] > 0){
      w_longway_normalised[j,] <- w_longway[j,] / RS[j]
    }
    else if (RS[j] == 0){
      w_longway_normalised[j,] <- w_longway[j,]
    }
  }
  
  # unname Z to remove dimnames (check with attribute(Z[[1]])), otherwise check doesnt work
  w_shortway = unname(Z[[i]]) / rowSums(Z[[i]])
  
  if (isTRUE(all.equal(w_longway_normalised, w_shortway))){
    W[[i]] = w_longway_normalised
    A[[i]] <- w_longway
  }else{
    print("ERROR: longway != shortway")
    break
  }
}




setwd("./data/German")
for (i in 1:3){
  year = year0 + (i-1)
  write.table(A[[i]], paste("A", paste(year, ".csv", sep = ""), sep = ""), sep = ";", row.names = FALSE, col.names = FALSE)
}



## 3) Check matrix orientation.
#-------------------------------
# colsums is equivalent to sum(matrix) in matlab (sum over i)
# if W had same orientation as in Acemoglu, outdegrees come out with rowsums, as Acemoglu does sum(X') = outdegree
# w_ij "sector j is an input supplier to sector i" (Acemoglu et al. 2012, p. 1985)

K_out_check_W <- list()
K_out_check_W_nonnormalized <- list()
for (i in 1:3){
  K_out_check_W[[i]] <- colSums(W[[i]])
  K_out_check_W_nonnormalized[[i]] <- colSums(W_nonnormalized[[i]])
}

hist(colSums(W[[1]])) # outdegree
hist(colSums(W[[2]]))
hist(colSums(W[[3]]))
hist(rowSums(W[[1]])) # indegree
# see Newman, for j --> i outdegree is sum of each column; check looks correct




## 4) Write matrix W and total output Y.
#--------------------------------------

save(K_out_check_W_nonnormalized, file = "K_out_check_W_nonnormalized.RData")
save(K_out_check_W, file = "K_out_check_W.RData") # save outdegree sequence as a check for subsequent data reads
# use .RData instead of .csv to prevent joint unintended data transformations in reading when both data and checkdata is .csv

for (i in 1:3){
  year = year0 + (i-1)
  #write.table(X[[i]], paste("X", paste(year, ".csv", sep = ""), sep = ""), sep = ";", row.names = FALSE, col.names = FALSE)
  write.table(W[[i]], paste("W", paste(year, ".csv", sep = ""), sep = ""), sep = ";", row.names = FALSE, col.names = FALSE)
  write.table(W_nonnormalized[[i]], paste("W_nonnormalized", paste(year, ".csv", sep = ""), sep = ""), sep = ";", row.names = FALSE, col.names = FALSE)
}




## 5) write sector names
#-----------------------
for (i in 1:3){
  year = year0 + (i-1)
  write.table(Names_sectors[[i]], paste("Names_sectors", paste(year, ".csv", sep = ""), sep = ""), sep = ";", row.names = FALSE, col.names = FALSE)
}

