setwd("./data/German")


# Input: normalized matrices W
# Output: list with shocks and tibble of different types of aggregate output


simulate_shocks <- function(){
  
  
  return()
}


year=2017
W <- read.table(paste("W", paste(year, ".csv", sep = ""), sep = ""), sep=";", header = FALSE)
I = diag(1, 71,71)
rowSums(W) # normalisierte matrix W hat rowSums all 1
alpha=0.3333  # Acemoglu (2012) use an alpha of 0.3333 for their influence vector v
L = solve(I-(1-alpha)*W)
# System ist für den Rechner singulär: reziproke Konditionszahl = 2.23797e-18




# also create large graph with realistic degree distribution with configuration model
# and then cluster this graph several times into smaller sized graphs [nice exercise :D]


# 1. create directed graph a la Newman chapter 12.11.1, then multiply weights?
# (first try an unweighted directed network)

# I need to define the degree k of edges beforehand the matching process
p = (k_i * k_j) / 2*m # probability for an edge between i and j (G(n,m) version)

# G(n,p) version 
p = (c_i * c_j) / 2*m  # j not i  
p = (c_i * c_j) / 4*m  # j == i


# estimate density of sequence
sq <- rnorm(100, 5, 3)
edf <- density(sq)

hist(sq)
line(edf)
plot(edf)
sample(edf, 10) 
#https://www.r-bloggers.com/2014/07/making-random-draws-from-an-arbitrarily-defined-pdf/
#https://stackoverflow.com/questions/28627487/how-do-i-sample-from-a-custom-distribution
#https://stats.stackexchange.com/questions/12843/generating-random-samples-from-a-custom-distribution



#--------------------
# CONFIGURATION Model
#--------------------
# draw degree sequence dseq from a fat tailed distribution

library(rlist)

degrees <- list()
As <- list()
N <- c(100,10^3)
for (l in 1:length(N)){
  n = N[l]
  k_in_seq <- c(1,2,3)
  while (length(k_in_seq) < n) {
    k_in_seq = round(rcauchy(1.015*n, 25, 1))
    k_in_seq =  k_in_seq[-which(k_in_seq <= 0 | k_in_seq >= n)]
  }
  k_out_seq = k_in_seq
  degrees[l] <- k_in_seq[1:n]
  
  As <- list.append(As, Stub_matching(n, k_in_seq, k_out_seq))  
}
names(As) <- N

As[1]

names(degrees) <- c("100", "1000", "10000")
names(As) <- c("100", "1000")


As$`100`






hist(k_in_seq)
sum(k_in_seq)
#k_out_seq = round(rcauchy(3*n, 25, 1))
#k_out_seq =  k_out_seq[-which(k_out_seq <= 25)]
hist(k_out_seq)
sum(k_out_seq)


Stub_matching <- function(n, indeg, outdeg){
  A <- diag(0, n, n)
  h = 0
  while (sum(k_in_seq[1:n]) > 0) {
    p_in = cumsum(k_in_seq[1:n])/sum(k_in_seq[1:n])
    p_out = cumsum(k_out_seq[1:n])/sum(k_out_seq[1:n])
    z_in = runif(1, 0, 1)
    z_out = runif(1,0,1)
    
    i <- min(which(p_in >= z_in))
    j <- min(which(p_out >= z_out))
    
    A[i,j] = 1
    k_in_seq[i] = k_in_seq[i] -1
    k_out_seq[j] = k_out_seq[j] -1
    
    h = h + 1
    if (h > 60000){
      break
    }
  }
  return(A)
}





As <- list()
As[1] <- A
As[2] <- A
As[3] <- A
names(As) <- c("100", "1000", "10000")


C <- list(a, matrix(1,1,1))
a <- matrix(1,5,5)
b <- matrix(1,3,7)
C[2] <- a
C[1]
library(rlist)
C <- list.append(C, b) # different object types can be appended with list.append() from rlist 


# store vectors in list
e<- list()
v <- c(1,3)
b <- c(8,9,7)
e <- list(v)
e <- list(e, list(b))
e <- list(v,b)
names(e) <- c("100", "1000")
e$`100`


which(is.na(A))

# degrees
deg1 = colSums(A)
deg2 = rowSums(A)
hist(deg1)
hist(deg2)


# random graph G(n,p)
library(igraph)
n = 100
g <- sample_gnp(n, p = 0.3, directed = TRUE)
A <- as_adjacency_matrix(g, sparse=FALSE)
I = diag(1, n, n)
L = solve(I-A)

#--------------------
# Community detection
#--------------------
# https://www.r-bloggers.com/2020/03/community-detection-with-louvain-and-infomap/
# https://www.r-bloggers.com/2012/06/summary-of-community-detection-algorithms-in-igraph-0-6/
# https://www.sixhat.net/finding-communities-in-networks-with-r-and-igraph.html

library(igraph)

g <- graph_from_adjacency_matrix(A, "directed", weighted = TRUE)

# Louvain algorithm

# Infomap algorithm

im <- cluster_infomap(g, modularity=FALSE)
communities(im) # all in the same community?
plot(im)

eb <- cluster_edge_betweenness(g)


# https://stats.stackexchange.com/questions/301466/how-to-detect-k-number-of-communities-in-a-weighted-graph-network
# https://stackoverflow.com/questions/38896546/setting-size-of-detected-communities-in-r/38899957#38899957
# "cut_at method simply gives you back the community indices for all nodes in the graph (i.e. a simple numeric vector)."

walk <- cluster_walktrap(g) 
walk[1]
cut_at(walk, no = 10) 

eb <- cluster_edge_betweenness(g)
eb[4]
eb_cut <- cut_at(eb, no = 10) 



#--------------------------------------
# one shock to each industry one period, structureless
#--------------------------------------
n= 72
n = 10^2
n = 5 * 10^2
n = 10^3
n = 4* 10^3
n = 10^4 # 10^4 is already to large for my memory
I = diag(1, n, n)
L = I # shock only on industry itself

out_agg <- c()
for (i in 1:1000){
  z_once <- rnorm(n, 0, 1)  #every industry receives a shock
  out = z_once %*% L
  out_agg[i] = sum((1/n)*out)
}

sd(out_agg)
hist(out_agg, breaks=1000)



#---------------------------------------
# one shock to one industry each period
#---------------------------------------

a = 1
n = dim(As[[a]])[1]
I = diag(1, n, n)
L = solve(I-As[[a]])
#n = dim(L)[1] 
 
n_shocks = 10^3
shocks = rnorm(n_shocks, 0, 1)
#select_ind = runif(n_shocks, 1, 72)
select_ind = runif(n_shocks, 1, n)
out_agg <- c()

for (i in 1:length(shocks)){
  z = (1:n)*0
  z[select_ind[i]] = shocks[i]
  out = z %*% L   #  out = L %*% z  Unterschied
  out_agg[i] = sum(out)
}


#----------------------------------------
# one shock to every industry each period
#----------------------------------------
# das ist was Acemoglu macht.

# exponential distributed shocks
#https://rdrr.io/cran/smoothmest/man/ddoublex.html
# oder 
# https://rdrr.io/cran/nimble/man/Double-Exponential.html
#?


n = dim(L)[1]
n_shocks = 10^3
Shocks <- matrix(0, n, n_shocks)
for (i in 1:n_shocks){
  shocks = rnorm(n, 0, 1)
  Shocks[,i] = shocks
}


out_agg <- c()

for (i in 1:n_shocks){
  out = Shocks[,i] %*% L   #  out = L %*% z  Unterschied
  out_agg[i] = (1/n)*sum(out) #nicht 1/n????
}

# Acemoglu nutzt nicht L sondern hat schon domar weights ausgerechnet.
# Also quasi colSum über L?

v <- (alpha/n) * colSums(L)  # ist 1/n schon in domar weights????, dann muss es auch hier dazu!!
for (i in 1:n_shocks){
  out_agg[i] = t(v) %*% Shocks[,i]
 
}


# structureless

v <- (1/n) * rep(1, n)
for (i in 1:n_shocks){
  out_agg_structureless[i] = t(v) %*% Shocks[,i]
  
}



fluctuations <- bind_cols(list(out_agg = out_agg, out_agg_structureless = out_agg_structureless ,shocks = Shocks[1,]))



sd(out_agg)
sd(shocks)
sd(out_agg)/sd(shocks)  

Shocks_agg = colSums(Shocks)*(1/n)
plot(Shocks_agg, type = "l")
sd(Shocks_agg)  # sd für structureless höher als sd für network; muss es nicht umgekehrt?
# same as in structureless network
plot(rnorm(1000, 0, 0.1154363), type="l")

# 0.7124042 for n = 100; 1.017427 for n=1000   --> increasing variance when going disaggregated???? why?
plot(out_agg, type = "l")
plot(shocks, type = "l")# shocks und agg output nebeneinander stellen
plot(Shocks[1,], type = "l")
hist(out_agg)
plot(cumsum(out_agg), type = "l")
plot(rcauchy(1000), type="l") #see Taleb for other fat tailed distributions besides Cauchy
plot(10000+cumsum(rcauchy(1000)), type = "l")

timestep <- 1:n_shocks
d1 <- data.frame(timestep, shocks, out_agg)





