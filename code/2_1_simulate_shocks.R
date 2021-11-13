setwd("./data/German")


# Input: normalized matrices W
# Output: list with shocks and tibble of different types of aggregate output


simulate_shocks <- function(W, shock_distribution, X, n_shocks){
  
  
  n <- dim(W)[1]
  I <- diag(1, n, n)
  alpha=0.3333  # Acemoglu (2012) use an alpha of 0.3333 for their influence vector v
  L <- solve(I-(1-alpha)*W)
  
  # set n_shocks = 1000 as default
  if (missing(n_shocks)){
    n_shocks = 10^3
  } 
  
  Shocks <- matrix(0, n, n_shocks)
  if (shock_distribution== "normal"){
    Shocks <- rnorm(n*n_shocks, 0, 1)
    'for (i in 1:n_shocks){
      shocks = rnorm(n, 0, 1)
      Shocks[,i] = shocks
    }'
  }else if (shock_distribution == "cauchy"){
    for (i in 1:n_shocks){
      shocks = rcauchy(n, 0, 1)
      Shocks[,i] = shocks
    }
  }
  
  
  # three different influence vectors: structureless economy, output share, network, random network???
  # 1 counterfactual economy with no heterogeneity
  homogenousEconomy <- (1/n)*rep(1, n)
  # 2 counterfactual economy with no primitive heterogeneity
  networkedEconomy <- (1/n)* colSums(L)
  # 3 Calibration with Domar weights matched to the corresponding values in the U.S. data.
  # I don't have Domar weights, or is output share Domar weight???
  # I use sector sales / aggregate output as Domar weight
  #heterogenousOutputEconomy <- X[[1]] / sum(X)   final outputs X must be cut to 71 industries first
  v <- lst(homogenousEconomy, networkedEconomy) # , heterogenousOutputEconomy 
  
  #v <- tibble(v1, v2)
  
  
  #shock_output <- v %>% map(.x=., .f = ~ shocking(., n_shocks, Shocks)) %>% reduce(left_join)
  shock_output <- v %>% map_dfc(.x=., .f = ~ shocking(., n_shocks, Shocks)) %>% mutate(timestep = 1:n_shocks)
  
  
  #ff <- v %>% mutate(across(everything(), ~ shocking(.x, n_shocks, Shocks)))
  
  return(lst(Shocks, shock_output))
}



shocking <- function(v, n_shocks, Shocks){
  out_agg <- c()
  for (i in 1:n_shocks){
    out_agg[i] = v %*% Shocks[(1+((i-1)*n)):(i*n)]
  }
  
  #return(tibble(timestep=c(1:length(out_agg)), out_agg))
  #return(tibble(timestep=c(1:length(v)), v))
  
  return(out_agg)
}


'year=2017
W <- read.table(paste("W", paste(year, ".csv", sep = ""), sep = ""), sep=";", header = FALSE)
I = diag(1, 71,71)
rowSums(W) # normalisierte matrix W hat rowSums all 1
alpha=0.3333  # Acemoglu (2012) use an alpha of 0.3333 for their influence vector v
L = solve(I-(1-alpha)*W)
# System ist für den Rechner singulär: reziproke Konditionszahl = 2.23797e-18




# also create large graph with realistic degree distribution with configuration model
# and then cluster this graph several times into smaller sized graphs [nice exercise :D]




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
'
