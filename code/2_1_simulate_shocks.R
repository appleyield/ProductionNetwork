

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
  }else if (shock_distribution == "laplace"){
    library(smoothmest)
    Shocks <- rdoublex(n*n_shocks,mu=0,lambda=1)
  }
  
  
  # three different influence vectors: structureless economy, output share, network, random network???
  # 1 counterfactual economy with no heterogeneity
  homogeneousEconomy <- rep(1/n, n)
  # 2 counterfactual economy with no primitive heterogeneity
  networkedEconomy <- (1/n)* colSums(L)
  # 3 Calibration with Domar weights matched to the corresponding values in the U.S. data.
  # I don't have Domar weights, or is output share Domar weight???
  # I use sector sales / aggregate output as Domar weight
  outputshare <- X /sum(X)
  heterogeneousnetworkedEconomy <- outputshare[,1] * colSums(L)
  #heterogenousOutputEconomy <- X[[1]] / sum(X)   #final outputs X must be cut to 71 industries first
  v <- lst(homogeneousEconomy, networkedEconomy, heterogeneousnetworkedEconomy) # , heterogenousOutputEconomy 
  
  #euklidian_norm_v <- v %>% map_dfc(calc_euklid_norm_vec) 
  euklidian_norm_v <- v %>% map(calc_euklid_norm_vec) 
  
  #shock_output <- v %>% map2_dfc(.x=., .y= euklidian_norm_v, .f = ~ shocking(.x, n_shocks, Shocks, n, .y)) %>% mutate(timestep = 1:n_shocks)
  
  shock_output <- v %>% map_dfc(.x=., .f = ~ shocking(.x, n_shocks, Shocks, n)) %>% mutate(timestep = 1:n_shocks)
  
  
  
  #ff <- v %>% mutate(across(everything(), ~ shocking(.x, n_shocks, Shocks)))
  
  return(lst(Shocks=tibble(timestep=1:(n_shocks*n), Shocks), shock_output, euklidian_norm_v_tibble <- map_dfc(.x = euklidian_norm_v, .f = ~ .x), v))
}

calc_euklid_norm_vec <- function(x) sqrt(sum(x^2))

shocking <- function(v, n_shocks, Shocks, n){
  out_agg <- c()
  for (i in 1:n_shocks){
    out_agg[i] = v %*% Shocks[(1+((i-1)*n)):(i*n)]
  }

'shocking <- function(v, n_shocks, Shocks, n, euklid_norm_v){
  out_agg <- c()
  for (i in 1:n_shocks){
    out_agg[i] = v %*% ((1/euklid_norm_v) * Shocks[(1+((i-1)*n)):(i*n)])
  }'
  
  #return(tibble(timestep=c(1:length(out_agg)), out_agg))
  #return(tibble(timestep=c(1:length(v)), v))
  
  return(out_agg)
}
