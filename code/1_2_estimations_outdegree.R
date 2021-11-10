### functions for outdegree estimation

# version: 2021-10-23


####### To do: #######

# - xmin of powerlaw is the value of the outdegree, should be the rank.
# - plotting lm looks strange, wrong direction, but also not regressed on CCDF, but rank? Rank is CCDF basically...
# - don't exclude zeros a priori like Acemoglu? Sol: I think I have found a good compromise to exclude only sectors with neither Inputs nor Outputs
# - implement ksr regression
# - xmin of powerlaw for all other distributions
# - test to compare fit of distribution against powerlaw
# - also estimate powerlaw with xmin of lm



######################

library(purrr)
library(dplyr)
library(tidyr)


# In: tibble[year outdegree CCDF], tibble[year cut]
# Out: tibble[year lm_xmin lm_par powerlaw_xmin powerlaw_par ...], tibble[year distribution
# x y]

#outdegree_data = Outdegrees_W_sorted_long

estimate_outdegree <- function(outdegree_data, bs){
  
  keys <- c("powerlaw", "lognormal", "weibull", "exponential")
  keys_exogcut <- c("powerlaw_exogcut", "lognormal_exogcut", "weibull_exogcut", "exponential_exogcut")
  
  models_estimated <- estimate_models(outdegree_data, keys, keys_exogcut)
    
  tib1 <- models_estimated  %>% extract_distros(keys, keys_exogcut)   
  tib2 <- models_estimated %>% extract_parameters(keys, keys_exogcut, bs)
  
  comparison_matrix <- models_estimated %>% compare_distros() %>% .[[1]]
  comparison_matrix_p_one_sided <- models_estimated %>% compare_distros() %>% .[[2]]
  comparison_matrix_p_two_sided <- models_estimated %>% compare_distros() %>% .[[3]]
  
  return(list(tib1, tib2, models_estimated, comparison_matrix, comparison_matrix_p_one_sided, comparison_matrix_p_two_sided))
  
}


initialize_models <- function(outdegree_data){
  
  #models_initialized <- outdegree_data %>% as_tibble() %>% group_by(year) %>% nest() %>%
    #initialize_distributions()
  
  models_initialized <- outdegree_data %>% initialize_distributions()
  
  return(models_initialized)
  
}


# In: tibble[year outdegree CCDF], tibble[year cut]
# Out: tibble[year lm powerlaw exponential ...]
estimate_models <- function(outdegree_data, keys, keys_exogcut){
  
  models_estimated <- outdegree_data %>% initialize_distributions() %>% 
    set_xmin(keys, keys_exogcut) %>% est_pars(keys, keys_exogcut) #maybe set_xmin is enough? only if OLS is put in set_xmin()
    
  return(models_estimated)
  
}





initialize_distributions <- function(Outdegrees_nested){
  
  #why does this work without powerlaw loaded?
  
  # Data should be strictly positive, i.e. no zeros. 
  # so only outdegrees > 0 are used to create distribution objects
  
  
'  for (i in keys){
    liste[[i]] <- Outdegrees_nested %>% map(.x = outdCCDF, ~conpl$new(.x$outdegree[.x$outdegree >0])) # tilde is important
  
  }
  
  Outdegrees_nested <- bind_cols()'
  
  
  Outdegrees_nested <- Outdegrees_nested %>% mutate(powerlaw = map(.x = outdCCDF, ~conpl$new(.x$outdegree[.x$outdegree >0]))) # tilde is important
  Outdegrees_nested <- Outdegrees_nested %>% mutate(powerlaw_exogcut = map(.x = outdCCDF, ~conpl$new(.x$outdegree[.x$outdegree >0]))) # tilde is important
  
  Outdegrees_nested <- Outdegrees_nested %>% mutate(exponential = map(.x = outdCCDF, ~conexp$new(.x$outdegree[.x$outdegree >0])))
  Outdegrees_nested <- Outdegrees_nested %>% mutate(exponential_exogcut = map(.x = outdCCDF, ~conexp$new(.x$outdegree[.x$outdegree >0])))
  
  Outdegrees_nested <- Outdegrees_nested %>% mutate(lognormal = map(.x = outdCCDF, ~conlnorm$new(.x$outdegree[.x$outdegree >0])))
  Outdegrees_nested <- Outdegrees_nested %>% mutate(lognormal_exogcut = map(.x = outdCCDF, ~conlnorm$new(.x$outdegree[.x$outdegree >0])))
  
  Outdegrees_nested <- Outdegrees_nested %>% mutate(weibull = map(.x = outdCCDF, ~conweibull$new(.x$outdegree[.x$outdegree >0])))
  Outdegrees_nested <- Outdegrees_nested %>% mutate(weibull_exogcut = map(.x = outdCCDF, ~conweibull$new(.x$outdegree[.x$outdegree >0])))
  
  
  return(Outdegrees_nested)
}



# In: 
# Out:
set_xmin <- function(Outdegrees_nested, keys, keys_exogcut){
  
  library(poweRlaw)
  
  # estimate 20% of tail by OLS
  pct_cut = 0.2 
  
  Outdegrees_nested$lm_xmin <- Outdegrees_nested$outdCCDF %>% map(select, outdegree) %>% 
    map(unlist) %>% map(length) %>% map(~ round(pct_cut*.x))
  # geht einfacher, length gibts a eine spalte
  
  #Outdegrees_nested <- Outdegrees_nested  %>% 
    #mutate(lm_GIcorrect = map2(.x = outdCCDF, .y =lm_xmin, ~estimateOLS_GIcorrect(.x, .y)))
  
  #Outdegrees_nested <- Outdegrees_nested  %>% 
    #mutate(lm = map2(.x = outdCCDF, .y =lm_xmin, ~estimateOLS(.x, .y)))
  
  
  
  # set xmin of other distributions to xmin of power law
  for (i in keys){
    Outdegrees_nested$powerlaw %>% map(estimate_xmin) %>% 
      map2(.x = Outdegrees_nested[[i]], ., .f = ~.x$setXmin(.y))
  }
  
  

  
  # set xmin of distributions to optimal xmin of power law
  
  'Outdegrees_nested$powerlaw %>% map(estimate_xmin) %>% 
    map2(.x = Outdegrees_nested$powerlaw, ., .f = ~.x$setXmin(.y))
  
  Outdegrees_nested$powerlaw %>% map(~ .x$xmin) %>% unlist() %>% 
    map2(.x = Outdegrees_nested$exponential, ., .f = ~.x$setXmin(.y))
  
  Outdegrees_nested$powerlaw %>% map(~ .x$xmin) %>% unlist() %>% 
    map2(.x = Outdegrees_nested$lognormal, ., .f = ~.x$setXmin(.y))
  
  Outdegrees_nested$powerlaw %>% map(~ .x$xmin) %>% unlist() %>% 
    map2(.x = Outdegrees_nested$weibull, ., .f = ~.x$setXmin(.y))'
  
  
  
  
  
  
  # set xmin of powerlaw_exogcut to xmin of lm
  # xmin has to be the degree, not the rank
  
  for (i in keys_exogcut){
    Outdegrees_nested$outdCCDF %>% map(select, outdegree) %>% 
      map(unlist) %>% map2(.x = ., .y = Outdegrees_nested$lm_xmin, .f = ~ .x[.y]) %>% 
      map2(.x = Outdegrees_nested[[i]], ., .f = ~.x$setXmin(.y))
  }
  
  
  'Outdegrees_nested$outdCCDF %>% map(select, outdegree) %>% 
    map(unlist) %>% map2(.x = ., .y = Outdegrees_nested$lm_xmin, .f = ~ .x[.y]) %>% 
    map2(.x = Outdegrees_nested$powerlaw_exogcut, ., .f = ~.x$setXmin(.y))
  
  Outdegrees_nested$outdCCDF %>% map(select, outdegree) %>% 
    map(unlist) %>% map2(.x = ., .y = Outdegrees_nested$lm_xmin, .f = ~ .x[.y]) %>% 
    map2(.x = Outdegrees_nested$lognormal_exogcut, ., .f = ~.x$setXmin(.y))
  
  Outdegrees_nested$outdCCDF %>% map(select, outdegree) %>% 
    map(unlist) %>% map2(.x = ., .y = Outdegrees_nested$lm_xmin, .f = ~ .x[.y]) %>% 
    map2(.x = Outdegrees_nested$weibull_exogcut, ., .f = ~.x$setXmin(.y))
  
  Outdegrees_nested$outdCCDF %>% map(select, outdegree) %>% 
    map(unlist) %>% map2(.x = ., .y = Outdegrees_nested$lm_xmin, .f = ~ .x[.y]) %>% 
    map2(.x = Outdegrees_nested$exponential_exogcut, ., .f = ~.x$setXmin(.y))'
  

  #Outdegrees_nested$exponential %>% map(estimate_xmin) %>% 
    #map2(.x = Outdegrees_nested$exponential, ., .f = ~.x$setXmin(.y))
  
  #Outdegrees_nested$lognormal %>% map(estimate_xmin) %>% 
    #map2(.x = Outdegrees_nested$lognormal, ., .f = ~.x$setXmin(.y))
  
  #Outdegrees_nested$weibull %>% map(estimate_xmin) %>% 
    #map2(.x = Outdegrees_nested$weibull, ., .f = ~.x$setXmin(.y))
  
  return(Outdegrees_nested)
}


est_pars <- function(Outdegrees_nested, keys, keys_exogcut){
  
  library(poweRlaw)
  
  Outdegrees_nested <- Outdegrees_nested  %>% 
    mutate(lm_GIcorrect = map2(.x = outdCCDF, .y =lm_xmin, ~estimateOLS_GIcorrect(.x, .y)))
  
  Outdegrees_nested <- Outdegrees_nested  %>% 
    mutate(lm = map2(.x = outdCCDF, .y =lm_xmin, ~estimateOLS(.x, .y)))
  
  
  
  # also make Nadarya-Watson estimation!!
  
  Outdegrees_nested <- Outdegrees_nested  %>% 
    mutate(ksr = map2(.x = outdCCDF, .y =lm_xmin, ~estimateKSR(.x, .y)))
  
  'ff1 <- ff  %>% map2(.x = outdCCDF, .y =lm_xmin, .f = ~getoutd(.x, .y))
  
  getoutd <- function(x, y){
    return(x)
  }'
  
  
  #Outdegrees_nested$powerlaw %>% map(estimate_pars) %>% 
    #map2(.x = Outdegrees_nested$powerlaw, ., .f = ~.x$setPars(.y)) # klappt!
  
  for (i in c(keys, keys_exogcut)){
    Outdegrees_nested[[i]] %>% map(estimate_pars) %>% 
      map2(.x = Outdegrees_nested[[i]], ., .f = ~.x$setPars(.y))
  }
  
  'Outdegrees_nested$exponential %>% map(estimate_pars) %>% 
    map2(.x = Outdegrees_nested$exponential, ., .f = ~.x$setPars(.y))
  
  Outdegrees_nested$lognormal %>% map(estimate_pars) %>% 
    map2(.x = Outdegrees_nested$lognormal, ., .f = ~.x$setPars(.y))
  
  Outdegrees_nested$weibull %>% map(estimate_pars) %>% 
    map2(.x = Outdegrees_nested$weibull, ., .f = ~.x$setPars(.y))
  
  Outdegrees_nested$powerlaw_exogcut %>% map(estimate_pars) %>% 
    map2(.x = Outdegrees_nested$powerlaw_exogcut, ., .f = ~.x$setPars(.y))
  
  Outdegrees_nested$lognormal_exogcut %>% map(estimate_pars) %>% 
    map2(.x = Outdegrees_nested$lognormal_exogcut, ., .f = ~.x$setPars(.y))
  
  Outdegrees_nested$weibull_exogcut %>% map(estimate_pars) %>% 
    map2(.x = Outdegrees_nested$weibull_exogcut, ., .f = ~.x$setPars(.y))
  
  Outdegrees_nested$exponential_exogcut %>% map(estimate_pars) %>% 
    map2(.x = Outdegrees_nested$exponential_exogcut, ., .f = ~.x$setPars(.y))'
  
  
  return(Outdegrees_nested)
  
}



extract_distros <- function(Outdegrees_nested, keys, keys_exogcut){
  
  # https://stackoverflow.com/questions/6034655/convert-string-to-a-variable-name
  # https://stackoverflow.com/questions/49242502/how-to-create-dynamic-variable-names-in-a-for-loop-in-r
  # create list
  # add object together with its name to list
  # then map_dfc this list to tibble
  
  distros_list <- list()

  #library(purrr)
  #library(dplyr)
  
  
  #lm <- Outdegrees_nested$lm %>% 
   # map(~tibble(x= .$model$'log(1:xmin)', y= .$fitted.values)) %>%
    #map(exp) %>% tibble(year=Outdegrees_nested$year, distribution="lm", xy=.)
  
  #lm_GIcorrect <- Outdegrees_nested$lm_GIcorrect %>% 
   # map(~tibble(x= .$model$'log((1 - 0.5):(xmin - 0.5))', y= .$fitted.values)) %>%
    #map(exp) %>% tibble(year=Outdegrees_nested$year, distribution="lm_GIcorrect", xy=.)
  
  distros_list[["lm"]] <- Outdegrees_nested$lm %>% 
    map(~tibble(x= .$model$'log(outdegree_tail)', y= .$fitted.values)) %>%
    map(exp) %>% tibble(year=Outdegrees_nested$year, distribution="lm", xy=.)
  
  distros_list[["lm_GIcorrect"]] <- Outdegrees_nested$lm_GIcorrect %>% 
    map(~tibble(x= .$model$'log(outdegree_tail)', y= .$fitted.values)) %>%
    map(exp) %>% tibble(year=Outdegrees_nested$year, distribution="lm_GIcorrect", xy=.)
  
  distros_list[["ksr"]] <- Outdegrees_nested$ksr %>% 
    map(~tibble(x= .$SmoothTail$x, y= .$SmoothTail$y)) %>%
    map(exp) %>% tibble(year=Outdegrees_nested$year, distribution="ksr", xy=.)
  
  
  for (i in c(keys, keys_exogcut)){
    distros_list[[i]] <- Outdegrees_nested[[i]] %>% map(lines, draw=F) %>%
      tibble(year=Outdegrees_nested$year, distribution=i, xy=.)
    
  }
  
  
  'powerlaw <- Outdegrees_nested$powerlaw %>% map(lines, draw=F) %>% 
    map(as_tibble) %>% tibble(year= Outdegrees_nested$year, distribution= "powerlaw", xy = .)
  
  # this has been wrong!!!
  exponential <- Outdegrees_nested$powerlaw %>% map(lines, draw=F) %>% 
    map(as_tibble) %>% tibble(year= Outdegrees_nested$year, distribution= "exponential", xy = .)
  
  weibull <- Outdegrees_nested$weibull %>% map(lines, draw=F) %>% 
    map(as_tibble) %>% tibble(year= Outdegrees_nested$year, distribution = "weibull", xy = .)
  
  # and this also!!
  # so my new method also makes code more robust against typing mistakes
  lognormal <- Outdegrees_nested$weibull %>% map(lines, draw=F) %>% 
    map(as_tibble) %>% tibble(year= Outdegrees_nested$year, distribution = "lognormal", xy = .)
  
  powerlaw_exogcut <- Outdegrees_nested$powerlaw_exogcut %>% map(lines, draw=F) %>% 
    map(as_tibble) %>% tibble(year= Outdegrees_nested$year, distribution= "powerlaw_exogcut", xy = .)'
  

  # solution found here: https://stackoverflow.com/questions/36424986/how-to-make-union-of-n-sets-using-r
  # https://blog.zhaw.ch/datascience/r-reduce-applys-lesser-known-brother/
  distros <- distros_list %>% reduce(union) %>% unnest(cols = c(xy))
  distros$y[which(distros$y==0)] <- NA  # zeros in powerlaw to NA because ggplot removes them
  
  return(distros)
  
}



extract_parameters <- function(Outdegrees_nested, keys, keys_exogcut, bs){
  
  subtract_one <- function(parametervector){
    parametervector_minusOne <- parametervector -1
    return(parametervector_minusOne)
  }
  
  make_positive <- function(parametervector){
    parametervector_positive <- parametervector * (-1)
    return(parametervector_positive)
  }
  
  ### Extract parameters of lm and powerlaw  --------------------
  
  # eine Liste von tibbles erstellen
  
  keys_pl <- c("powerlaw", "powerlaw_exogcut")
  
  Params_list <- list()
  
  Params_list[["lm"]] <- Outdegrees_nested$lm  %>% map(coefficients) %>% map(pluck("log(outdegree_tail)"))  %>%
    unlist() %>% make_positive() %>%
    tibble(year= Outdegrees_nested$year, distribution = "lm" , parameter = .)
  
  Params_list[["lm_GIcorrect"]] <- Outdegrees_nested$lm_GIcorrect  %>% map(coefficients) %>% map(pluck("log(outdegree_tail)"))  %>%
    unlist() %>% make_positive() %>%
    tibble(year= Outdegrees_nested$year, distribution = "lm_GIcorrect" , parameter = .)
  
  Params_list[["ksr"]] <- Outdegrees_nested$ksr  %>% map(.x = ., .f = ~ .x$avg_slope_nw_wo) %>% 
    unlist() %>% make_positive() %>%
    tibble(year= Outdegrees_nested$year, distribution = "ksr" , parameter = .)
  
  ### don't know why this doesn't work????? map(avg_slope_nw_wo)
  
  
  for (i in keys_pl){
    Params_list[[i]] <- Outdegrees_nested[[i]] %>% map(~ .x$pars) %>% unlist() %>%
      subtract_one() %>% 
      tibble(year= Outdegrees_nested$year, distribution = i , parameter = .)
    
  }
  
  
'  for (i in keys_pl){
    Params_list[[i]] <- tt[[i]] %>% map(~ .x$pars) %>% unlist() %>%
      subtract_one() %>%
      tibble(year= tt$year, distribution = i , parameter = .)
    
  }'
  
  'Params_lm <- Outdegrees_nested$lm  %>% map(coefficients) %>% map(pluck("log(outdegree_tail)"))  %>%
    unlist() %>% make_positive() %>%
    tibble(year= Outdegrees_nested$year, distribution = "lm" , parameter = .)
  
  Params_lm_GIcorrect <- Outdegrees_nested$lm_GIcorrect  %>% map(coefficients) %>% map(pluck("log(outdegree_tail)"))  %>%
    unlist() %>% make_positive() %>%
    tibble(year= Outdegrees_nested$year, distribution = "lm_GIcorrect" , parameter = .)'
  
  
  
  'Params_powerlaw <- Outdegrees_nested$powerlaw %>% map(~ .x$pars) %>% unlist() %>%
    subtract_one() %>% 
    tibble(year= Outdegrees_nested$year, distribution = "powerlaw" , parameter = .)
  
 
  Params_powerlaw_exogcut <- Outdegrees_nested$powerlaw_exogcut %>% map(~ .x$pars) %>% unlist() %>%
    subtract_one() %>% 
    tibble(year= Outdegrees_nested$year, distribution = "powerlaw_exogcut" , parameter = .)'
  
  
  # extract also xmin:
  
  Params_list[["lm"]] <- Outdegrees_nested$lm_xmin %>% unlist() %>%
    mutate(Params_list[["lm"]], xmin = .)
  
  Params_list[["lm_GIcorrect"]] <- Outdegrees_nested$lm_xmin %>% unlist() %>%
    mutate(Params_list[["lm_GIcorrect"]], xmin = .)
  
  Params_list[["ksr"]] <- Outdegrees_nested$lm_xmin %>% unlist() %>%
    mutate(Params_list[["ksr"]], xmin = .)
  
  
  for (i in keys_pl){
    #Params_list[[i]] <- Outdegrees_nested[[i]] %>% map(~ .x$xmin) %>% unlist() %>%
      #mutate(Params_list[[i]], xmin = .)
    
    Params_list[[i]] <- Outdegrees_nested[[i]] %>% map(get_ntail) %>% 
      map(as.numeric) %>% unlist(2) %>% mutate(Params_list[[i]], xmin = .)
    #
  }
  
  
 'Params_powerlaw <- Outdegrees_nested$powerlaw %>% map(~ .x$xmin) %>% unlist() %>%
    mutate(Params_powerlaw, xmin = .)
  
  Params_powerlaw_exogcut <- Outdegrees_nested$powerlaw_exogcut %>% map(~ .x$xmin) %>% unlist() %>%
    mutate(Params_powerlaw_exogcut, xmin = .)
  
  # convert the powerlaw xmin to the number of observations:
  # get_ntail extracts "data points greater than or equal to current value of xmin" (powerlaw package documentation)
  Params_powerlaw <- Outdegrees_nested$powerlaw %>% map(get_ntail) %>% 
    map(as.numeric) %>% mutate(Params_powerlaw, xmin_n = .)
  
  Params_powerlaw_exogcut <- Outdegrees_nested$powerlaw_exogcut %>% map(get_ntail) %>% 
    map(as.numeric) %>% mutate(Params_powerlaw_exogcut, xmin_n = .)'
  
  
  
  # GoF, H0: data generated from power law distribution, H1: no power law
  # GoF also for other than powerlaw?
  
  if (bs == TRUE){
    for (i in c("powerlaw", "powerlaw_exogcut")){
      Params_list[[i]] <- Outdegrees_nested[[i]] %>% map(bootstrap_p) %>%
        mutate(Params_list[[i]], bs = .)
      
      Params_list[[i]] <- Params_list[[i]]$bs %>% map(~ .x$p) %>% unlist %>%
        mutate(Params_list[[i]], p_value = .)
    }
  }
  
  
  
  
  'Params_powerlaw <- Outdegrees_nested$powerlaw %>% map(bootstrap_p) %>%
    mutate(Params_powerlaw, bs = .)
  
  Params_powerlaw_exogcut <- Outdegrees_nested$powerlaw_exogcut %>% map(bootstrap_p) %>%
    mutate(Params_powerlaw_exogcut, bs = .)
  
  Params_powerlaw <- Params_powerlaw$bs %>% map(~ .x$p) %>% unlist %>%
    mutate(Params_powerlaw, p_value = .)
  
  Params_powerlaw_exogcut <- Params_powerlaw_exogcut$bs %>% map(~ .x$p) %>% unlist %>%
    mutate(Params_powerlaw_exogcut, p_value = .)'
  
  # standard error of slope:
  #summary(T$lm_GIcorrect[[1]])$coefficients[4]
  
  Params_list[["lm"]] <- Outdegrees_nested[["lm"]] %>% map(summary) %>% map(.x =. , .f= ~.x$coefficients[2,2]) %>% unlist() %>%
    mutate(Params_list[["lm"]], stderror = .)
  
  #Params_lm_GIcorrect <- Outdegrees_nested$lm_GIcorrect %>% map(summary) %>% map(.x =. , .f= ~.x$coefficients[2,2]) %>% unlist() %>%
     #mutate(Params_lm_GIcorrect, stderror = .)
  
  Params_list[["lm_GIcorrect"]] <- Params_list[["lm_GIcorrect"]]$parameter %>% 
    calc_GI_se(unlist(Outdegrees_nested$lm_xmin)) %>% 
    mutate(Params_list[["lm_GIcorrect"]], stderror = .)
  
  
  # bind all together
  
  #Params <- Params_list %>% reduce(bind_rows)
  Params <- Params_list %>% map_dfr(~ .x)
  #Params <- bind_rows(Params_lm, Params_lm_GIcorrect, Params_powerlaw, Params_powerlaw_exogcut)

  return(Params)
}





compare_distros <- function(Outdegrees_nested){

  Comparison_matrix <- tibble(distribution = c("powerlaw", "powerlaw_exogcut"))
    
  # here [[1]] has to be attached, otherwise $getXmin() of compare_distributions() doesnt work  
  lognormal <- list(compare_distributions(Outdegrees_nested$powerlaw[[1]], Outdegrees_nested$lognormal[[1]]), compare_distributions(Outdegrees_nested$powerlaw_exogcut[[1]], Outdegrees_nested$lognormal_exogcut[[1]]))
  weibull <- list(compare_distributions(Outdegrees_nested$powerlaw[[1]], Outdegrees_nested$weibull[[1]]), compare_distributions(Outdegrees_nested$powerlaw_exogcut[[1]], Outdegrees_nested$weibull_exogcut[[1]]))
  exponential <- list(compare_distributions(Outdegrees_nested$powerlaw[[1]], Outdegrees_nested$exponential[[1]]), compare_distributions(Outdegrees_nested$powerlaw_exogcut[[1]], Outdegrees_nested$exponential_exogcut[[1]]))
  
  
  # lognormal also needs the exogenous xmin
  #compare_distributions(Outdegrees_nested$powerlaw_exogcut[[1]], Outdegrees_nested$lognormal_exogcut[[1]])
  
  Comparison_matrix <- Comparison_matrix %>% mutate(lognormal, weibull, exponential)
  
  Comparison_matrix_p_two_sided <- Comparison_matrix %>% 
    mutate(lognormal = map(.x = lognormal, .f = ~ .x$p_two_sided) %>% unlist(), weibull = map(.x = weibull, .f = ~ .x$p_two_sided) %>% unlist(), exponential = map(.x = exponential, .f = ~ .x$p_two_sided) %>% unlist())
  
  Comparison_matrix_p_one_sided <- Comparison_matrix %>% 
    mutate(lognormal = map(.x = lognormal, .f = ~ .x$p_one_sided) %>% unlist(), weibull = map(.x = weibull, .f = ~ .x$p_one_sided) %>% unlist(), exponential = map(.x = exponential, .f = ~ .x$p_one_sided) %>% unlist())
  
  return(list(Comparison_matrix, Comparison_matrix_p_one_sided, Comparison_matrix_p_two_sided))
}


estimateOLS_GIcorrect <- function(Da, xmin){
  #Da$CCDF = Da$CCDF -0.5
  outdegree_tail <- Da$outdegree %>% .[1:xmin] # OLS needs to estimate log-values
  # xmin = 35 now in here
  #cutoff <- Da$CCDF %>% .[xmin]  # substract log(cutoff)? doesn't work
  CCDF_corrected_tail <- ((1-0.5):(length(Da$outdegree)-0.5) / length(Da$outdegree)) %>% .[1:xmin]
  lm(log(CCDF_corrected_tail) ~ log(outdegree_tail))  # log(CCDF_tail - 0.5) das logarithmiert negative Werte,error
  # log((1-0.5):(xmin-0.5))
}


estimateOLS <- function(Da, xmin){
  outdegree_tail <- Da$outdegree %>% .[1:xmin] # OLS needs to estimate log-values
  CCDF_tail <- Da$CCDF %>% .[1:xmin]
  lm(log(CCDF_tail) ~ log(outdegree_tail))  # log(1:xmin)
}

'estimateKSR <- function(Da, xmin){
  
  xy <- Da[[1]] %>% filter(outdegree>0) %>% log()
  
  # bandwidth 
  ### sm-package von Bowman and Azzalini
  #https://cran.r-project.org/web/packages/sm/sm.pdf
  #library(sm)
  #bw_sm <- hcv(xy)
  bw_sm <- sm::h.select(xy, method="cv")
  h = sqrt(bw_sm[1]*bw_sm[2]) %>% as.numeric()  # Acemoglu do this because one value for x an one for y is obtained
  
  
  # kernel smoothing function
  library(bbemkr)
  Linspace <- seq(min(xy$outdegree), max(xy$outdegree), l= length(xy$outdegree))
  
  K1 <- NadarayaWatsonkernel(xy$outdegree, xy$CCDF, h, Linspace) %>% map_dfc(~.x)
  names(K1) <- c("x", "y")
  
  
  
  # calculate average slope of data points
  btemp <- xy$outdegree[xmin[[1]]]
  temp_index <- which(K1$x >= btemp)
  
  avg_slope_nw_wo = (K1$y[1] - K1$y[temp_index[length(temp_index)]]) / (K1$x[1]-K1$x[temp_index[length(temp_index)]])
  
  ksr_object <- lst(avg_slope_nw_wo, h, SmoothTail = K1[temp_index,])
  
  return(ksr_object) # return an object, like the lm object (maybe a tibble?)
}'

estimateKSR <- function(Da, xmin){
  xy <- Da %>% filter(outdegree >0) %>% log()
  
  bw_sm <- sm::h.select(xy, method="cv")
  h = sqrt(bw_sm[1]*bw_sm[2]) %>% as.numeric()  # Acemoglu do this because one value for x an one for y is obtained
  
  
  
  #library(bbemkr)
  Linspace <- seq(min(xy$outdegree), max(xy$outdegree), l= length(xy$outdegree))
  
  K1 <- bbemkr::NadarayaWatsonkernel(xy$outdegree, xy$CCDF, h, Linspace) %>% map_dfc(~.x)
  names(K1) <- c("x", "y")
  
  # calculate average slope of data points
  btemp <- xy$outdegree[xmin[[1]]]
  temp_index <- which(K1$x >= btemp)
  
  avg_slope_nw_wo = (K1$y[temp_index[length(temp_index)]] - K1$y[temp_index[1]]) / (K1$x[temp_index[length(temp_index)]] - K1$x[temp_index[1]])
  
  ksr_object <- lst(avg_slope_nw_wo, h, SmoothTail = K1[temp_index,])
  
  
  return(ksr_object)
}


calc_GI_se <- function(param, xmin){
  se = param * (2/xmin)^(1/2)
  return(se)
}