# Function to initialize a population data frame
init_population <- function(pars, seed=19) {
  require(dplyr)
  
  # Check that required parameters are present
  needed_pars <- c("StartN", "sex_ratio", "no_age_classes")
  par_names <- names(pars)
  missing_pars <- needed_pars[which(!needed_pars %in% par_names)]
  if(length(missing_pars) > 0){
    stop(paste0("Error: Missing parameters are ", paste0(missing_pars, collapse=", ")))
  }
  
  # Define subpopulation labels
  subpops <- c("A", "B", "C", "D", "E")
  pop <- data.frame()
  existing_ids<-character()
  
  # Loop through each subpopulation
  for (i in seq_along(pars$StartN)) {
    set.seed(i + seed)  # Ensure reproducibility with seed offset
    
    # Generate new individuals with IDs, random ages and sexes
    new_indivs <- data.frame(
      id = generate_unique_ids(n = pars$StartN[i],existing = existing_ids),
      age = sample(1:pars$no_age_classes, pars$StartN[i], replace = TRUE),
      sex = sample(c("F", "M"), pars$StartN[i], replace = TRUE, prob = c(pars$sex_ratio, 1 - pars$sex_ratio)),
      subpop = subpops[i]
    )
    
    # Append new individuals to the population
    pop <- rbind(pop, new_indivs)
    
    existing_ids<-unique(pop$id)
  }
  
  # Add time (t) and alive status columns
  pop <- pop %>%
    dplyr::mutate(t = 0, alive = TRUE,pair=NA)
  
  return(pop)
}

# Function to simulate mortality for individuals at a given time step
mortality_aging <- function(pop, currentT, pars,seed=19) {
  
  # Check that required mortality parameter is provided
  needed_pars <- c("phi_df","max_age")
  par_names <- names(pars)
  missing_pars <- needed_pars[which(!needed_pars %in% par_names)]
  if(length(missing_pars) > 0){
    stop(paste0("Error: Missing parameters are ", paste0(missing_pars, collapse=", ")))
  }
  
  # Filter population for individuals at the current time step
  current <- pop %>%
    dplyr::filter(t == currentT)%>%
    dplyr::left_join(pars$phi_df)
  
  # Assign constant survival probability
  surv <- current$survival
  
  set.seed(seed)
  # Simulate survival for each individual using Bernoulli trial
  alive_vec <- sapply(seq_along(current$id), FUN = function(i) {
    rbinom(n = 1, size = 1, prob = surv[i])
  }) == 1 & 
    current$alive & 
    current$age < pars$max_age
  
  # Update alive status based on survival outcome
  current$alive <- alive_vec
  
  current<-current%>%
    dplyr::mutate(pair=case_when(
      !alive ~ NA,
      TRUE ~ pair
    ),
    age=case_when(
      alive ~ age +1,
      TRUE ~ age
    ))%>%
    dplyr::select(id,subpop,sex,age,t,alive,pair)
    
  return(current)
}

#--------------------------#
# Function: pairing        #
#--------------------------#
# Pairs breeding-age adults within each subpopulation.
# Args:
#   pop: Population dataframe.
#   currentT: Current time step.
#   pars: A list with "breeding_age".
#   seed: Random seed for reproducibility.
# Returns:
#   Updated adult population dataframe with pair IDs assigned.
pairing <- function(pop, currentT, pars, seed=19) {
  require(dplyr)
  
  needed_pars <- c("breeding_age")
  par_names <- names(pars)
  missing_pars <- needed_pars[which(!needed_pars %in% par_names)]
  if(length(missing_pars) > 0){
    stop(paste0("Error: Missing parameters are ", paste0(missing_pars, collapse=", ")))
  }
  
  # Add priority to manage replacement of existing pairs
  pop <- pop %>%
    dplyr::mutate(priority = 0)
  
  adults <- pop %>%
    filter(alive, t == currentT, age >= pars$breeding_age)
  
  pairs <- data.frame()
  subpops <- unique(adults$subpop)
  
  # Pairing process for each subpopulation
  for (sp in subpops) {
    males <- adults %>% filter(sex == "M", subpop == sp, is.na(pair))
    females <- adults %>% filter(sex == "F", subpop == sp, is.na(pair))
    
    set.seed(seed + currentT)
    males <- males[sample(nrow(males)), ]
    females <- females[sample(nrow(females)), ]
    
    n_pairs <- min(nrow(males), nrow(females))
    if(n_pairs>0){
    pairs <- plyr::rbind.fill(pairs,
                              (males[1:n_pairs, ]) %>%
                                dplyr::mutate(pair = females$id[1:n_pairs], priority = 1),
                              (females[1:n_pairs, ]) %>%
                                dplyr::mutate(pair = males$id[1:n_pairs], priority = 1)
    )
    }
  }
  
  # Prioritize newly formed pairs
  adults_new <- plyr::rbind.fill(pop, pairs) %>%
    dplyr::arrange(desc(priority)) %>%
    dplyr::filter(!duplicated(id, t)) %>%
    dplyr::mutate(priority = NULL) %>%
    dplyr::ungroup()
  
  return(adults_new)
}

#-----------------------------------#
# Function: unpair_if_dead          #
#-----------------------------------#
# Removes pair references to individuals who died in currentT.
# Args:
#   pop: Population dataframe (includes both dead and alive).
#   currentT: Time step.
# Returns:
#   Updated dataframe with invalid pairings removed.
unpair_if_dead <- function(pop, currentT) {
  require(dplyr)
  
  alive_ids <- pop %>%
    dplyr::filter(t == currentT, alive) %>%
    dplyr::pull(id)
  
  pop <- pop %>%
    dplyr::mutate(pair = case_when(
      (!pair %in% alive_ids) ~ NA,
      TRUE ~ pair
    ))
  
  return(pop)
}


#----------------------------#
# Function: recruitment      #
#----------------------------#
# Adds new offspring to the population at a given time step.
# Only females that are alive, paired, of breeding age, and present at time `currentT` can reproduce.
# Offspring number per female is drawn from a truncated Poisson distribution if nesting is successful.
#
# Args:
#   pop: Current population dataframe.
#   currentT: Current time step.
#   pars: A list containing:
#     - breeding_age: Minimum age required for reproduction.
#     - nesting_success: Probability that a female successfully nests.
#     - av_brood_size: Mean number of offspring per nesting attempt.
#     - sex_ratio: Probability of female among offspring.
#     - max_brood_size: Maximum allowed number of offspring per female.
#     - all_ids: Character vector of all previously used IDs.
#   seed: Random seed for reproducibility.
# Returns:
#   Updated population dataframe with new individuals appended.
recruitment <- function(pop, currentT, pars, seed = 19) {
  
  # Ensure required parameters are present
  needed_pars <- c("breeding_age", "nesting_success_df", "av_brood_size", 
                   "sex_ratio", "max_brood_size", "all_ids")
  par_names <- names(pars)
  missing_pars <- needed_pars[which(!needed_pars %in% par_names)]
  if(length(missing_pars) > 0){
    stop(paste0("Error: Missing parameters are ", paste0(missing_pars, collapse = ", ")))
  }
  
  # Filter eligible reproducing females
  reproducing <- pop %>%
    dplyr::filter(sex == "F",
                  !is.na(pair),
                  alive,
                  t == currentT,
                  age >= pars$breeding_age)
  
  if(nrow(reproducing)>0){
  # Set random seed for reproducibility
  set.seed(seed + currentT)
  
  # Assign per-female nesting success and expected brood size
    
  reproducing<-reproducing%>%
    dplyr::left_join(pars$nesting_success_df)
    
    
  prob_nest_sucess <- reproducing$p_nest_success
  brood_size <- rep(pars$av_brood_size, nrow(reproducing))
  
  # Simulate number of offspring per reproducing female
  offspring <- sapply(seq_along(reproducing$id), function(i) {
    rbinom(n = 1, size = 1, prob = prob_nest_sucess[i]) *
      rtpois(n = 1, lambda = brood_size[i], max = pars$max_brood_size + 0.01)
  })
  
  # Determine subpopulation for each offspring
  newSubpop <- lapply(seq_along(reproducing$id), function(i) {
    rep(reproducing$subpop[i], offspring[i])
  }) %>%
    unlist()
  
  # Assign sexes to offspring
  newSex <- c("F", "M")[rbinom(n = sum(offspring), size = 1, prob = pars$sex_ratio) + 1]
  
  # Generate unique IDs for all new offspring
  born <- data.frame(
    id = generate_unique_ids(n = sum(offspring), existing = pars$all_ids),
    age = 1,
    sex = newSex,
    subpop = newSubpop,
    t = currentT,
    alive = TRUE,
    pair = NA
  )}else{born<-data.frame()}
  
  # Append new offspring to existing population
  newpop <- plyr::rbind.fill(pop, born)
  
  return(newpop)
}

dispersal   <- function(pop, currentT,pars,seed=19){
  
  needed_pars <- c("dispersalMat","dispersalAges")
  par_names <- names(pars)
  missing_pars <- needed_pars[which(!needed_pars %in% par_names)]
  if(length(missing_pars) > 0){
    stop(paste0("Error: Missing parameters are ", paste0(missing_pars, collapse=", ")))
  }
  
  pop<-pop%>%
    dplyr::mutate(priority=0)
  
  subpops<-rownames(pars$dispersalMat)
  
  
  dispersers<-pop%>%
    dplyr::filter(alive,t==currentT,age%in%pars$dispersalAges)%>%
    dplyr::mutate(priority=1)
  
  if(nrow(dispersers)>0){
    
    dispersal_destinations<-sapply(seq_along(dispersers$id),function(i){
      
      subpops[which(rmultinom(n = 1,prob=pars$dispersalMat[dispersers$subpop,],size = 1)[,1]==1)]
      
    })
    
    dispersers$subpop<-dispersal_destinations
    
    newpop<-plyr::rbind.fill(pop,dispersers)%>%
      dplyr::arrange(desc(priority))%>%
      dplyr::filter(!duplicated(id))%>%
      dplyr::select(id,subpop,sex,age,t,alive,pair)
    
  }
  
  
  return(newpop)
  
}

# Function to simulate zero-truncated Poisson
rtpois <- function(n, lambda,min=0,max=Inf) {
  x <- rpois(n, lambda)
  while(any(x <= min | x >= max)) {
    x[x <= min | x >= max] <- rpois(sum(x <= min | x >= max), lambda)
  }
  return(x)
}

#----------------------------#
# Function: generate_unique_ids
#----------------------------#
# Generates multiple unique 8-character strings not found in the given vector.
# Args:
#   n: Number of unique IDs to generate.
#   existing: A character vector of already-used strings.
#   charset: Characters to sample from (default: letters and digits).
#   max_tries: Maximum attempts to find unique IDs (default: 10 * n).
# Returns:
#   A character vector of `n` unique strings.
generate_unique_ids <- function(n, existing = character(0), 
                                charset = c(0:9, LETTERS), 
                                max_tries = 10 * n) {
  unique_ids <- character(0)
  tries <- 0
  
  while (length(unique_ids) < n && tries < max_tries) {
    needed <- n - length(unique_ids)
    candidates <- replicate(needed, paste0(sample(charset, 8, replace = TRUE), collapse = ""))
    # Filter out existing and duplicate values
    new_ids <- setdiff(candidates, c(existing, unique_ids))
    unique_ids <- unique(c(unique_ids, new_ids))
    tries <- tries + 1
  }
  
  if (length(unique_ids) < n) {
    stop("Failed to generate the required number of unique IDs after ", max_tries, " attempts.")
  }
  
  return(unique_ids)
}