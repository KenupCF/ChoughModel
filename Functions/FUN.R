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
  
  # Add time (t), alive, pair, and parental info
  pop <- pop %>%
    dplyr::mutate(t = 0, alive = TRUE, 
                  pair = NA_character_, mother_id = NA_character_, father_id = NA_character_,
                  origin="Wild",
                  release_meth=NA_character_,habituation=NA,release_time = NA_character_,
                  age_release=NA,tsr=NA) #tsr = time since release (years)
  
  
  return(pop)
}

# Function to simulate mortality for individuals at a given time step
mortality_aging <- function(pop, currentT, pars,seed=19) {
  
  # Check that required mortality parameter is provided
  needed_pars <- c("phi_df","max_age","improved_foraging","acc_period_df","release_year_cont","supp_feeding_df")
  par_names <- names(pars)
  missing_pars <- needed_pars[which(!needed_pars %in% par_names)]
  if(length(missing_pars) > 0){
    stop(paste0("Error: Missing parameters are ", paste0(missing_pars, collapse=", ")))
  }
  
  # Filter population for individuals at the current time step
  
  current <- pop %>%
    dplyr::filter(t == currentT,alive)
  
  # current$habituation[1]<-1
  # current$release_meth[1]<-"Staged"
  # current$release_time[1]<-"Winter"
  # current$age_release[1]<-current$age[1]
  # current$origin[1]<-"Captive"
  # current$tsr[1]<-.5
  
  current<-current%>%
    
    dplyr::left_join(pars$phi_df)%>%
    dplyr::left_join(pars$acc_period_df)%>%
    dplyr::left_join(pars$release_year_cont)%>%
    dplyr::left_join(pars$supp_feeding_df)%>%
    
    dplyr::mutate(yr_duration=case_when(
      tsr>0~1,
      TRUE~yr_duration
    ))%>%
    dplyr::group_by(id)%>%
    dplyr::mutate(prop_year_acc=min(acc_period-tsr,yr_duration)/yr_duration)
  
  # Assign constant survival probability
  surv <- (
          ### Calculate survival based on habitat conditions
          inv.logit(current$Beta0 + 
                    current$Beta_sf * current$sf +
                    current$Beta_if * pars$improved_foraging[currentT] +
                    current$Beta_b  * current$sf * pars$improved_foraging[currentT]
                    + 0)
          #### Adjust by how much of the year individual spent in the wild
           ^ current$yr_duration)%>%
    ### adjust probability of surviving using odds-ratio, given the proportion of the period individual was under acclimation    
    adjust_probability(current$OR_release,prop=current$prop_year_acc)
    # adjust_probability(current$OR_release,prop=.75)
  
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
    dplyr::mutate(
      ## time since release
      tsr=case_when(tsr==0~tsr+yr_duration, # if 0, add the year duration from release to "new year"
                    TRUE~tsr+1), # if bigger than 0, add a full year
      
      pair=case_when(
      !alive ~ NA,
      TRUE ~ pair
    ),
    
    age=case_when(
      alive ~ age +1,
      TRUE ~ age
    ))%>%
    tidy_pop_df()
    
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
    filter(alive, t == currentT, age >= pars$breeding_age)%>%
    dplyr::ungroup()
  
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
    tidy_pop_df()
  
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
  needed_pars <- c("breeding_age","av_clutch_size","sex_ratio", "max_brood_size", 
                   "nesting_success_df", "brood_size_df", ,"supp_feeding_df","improved_foraging",
                   "transp_fl_OR","eggs_replaced_fem_ids","no_eggs_replaced","prob_nest_aband",
                   "all_ids")
  
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
                  age >= pars$breeding_age)%>%
    dplyr::mutate(egg_swapped=case_when(id%in%pars$eggs_replaced_fem_ids~TRUE,
                                        TRUE~FALSE))%>%
    dplyr::left_join(pars$supp_feeding_df)
    
  if(nrow(reproducing)>0){
    
  # reproducing$egg_swapped[1:5]<-TRUE
    
  egg_swapped_bkp<-reproducing$egg_swapped
    
  # Set random seed for reproducibility
  set.seed(seed + currentT)
  
  # Assign per-female nesting success and expected brood size
    
  reproducing<-reproducing%>%
    dplyr::mutate(B0=NULL,Beta_sf=NULL,Beta_if=NULL,Beta_b=NULL)%>%
    dplyr::left_join(pars$nesting_success_df)
    
    
  prob_nest_sucess <- inv.logit(reproducing$B0+
                                  reproducing$Beta_sf * reproducing$sf +
                                  reproducing$Beta_if * pars$improved_foraging[currentT]+
                                  reproducing$Beta_b  * reproducing$sf * pars$improved_foraging[currentT] +
                                  0) * 
    ((1-pars$prob_nest_aband) ^ as.numeric(reproducing$egg_swapped))
  
  reproducing<-reproducing%>%
    dplyr::mutate(B0=NULL,Beta_sf=NULL,Beta_if=NULL,Beta_b=NULL)%>%
    dplyr::left_join(pars$brood_size_df)
  
  brood_size <- exp(reproducing$B0+
                      reproducing$Beta_sf * reproducing$sf +
                      reproducing$Beta_if * pars$improved_foraging[currentT]+
                      reproducing$Beta_b  * reproducing$sf * pars$improved_foraging[currentT] +
                      0)%>%
    pmax(0)%>%
    pmin(pars$max_brood_size)%>%
    identity()
  
  reproducing$egg_swapped<-egg_swapped_bkp
  
  av_p_fl<-brood_size/pars$max_brood_size # calculate the expected probability of fledging from each egg
  av_p_fl_tr<-adjust_probability(av_p_fl,odds_ratio = pars$transp_fl_OR)
  
  brood_size[reproducing$egg_swapped]<-brood_size[reproducing$egg_swapped]*(av_p_fl_tr[reproducing$egg_swapped]/av_p_fl[reproducing$egg_swapped])
  
  expEggsUnfledged<-sum((reproducing$egg_swapped)*(1-(av_p_fl_tr*prob_nest_sucess))*pars$max_brood_size)
  
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
  
  # Determine subpopulation for each offspring
  motherIDs <- lapply(seq_along(reproducing$id), function(i) {
    tempId<-ifelse(reproducing$egg_swapped[i],NA,reproducing$id[i])
    rep(tempId, offspring[i])
  }) %>%
    unlist() 
  
  # Determine subpopulation for each offspring
  fatherIDs <- lapply(seq_along(reproducing$id), function(i) {
    tempId<-ifelse(reproducing$egg_swapped[i],NA,reproducing$pair[i])
    rep(tempId, offspring[i])
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
    # t = currentT+1,
    alive = TRUE,
    pair = NA,
    mother_id=motherIDs,
    origin="Wild",
    father_id=fatherIDs
  )
  
  # if no reproduction
  }else{
    
    expEggsUnfledged<-0
    
    born<-data.frame()
    
    }
  
  # Append new offspring to existing population
  newpop <- plyr::rbind.fill(pop, born)
  
  return(born)
}

#----------------------------#
# Function: dispersal        #
#----------------------------#
# Simulates the dispersal of individuals across subpopulations.
# Only alive individuals at time `currentT` and in allowed dispersal ages can disperse.
# Dispersal is based on a matrix of probabilities for moving between subpopulations.
#
# Args:
#   pop: Population dataframe.
#   currentT: Current time step.
#   pars: A list containing:
#     - dispersalMat: A square matrix of movement probabilities between subpops.
#       Rows = origin subpop, Columns = destination subpop.
#     - dispersalAges: Vector of ages at which dispersal is allowed.
#   seed: Random seed for reproducibility.
# Returns:
#   Updated population dataframe with new subpopulation assignments for dispersers.
dispersal <- function(pop, currentT, pars, seed = 19) {
  
  # Check required parameters
  needed_pars <- c("dispersalMat", "dispersalAges")
  par_names <- names(pars)
  missing_pars <- needed_pars[which(!needed_pars %in% par_names)]
  if(length(missing_pars) > 0){
    stop(paste0("Error: Missing parameters are ", paste0(missing_pars, collapse = ", ")))
  }
  
  # Set default priority to 0 for everyone
  pop <- pop %>%
    dplyr::mutate(priority = 0)
  
  # Identify all subpop labels
  subpops <- rownames(pars$dispersalMat)
  
  # Select eligible dispersers: alive, at current time, and within dispersal age class
  dispersers <- pop %>%
    dplyr::filter(alive, t == currentT, age %in% pars$dispersalAges) %>%
    dplyr::mutate(priority = 1)
  
  set.seed(seed)
  
  # If there are individuals eligible to disperse
  if(nrow(dispersers) > 0) {
    
    # Sample destination subpopulation based on dispersal matrix
    dispersal_destinations <- sapply(seq_along(dispersers$id), function(i) {
      subpops[which(
        rmultinom(n = 1, size = 1, 
                  prob = pars$dispersalMat[dispersers$subpop[i], ])[, 1] == 1
      )]
    })
    
    # Update subpopulation assignment for dispersers
    dispersers$subpop <- dispersal_destinations
    
    # Combine updated dispersers and original population
    newpop <- plyr::rbind.fill(pop, dispersers) %>%
      tidy_pop_df()
    
  }else{
    newpop<-pop
  }
  
  # Return updated population (with dispersal applied if needed)
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
                                strlength = 8,
                                charset = c(0:9, LETTERS), 
                                max_tries = 10 * n) {
  unique_ids <- character(0)
  tries <- 0
  
  while (length(unique_ids) < n && tries < max_tries) {
    needed <- n - length(unique_ids)
    candidates <- replicate(needed, paste0(sample(charset, strlength, replace = TRUE), collapse = ""))
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



tidy_pop_df <- function(df){
  
  suppressWarnings({if(is.null(df$priority)){df$priority<-0}})
  
  resu<-df%>%
  dplyr::arrange(desc(priority),desc(t)) %>%           # Prioritize updates
    dplyr::filter(!duplicated(data.frame(id,t))) %>%           # Keep only one entry per individual
    dplyr::select(id, subpop, sex, age, t, alive, pair,mother_id,father_id,origin,age_release,release_meth,release_time,habituation,tsr)
  
  return(resu)
}

# Wrapper function to calculate inbreeding coefficients from population dataframe
calculate_inbreeding <- function(pop, initial_inb = NULL) {
  require(kinship2)
  require(dplyr)
  
  # Check if required pedigree columns exist
  if(!all(c("id", "sex", "mother_id", "father_id") %in% names(pop))){
    stop("Population must contain 'id', 'sex', 'mother_id', and 'father_id' columns.")
  }
  
  # Map sex to numeric codes (1 = male, 2 = female) as required by kinship2
  pop <- pop %>%
    mutate(sex_code = ifelse(sex == "M", 1, ifelse(sex == "F", 2, NA)))
  
  # Create pedigree object
  ped <- with(pop, pedigree(id = id, dadid = father_id, momid = mother_id, sex = sex_code))
  
  # Compute kinship matrix
  K <- kinship(ped)
  
  # Inbreeding coefficient F = 2 * kinship(self, self)
  inb <- diag(K) * 2
  
  # Store as named vector
  inb_vec <- tibble(id = names(inb), F = inb)
  
  # Join back into population dataframe
  pop <- pop %>%
    left_join(inb_vec, by = "id")
  
  # Override with known founder inbreeding if provided
  if(!is.null(initial_inb)) {
    matched_ids <- intersect(names(initial_inb), pop$id)
    pop$F[match(matched_ids, pop$id)] <- initial_inb[matched_ids]
  }
  
  return(pop)
}




adjust_probability <- function(prob, odds_ratio,prop=1) {
  
  if (any(prob <= 0) || any(prob >= 1)) {
    warning("Probability must be between 0 and 1 (exclusive).")
  }
  
  if (any(odds_ratio <= 0)) {
    stop("Odds ratio must be greater than 0.")
  }
  
  prop[is.na(prop)] <- 1
  
  odds <- prob / (1 - prob)
  
  adjusted_odds <- odds * odds_ratio
  adjusted_prob <- adjusted_odds / (1 + adjusted_odds)
  
  adjusted_prob[prob%in%c(0,1)]<-prob[prob%in%c(0,1)]
  
  resu<- (prob^(1-prop)) * (adjusted_prob^prop)
  
  return(resu)
}



release_schedule <- function(pars,release_schedule){
  
  
  
  
}


















