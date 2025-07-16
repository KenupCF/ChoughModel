# Function to initialize a population data frame
init_population <- function(pars, seed=19) {
  require(dplyr)
  
  # Check that required parameters are present
  needed_pars <- c("StartN", "sex_ratio", "no_age_classes","Fp","age_structure")
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
    # set.seed(i + seed)  # Ensure reproducibility with seed offset
    
    # Generate new individuals with IDs, random ages and sexes
    new_indivs <- data.frame(
      id = generate_unique_ids(n = pars$StartN[i],existing = existing_ids),
      age = sample(1:pars$no_age_classes, pars$StartN[i], replace = TRUE, prob = pars$age_structure),
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
                  origin="Wild",scot_heritage=1,
                  release_meth=NA_character_,habituation=NA,release_time = NA_character_,
                  Fi=pars$Fp,
                  age_release=NA,tsr=NA,year_born=age-t) #tsr = time since release (years)
  
  
  return(pop)
}

# Function to simulate mortality for individuals at a given time step
mortlity_agng <- function(pop, currentT, pars,seed=19) {
  
  # Check that required mortality parameter is provided
  needed_pars <- c("phi_df","max_age","improved_foraging",
                   "start_cycle",
                   "dip_leth_eq",
                   "carr_capac_df",
                   "acc_period_df","release_year_cont","supp_feeding_current","full_pop")
  par_names <- names(pars)
  missing_pars <- needed_pars[which(!needed_pars %in% par_names)]
  if(length(missing_pars) > 0){
    stop(paste0("Error: Missing parameters are ", paste0(missing_pars, collapse=", ")))
  }
  
  # Filter population for individuals at the current time step
  
  current <- pop %>%
    dplyr::filter(t == currentT,alive)
  
  
  current<-current%>%
    
    dplyr::left_join(pars$phi_df)%>%
    dplyr::left_join(pars$acc_period_df)%>%
    dplyr::left_join(pars$release_year_cont)%>%
    dplyr::left_join(pars$supp_feeding_current)%>%
    
    dplyr::mutate(yr_duration=case_when(
      tsr>0~1,
      TRUE~yr_duration
    ))%>%
    # dplyr::group_by(id)%>%
    dplyr::mutate(prop_year_acc=pmin(pmax(acc_period-tsr,0),yr_duration)/yr_duration)
  
  N_df<-current%>%
    dplyr::group_by(subpop)%>%
    dplyr::summarise(N=sum(alive))%>%
  dplyr::left_join(pars$carr_capac_df)%>%
  dplyr::mutate(AboveC=N>C,surv_trunc=1/(N/C))
  
  # ib_df<-calculate_inbreeding(pop = pars$full_pop,pars=pars)
  
  # Assign constant survival probability
  
  # environmental stochasticity (random increases throughout all parameters)
  # cyclcical variation
  # set.seed(seed+1)
  q_sample<-runif(n = 1,min = 0,max = 1)
  
  surv_mean<-mean<-current$Beta0
  cyc_var <- current$Beta0_sd_cyc*sin(pi*(currentT+pars$start_cycle)/2.5)
  sto_var<- sapply(seq_along(current$id),function(i){
    qnorm(p = q_sample,mean = 0,sd=current$Beta0_sd_sto[i])})
  
    surv_int<- surv_mean + sto_var + cyc_var
    
  
    surv <-
          ### Calculate survival based on habitat conditions
          inv.logit(surv_int + 
                    current$Beta_sf * current$sf +
                    current$Beta_if * pars$improved_foraging[currentT] +
                    current$Beta_b  * current$sf * pars$improved_foraging[currentT]
                    + 0)
    
   
    # add inbreeding depressio on survival if it is age class 1
    surv <- exp(log(surv) - current$Fi*(pars$dip_leth_eq/2)*(current$age==1) )
     
    #### Adjust by how much of the year individual spent in the wild
    surv<-surv ^ current$yr_duration
    
    ### adjust probability of surviving using odds-ratio, given the proportion of the period individual was under acclimation    
    surv<-adjust_probability(surv,current$OR_release,prop=current$prop_year_acc)
    
    current$surv<-surv
    
    current<-current%>%
      dplyr::left_join(N_df)%>%
      dplyr::group_by(id)%>%
      dplyr::mutate(surv=case_when(AboveC~min(surv_trunc,surv),
                                   TRUE~surv))
          
    if(any(is.na(surv)) | any(surv < 0) | any(surv > 1)){stop("Invalid estimated survival probability")}
    
  # set.seed(seed)
  # Simulate survival for each individual using Bernoulli trial
  alive_vec <- sapply(seq_along(current$id), FUN = function(i) {
    rbinom(n = 1, size = 1, prob = current$surv[i])
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
  
  resu<-list(alive=current,stoch_q=q_sample)  
  return(resu)
}

mortality <- function(pop, currentT, pars,seed=19) {
  
  # Check that required mortality parameter is provided
  needed_pars <- c("phi_df","max_age","improved_foraging",
                   "start_cycle",
                   "dip_leth_eq",
                   "carr_capac_df",
                   "acc_period_df","release_year_cont","supp_feeding_current","full_pop")
  par_names <- names(pars)
  missing_pars <- needed_pars[which(!needed_pars %in% par_names)]
  if(length(missing_pars) > 0){
    stop(paste0("Error: Missing parameters are ", paste0(missing_pars, collapse=", ")))
  }
  
  # Filter population for individuals at the current time step
  
  current <- pop %>%
    dplyr::filter(t == currentT,alive)
  
  # current<-current%>%
  #   dplyr::filter(!is.na(age_release))
  current<-current%>%
    
    dplyr::left_join(pars$phi_df)%>%
    dplyr::left_join(pars$acc_period_df)%>%
    dplyr::left_join(pars$release_year_cont)%>%
    dplyr::left_join(pars$supp_feeding_current)%>%
    
    dplyr::mutate(yr_duration=case_when(
      tsr>0~1,
      TRUE~yr_duration
    ))%>%
    # dplyr::group_by(id)%>%
    dplyr::mutate(prop_year_acc=pmin(pmax(acc_period-tsr,0),yr_duration)/yr_duration)
  
  N_df<-current%>%
    dplyr::group_by(subpop)%>%
    dplyr::summarise(N=sum(alive))%>%
    dplyr::left_join(pars$carr_capac_df)%>%
    dplyr::mutate(AboveC=N>C,surv_trunc=1/(N/C))
  
  # ib_df<-calculate_inbreeding(pop = pars$full_pop,pars=pars)
  
  # Assign constant survival probability
  
  # environmental stochasticity (random increases throughout all parameters)
  # cyclcical variation
  # set.seed(seed+1)
  q_sample<-runif(n = 1,min = 0,max = 1)
  
  surv_mean<-mean<-current$Beta0
  cyc_var <- current$Beta0_sd_cyc*sin(pi*(currentT+pars$start_cycle)/2.5)
  sto_var<- sapply(seq_along(current$id),function(i){
    qnorm(p = q_sample,mean = 0,sd=current$Beta0_sd_sto[i])})
  
  surv_int<- surv_mean + sto_var + cyc_var
  
  
  surv <-
    ### Calculate survival based on habitat conditions
    inv.logit(surv_int + 
                current$Beta_sf * current$sf +
                current$Beta_if * pars$improved_foraging[currentT] +
                current$Beta_b  * current$sf * pars$improved_foraging[currentT]
              + 0)
  
  
  # add inbreeding depressio on survival if it is age class 1
  surv <- exp(log(surv) - current$Fi*(pars$dip_leth_eq/2)*(current$age==1) )
  
  #### Adjust by how much of the year individual spent in the wild
  surv<-surv ^ current$yr_duration
  
  ### adjust probability of surviving using odds-ratio, given the proportion of the period individual was under acclimation    
  surv<-adjust_probability(surv,current$OR_release,prop=current$prop_year_acc)
  
  current$surv<-surv
  
  current<-current%>%
    dplyr::left_join(N_df)%>%
    dplyr::group_by(id)%>%
    dplyr::mutate(surv=case_when(AboveC~min(surv_trunc,surv),
                                 TRUE~surv))
  
  if(any(is.na(surv)) | any(surv < 0) | any(surv > 1)){stop("Invalid estimated survival probability")}
  
  # set.seed(seed)
  # Simulate survival for each individual using Bernoulli trial
  alive_vec <- sapply(seq_along(current$id), FUN = function(i) {
    rbinom(n = 1, size = 1, prob = current$surv[i])
  }) == 1 & 
    current$alive & 
    current$age < pars$max_age
  
  # Update alive status based on survival outcome
  current$alive <- alive_vec
  
  current<-current%>%
    dplyr::mutate(
      ## time since release
      # tsr=case_when(tsr==0~tsr+yr_duration, # if 0, add the year duration from release to "new year"
                    # TRUE~tsr+1), # if bigger than 0, add a full year
      
      pair=case_when(
        !alive ~ NA,
        TRUE ~ pair
      ),
      
      # age=case_when(
        # alive ~ age +1,
        # TRUE ~ age
      # )
    )%>%
    tidy_pop_df()
  
  resu<-list(alive=current,stoch_q=q_sample)  
  return(resu)
}

aging <- function(pop, currentT, pars,seed=19) {
  
  # Check that required mortality parameter is provided
  needed_pars <- c("phi_df","max_age",
                   # "improved_foraging",
                   # "start_cycle",
                   # "dip_leth_eq",
                   # "carr_capac_df",
                   "acc_period_df","release_year_cont",
                   "supp_feeding_current","full_pop")
  par_names <- names(pars)
  missing_pars <- needed_pars[which(!needed_pars %in% par_names)]
  if(length(missing_pars) > 0){
    stop(paste0("Error: Missing parameters are ", paste0(missing_pars, collapse=", ")))
  }
  
  # Filter population for individuals at the current time step
  
  current <- pop %>%
    dplyr::filter(t == currentT)
  
  
  current<-current%>%
    
    # dplyr::left_join(pars$phi_df)%>%
    dplyr::left_join(pars$acc_period_df)%>%
    dplyr::left_join(pars$release_year_cont)%>%
    # dplyr::left_join(pars$supp_feeding_current)%>%
    
    dplyr::mutate(yr_duration=case_when(
      tsr>0~1,
      TRUE~yr_duration
    ))%>%
    # dplyr::group_by(id)%>%
    dplyr::mutate(prop_year_acc=pmin(pmax(acc_period-tsr,0),yr_duration)/yr_duration)


  # Assign constant survival probability
  
  current<-current%>%
    dplyr::mutate(
      ## time since release
      tsr=case_when(tsr==0~tsr+yr_duration, # if 0, add the year duration from release to "new year"
      TRUE~tsr+1), # if bigger than 0, add a full year
      
      # pair=case_when(
        # !alive ~ NA,
        # TRUE ~ pair
      # ),
      
      age=case_when(
      alive ~ age +1,
      TRUE ~ age
      )
    )%>%
    tidy_pop_df()
  
  resu<-current
  return(resu)
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
    
    # set.seed(seed + currentT)
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
recruitment <- function(pop, currentT, pars, seed = 19,envir_stoch=TRUE,startAge=0) {
  
  # Ensure required parameters are present
  needed_pars <- c("breeding_age","av_clutch_size","sex_ratio", "max_brood_size", 
                   "nesting_success_df", "brood_size_df","supp_feeding_current","improved_foraging",
                   "transp_fl_OR","eggs_replaced_fem_ids","no_eggs_replaced","prob_nest_aband",
                   "all_ids")
  
  par_names <- names(pars)
  
  missing_pars <- needed_pars[which(!needed_pars %in% par_names)]
  
  if(length(missing_pars) > 0){
    stop(paste0("Error: Missing parameters are ", paste0(missing_pars, collapse = ", ")))
  }
  # 
  #  
  temp<-pop%>%
    dplyr::filter(
  #                 sex == "F",
                  !is.na(pair),
                  alive,
                  t == currentT,
                  age >= pars$breeding_age)%>%
    dplyr::left_join(pars$acc_period_df)%>%
    # dplyr::mutate(acc_period=case_when(
    # is.na(acc_period)~0,
    # TRUE~acc_period))%>%
    dplyr::left_join(pars$release_year_cont)%>%
    dplyr::mutate(yr_duration=case_when(
      tsr>0~1,
      TRUE~yr_duration
    ))%>%
    dplyr::mutate(prop_year_acc=pmin(pmax(acc_period-tsr,0),yr_duration)/yr_duration)%>%
    dplyr::mutate(prop_year_acc=case_when(
      is.na(prop_year_acc)~0,
      TRUE~prop_year_acc
    ))%>%
    # dplyr::mutate(yr_duration=4/12,prop_year_acc=1)%>%
    dplyr::mutate(
      present_in_april=yr_duration>=(3/12),
      months_in_wild=yr_duration*12,
      months_acclimating=months_in_wild*prop_year_acc,
      begin_acc=(12*(1-yr_duration)),
      final_acc=begin_acc+months_acclimating,
      acclimating_in_april=final_acc>=10 & begin_acc<=10)%>%
    dplyr::mutate(unable_to_breed = !present_in_april | acclimating_in_april,
                  able_to_breed = !unable_to_breed)

  male_breed_capacity <- temp%>%filter(sex=="M")%>%
    pull_named(able_to_breed,id)
  
  female_breed_capacity <- temp%>%filter(sex=="F")%>%
    pull_named(able_to_breed,id)
  
  female_pair <- temp%>%filter(sex=="F")%>%
    pull_named(pair,id)
  
  f_breed_df<-data.frame(f_able_to_breed=female_breed_capacity,
                         id=names(female_breed_capacity),
                         pair=female_pair,
                         m_able_to_breed=male_breed_capacity[female_pair])
  
  # Filter eligible reproducing females
  reproducing <- pop %>%
    dplyr::filter(sex == "F",
                  !is.na(pair),
                  alive,
                  t == currentT,
                  age >= pars$breeding_age)%>%
    dplyr::mutate(egg_swapped=FALSE)%>%
    dplyr::left_join(pars$supp_feeding_current)%>%
    dplyr::left_join(f_breed_df)%>%
    dplyr::mutate(able_to_breed = f_able_to_breed & m_able_to_breed)
  
  # # reproducing$male_breed<-male_breed_capacity[reproducing$pair]
  # reproducing$female_breed<-reproducing$able_to_breed
  
  # reproducing$able_to_breed<-reproducing$female_breed & reproducing$male_breed
  
  # set.seed(seed+1)
  if(envir_stoch){q_sample<-runif(n = 1,min = 0,max = 1)}else{q_sample<-.5}
  
  if(nrow(reproducing)>0){
    
  
  if(nrow(pars$release_schedule_df)>0){
  firstRelease<-1
  lastRelease<-unique(pars$release_schedule_df$release_years)
    
  releaseYears<-firstRelease:lastRelease
    
  if(pars$release_schedule_df$wait_for_habitat){
      
      suppressWarnings({first_year_improved_foraging<-min(which(pars$improved_foraging==1))})
      releaseYears<-releaseYears+first_year_improved_foraging-1
      
  }  
  }else{releaseYears=Inf}
    
    
  no_surrogate_nests<-round( pars$release_schedule_df$noEggsReleased * (currentT%in%releaseYears) /pars$max_brood_size)
  
  candidate_surrogates<-reproducing%>%
    filter(is.na(tsr),subpop%in%pars$release_schedule_df$subpop)%>%
    pull(id)
  
  # set.seed(seed + currentT)
  surrogates<-sample(candidate_surrogates,size = min(no_surrogate_nests,length(candidate_surrogates)),replace=F)
  
  reproducing$egg_swapped[reproducing$id%in%surrogates]<-TRUE
  egg_swapped_bkp<-reproducing$egg_swapped
    
  # Set random seed for reproducibility
  # set.seed(seed + currentT)
  
  # Assign per-female nesting success and expected brood size
    
  reproducing<-reproducing%>%
    dplyr::mutate(B0=NULL,Beta_sf=NULL,Beta_if=NULL,Beta_b=NULL)%>%
    dplyr::left_join(pars$nesting_success_df)
  
  prob_nest_sucess <- inv.logit(reproducing$B0+
                                  reproducing$Beta_sf * reproducing$sf +
                                  reproducing$Beta_if * pars$improved_foraging[currentT]+
                                  reproducing$Beta_b  * reproducing$sf * pars$improved_foraging[currentT] +
                                  0) 
  prob_nest_sucess<-prob_nest_sucess *
    ((1-(pars$prob_nest_aband*unique(pars$release_schedule_df$nest_aband_allowed))) ^ as.numeric(reproducing$egg_swapped))

  # prob_nest_sucess<-prob_nest_sucess * as.numeric(reproducing$able_to_breed)
  
  reproducing<-reproducing%>%
    dplyr::mutate(B0=NULL,Beta_sf=NULL,Beta_if=NULL,Beta_b=NULL)%>%
    dplyr::left_join(pars$brood_size_df)
  
  bs_mean<-reproducing$B0
  
  bs_st_var<-sapply(seq_along(reproducing$id),function(i){
    qnorm(p = q_sample,mean = 0,sd=reproducing$B0_sd[i])
    })
  
  bs_int <- bs_mean + bs_st_var
  # bs_int <- pmax(bs_int,0)
  # bs_int <- log(bs_int)
  
  brood_size <- exp(bs_int+
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
  
  expEggsUnfledged<-sum( 
    
     (reproducing$egg_swapped)*
       
     (1-(av_p_fl_tr*prob_nest_sucess))*
       
     pars$max_brood_size)
  
  # Simulate number of offspring per reproducing female
  offspring <- sapply(seq_along(reproducing$id), function(i) {
    rbinom(n = 1, size = 1, prob = prob_nest_sucess[i]) *
      rtpois(n = 1, lambda = brood_size[i], max = pars$max_brood_size + 0.01)
  })
  
  actEggsSwapped<-(pars$max_brood_size*length(surrogates))
  actEggsUnfledged<-actEggsSwapped-sum(offspring[reproducing$egg_swapped])
  
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
  
  
  # Determine subpopulation for each offspring
  scot_heritage_vec <- lapply(seq_along(reproducing$id), function(i) {
    tempId<-ifelse(reproducing$egg_swapped[i],0,NA)
    rep(tempId, offspring[i])
  }) %>%
    unlist()  
  
  
  # Determine age release for each offspring
  age_rel_vec <- lapply(seq_along(reproducing$id), function(i) {
    tempId<-ifelse(reproducing$egg_swapped[i],0,NA)
    rep(tempId, offspring[i])
  }) %>%
    unlist()  
  # Assign sexes to offspring
  newSex <- c("F", "M")[rbinom(n = sum(offspring), size = 1, prob = pars$sex_ratio) + 1]
  
  if(sum(offspring)>0){
  # Generate unique IDs for all new offspring
  born <- data.frame(
    id = generate_unique_ids(n = sum(offspring), existing = pars$all_ids),
    age = startAge,
    age_release=age_rel_vec,
    sex = newSex,
    subpop = newSubpop,
    t = currentT,
    # t = currentT+1,
    alive = TRUE,
    pair = NA,
    mother_id=motherIDs,
    origin="Wild",
    father_id=fatherIDs,
    scot_heritage = scot_heritage_vec,
    year_born=currentT
  )
  }else{
    
    born<-data.frame()
  }
  
  # if no reproduction
  }else{
    
    expEggsUnfledged<-0
    actEggsUnfledged<-0
    actEggsSwapped<-0
    born<-data.frame()
    
    }
  
  # Append new offspring to existing population
  newpop <- plyr::rbind.fill(pop, born)
  egg_fate<-data.frame(t=currentT,swapped=actEggsSwapped,unfledged=actEggsUnfledged)
  
  return(list(born=born,expEggsUnfledged=expEggsUnfledged,actEggsUnfledged=actEggsUnfledged,stoch_q=q_sample,egg_fate=egg_fate))
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
  
  # set.seed(seed)
  
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
    dplyr::select(id, subpop, sex, age, t, alive, pair,mother_id,father_id,Fi,scot_heritage,year_born,
                  origin,age_release,release_meth,release_time,habituation,tsr)%>%
    dplyr::arrange(desc(alive),subpop,age,mother_id,origin)
  
  return(resu)
}



adjust_probability <- function(prob, odds_ratio,prop=1,agg_rule="survival") {
  
  if (any(prob < 0) || any(prob > 1)) {
    stop("Probability must be between 0 and 1 (inclusive).")
  }
  
  if (any(odds_ratio <= 0)) {
    stop("Odds ratio must be greater than 0.")
  }
  
  prop[is.na(prop)] <- 1
  
  odds <- prob / (1 - prob)
  
  adjusted_odds <- odds * odds_ratio
  adjusted_prob <- adjusted_odds / (1 + adjusted_odds)
  
  adjusted_prob[prob%in%c(0,1)]<-prob[prob%in%c(0,1)]
  
  if(agg_rule=="survival"){
  resu<- (prob^(1-prop)) * (adjusted_prob^prop)
  }
  
  # if(agg_rule=="success"){
  #   fail <- ((1-prob)^(1-prop)) * ((1 - adjusted_prob)^prop)
  #   resu <- 1-fail
  # }
  # 
  
  return(resu)
}


releases<-function(pars,currentT){
  
  # Ensure required parameters are present
  needed_pars <- c("release_schedule_df","all_ids","improved_foraging")
  
  par_names <- names(pars)
  
  missing_pars <- needed_pars[which(!needed_pars %in% par_names)]
  
  if(length(missing_pars) > 0){
    stop(paste0("Error: Missing parameters are ", paste0(missing_pars, collapse = ", ")))
  }
  
  if(nrow(pars$release_schedule_df)>1){
    stop("Error with release schedule")
  }
  
  firstRelease<-1
  lastRelease<-unique(pars$release_schedule_df$release_years)
  
  releaseYears<-firstRelease:lastRelease
  
  if(pars$release_schedule_df$wait_for_habitat){
    
    suppressWarnings({first_year_improved_foraging<-min(which(pars$improved_foraging==1))})
    releaseYears<-releaseYears+first_year_improved_foraging-1
    
  }
  
  if(currentT%in%releaseYears & pars$release_schedule_df$release_size>0){
    
    # Generate unique IDs for all new offspring
    released <- data.frame(
      id = generate_unique_ids(n = pars$release_schedule_df$release_size, existing = pars$all_ids),
      age = pars$release_schedule_df$age_release,
      age_release = pars$release_schedule_df$age_release,
      sex = c("F", "M")[rbinom(n = pars$release_schedule_df$release_size, size = 1, prob = 0.5) + 1],
      subpop = pars$release_schedule_df$subpop,
      t = currentT,
      # t = currentT+1,
      alive = TRUE,
      pair = NA,
      mother_id=NA,
      release_meth=pars$release_schedule_df$release_meth,
      release_time=pars$release_schedule_df$release_time,
      habituation=pars$release_schedule_df$habituation,
      origin=pars$release_schedule_df$origin,
      scot_heritage=pars$release_schedule_df$scot_heritage,
      tsr=0,
      father_id=NA
    )      
  }else{
    
  released<-data.frame()
  }
  
  return(released)
  
}


replace_na_characters <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      col[is.na(col)] <- ""
    }
    return(col)
  })
  return(df)
}

split_evenly_by_col <- function(df, n_groups, target_col) {
  library(dplyr)
  
  df <- df %>%
    arrange(desc(!!sym(target_col)))  # Sort by descending target column value
  
  # Initialize groups
  group_sums <- rep(0, n_groups)
  df$group <- NA  # Column to store group assignment
  
  for (i in 1:nrow(df)) {
    # Find the group with the smallest current sum
    min_group <- which.min(group_sums)
    
    # Assign the current row to that group
    df$group[i] <- min_group
    
    # Update the sum of that group
    group_sums[min_group] <- group_sums[min_group] + df[[target_col]][i]
  }
  
  # Split into a list of sub-dataframes
  split_list <- split(df, df$group)
  
  return(split_list)
}



output_clean_up<-function(rev=FALSE){
  ### Get files in folder
  files<-list.files(path = "./Results",pattern = ".RData",full.names = T)
  
  cat(paste0("\nFound ",length(files)," files\n"))
  
  if(rev){files<-rev(files)}
  if(length(files)>0){
    #  Upload to Gdrive
    for(f in files){
      
      suppressMessages({
        gdrive_upload<-sapply(f,drive_upload, path = as_id("1FSqFfBrvyedxTOUuIFmI44u9tXxEMMU0"))
        uploaded_file<-attributes(gdrive_upload)$dimnames[[2]]
        # Move files to the backup directory
        file.rename(uploaded_file, file.path(backup_dir, basename(f)))
      })
    }
  }
}



output_clean_up_zip <- function(rev = FALSE) {
  # Get files in folder
  files <- list.files(path = "./Results", pattern = "\\.RData$", full.names = TRUE)
  
  cat(paste0("\nFound ", length(files), " files\n"))
  
  if (length(files) == 0) return(invisible(NULL))
  if (rev) files <- rev(files)
  
  # Define zip filename
  zip_filename <- file.path(tempdir(), paste0("results_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"))
  
  # Create zip file containing all files
  zip(zipfile = zip_filename, files = files, flags = "-j")  # -j removes directory structure
  
  suppressMessages({
    # Upload the zip file to Google Drive
    gdrive_upload <- drive_upload(zip_filename, path = as_id("1FSqFfBrvyedxTOUuIFmI44u9tXxEMMU0"))
    
    # Move the zip file to backup directory
    file.rename(zip_filename, file.path(backup_dir, basename(zip_filename)))
    
    # Move original .RData files to backup directory
    file.rename(files, file.path(backup_dir, basename(files)))
  })
}












