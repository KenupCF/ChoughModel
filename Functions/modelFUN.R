run_model<-function(start_conditions,model_pars,idx=NA){

  
  start.<-now()
  n_years<-model_pars$sim$n_years
  
  
  pars<-list()
  
  pars$start_cycle<-model_pars$sim$start_cycle
  
  pars$max_age<-model_pars$bio$inherent$max_age
  pars$breeding_age<-model_pars$bio$inherent$age_first_breed
  pars$sex_ratio<-model_pars$bio$inherent$sex_ratio
  pars$max_brood_size<-model_pars$bio$inherent$max_prog_per_brood
  pars$av_clutch_size<-model_pars$bio$inherent$av_clutch_size
  
  pars$Fp<-model_pars$bio$gen$starting_inbreeding
  pars$founder_kinship    <-model_pars$bio$gen$founder_kinship
  pars$dip_leth_eq<-model_pars$bio$gen$dip_leth_eq
  
  pars$supp_feeding_df<-model_pars$mgmt$supp_feeding_df
  
  pars$acc_period_df      <-model_pars$mgmt$acc_period_df
  pars$release_year_cont  <-model_pars$mgmt$release_year_cont
  pars$release_schedule_df<-model_pars$mgmt$release_schedule
  
  
  pars$eggs_replaced_fem_ids<-character()
  pars$prob_nest_aband<-model_pars$mgmt$prob_nest_aband
  pars$transp_fl_OR<-model_pars$mgmt$transp_fl_OR
  pars$no_eggs_replaced<-NA
  
  
  pars$carr_capac_df<-model_pars$bio$carr_capac_df
  pars$phi_df<-model_pars$bio$surv_coeff
  pars$nesting_success_df<-model_pars$bio$nest_succ_coeff
  pars$brood_size_df<-model_pars$bio$brood_size_coeff
  
  
  pars$dispersalMat<-model_pars$bio$dispersalMat
  pars$dispersalAges<-2:model_pars$bio$inherent$max_age
  
  
  year_imp_for<-model_pars$mgmt$year_for_imp
  bin_imp_for<-rbinom(n = 1,size = 1,prob = model_pars$mgmt$prob_for_imp)
  
  
  if(bin_imp_for==0){year_imp_for<-Inf}
  
  pars$improved_foraging<-pmin(1,pmax(0,(1:n_years)-year_imp_for+1))
  
  
  pop<-start_conditions$init_pop
  
  pars$all_ids<-pars$founder_ids<-unique(pop$id)
  
  Fi_df<-pop%>%
    dplyr::arrange((is.na(Fi)))%>%
    dplyr::filter(!duplicated(id))%>%
    dplyr::select(id,Fi,scot_heritage)
  
  egg_fate<-data.frame()
  
  j<-1
  suppressMessages({
  while(j <= n_years){
  # while(j <= 15){
      
    pars$all_ids<-unique(pop$id)
    
    ### remove supplementary feeding if there is improved foraging
    pars$supp_feeding_current<-pars$supp_feeding_df
    pars$supp_feeding_current$sf <- pmax(0,model_pars$mgmt$supp_feeding_df$sf - (pars$improved_foraging[j]*model_pars$mgmt$supp_feeding_df$StopAfterIF))
    
    currentPop0<-pop%>%
      dplyr::filter(t==j-1,alive)%>%
      dplyr::mutate(t=j)
    
    released<-releases(pars=pars,currentT = j)%>%
      dplyr::mutate(Fi=0)
    
    pars$full_pop<-plyr::rbind.fill(pop,released)
    
    pars$all_ids<-unique(c(pars$all_ids,pars$full_pop$id))
    
    currentPop0<-currentPop0%>%
      dplyr::mutate(Fi=NULL)%>%
      dplyr::left_join(Fi_df)%>%
      plyr::rbind.fill(released)
    
    if(nrow(currentPop0)==0){break}
    
    # survival_outcome<-mortality_aging(pop=currentPop0,currentT = j,pars=pars)
    survival_outcome<-mortality(pop=currentPop0,currentT = j,pars=pars)
    
    currentPop1<-survival_outcome$alive
    
    currentPop2<-unpair_if_dead(pop=currentPop1,currentT = j)
    
    currentPop3<-pairing(pop=currentPop2,currentT = j,pars=pars)
    
    currentPop4<-dispersal(pop=currentPop3,currentT = j,pars=pars)
    
    born_resu<-recruitment(pop=currentPop4,currentT = j,pars=pars)
    
    egg_fate<-plyr::rbind.fill(egg_fate,born_resu$egg_fate)
    
    currentPop5<-plyr::rbind.fill(currentPop4,born_resu$born)%>%
      aging(currentT = j,pars=pars)
    
    
    pop<-plyr::rbind.fill(pop,currentPop5)%>%
      tidy_pop_df()
    
    Fi_df<-calculate_inbreeding(pop_df = pop,pars = pars)
    
    pop<-pop%>%
      dplyr::mutate(Fi=NULL,scot_heritage=NULL)%>%
      dplyr::left_join(Fi_df)
    
    # print(j)
  
    j<-j+1
  
  }
  })
  
  
  full_ids<-pop%>%
    dplyr::filter(!duplicated(id))%>%
    dplyr::select(id,father_id,mother_id,origin,sex)%>%
    dplyr::left_join(Fi_df)%>%
    dplyr::mutate(i=idx)
  
  kinship<-calculate_kinship(pop_df =full_ids,pars=pars,rm_non_breeders=FALSE)
  
  return(list(pop=pop%>%
                dplyr::mutate(i=idx),
              full_ids=full_ids,
              kinship=kinship,
              egg_fate=egg_fate%>%
                dplyr::mutate(i=idx),
              model_pars=model_pars,
              time_run=difftime(time1 = now(),time2 = start.,units = "secs")
              ))
  
}