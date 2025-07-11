run_model<-function(start_conditions,model_pars){
  
  start.<-now()
  n_years<-model_pars$sim$n_years
  
  
  pars<-list()
  
  pars$all_ids<-unique(pop$id)
  
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
  
  pars$founder_ids<-unique(pop$id)
  
  j<-1
  
  # while(j <= n_years){
  while(j <= 8){
      
    pars$all_ids<-unique(pop$id)
    
    currentPop0<-pop%>%
      dplyr::filter(t==j-1,alive)%>%
      dplyr::mutate(t=j)
    
    released<-releases(pars=pars,currentT = j)
    
    pars$full_pop<-plyr::rbind.fill(pop,released)
    
    currentPop0<-plyr::rbind.fill(currentPop0,released)
    
    pars$all_ids<-unique(c(pars$all_ids,pars$full_pop$id))
    
    currentPop0<-currentPop0%>%
      dplyr::mutate(Fi=NULL)%>%
      dplyr::left_join(calculate_inbreeding(pop_df=pars$full_pop,pars=pars))
    
    currentPop1<-mortality_aging(pop=currentPop0,currentT = j,pars=pars)
    
    currentPop2<-unpair_if_dead(pop=currentPop1,currentT = j)
    
    currentPop3<-pairing(pop=currentPop2,currentT = j,pars=pars)
    
    currentPop4<-dispersal(pop=currentPop3,currentT = j,pars=pars)
    
    born_resu<-recruitment(pop=currentPop4,currentT = j,pars=pars)
    
    
    pop<-plyr::rbind.fill(pop,currentPop4,born_resu$born)%>%
      tidy_pop_df()
    
    
    
    print(j)
  j<-j+1
  }
  # })
  
}