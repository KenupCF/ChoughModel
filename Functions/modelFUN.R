run_model<-function(start_conditions,model_pars){
  
  n_years<-model_pars$sim$n_years
  
  pop<-start_conditions$init_pop
  
  pars<-list()
  pars$max_age<-model_pars$bio$inherent$max_age
  pars$breeding_age<-model_pars$bio$inherent$age_first_breed
  pars$sex_ratio<-model_pars$bio$inherent$sex_ratio
  pars$max_brood_size<-model_pars$bio$inherent$max_prog_per_brood
  pars$all_ids<-unique(pop$id)
  
  pars$phi_df<-model_pars$bio$surv_coeff
  pars$nesting_success_df<-model_pars$bio$nest_succ_coeff
  pars$brood_size_df<-model_pars$bio$brood_size_coeff
  
  year_imp_for<-model_pars$mgmt$year_for_imp
  bin_imp_for<-rbinom(n = 1,size = 1,prob = model_pars$mgmt$prob_for_imp)
  pars$improved_foraging<-0
  if(bin_imp_for==0){year_imp_for<-Inf}
  
  for(j in 1:n_years){
    
    pars$improved_foraging<-pmin(1,pmax(0,j-year_imp_for+1))
    
    pars$all_ids<-unique(pop$id)
    
    currentPop0<-pop%>%
      dplyr::filter(t==j-1,alive)%>%
      dplyr::mutate(t=j)
    
    currentPop1<-mortality_aging(pop=currentPop0,currentT = j,pars=pars)
    
    currentPop2<-unpair_if_dead(pop=currentPop1,currentT = j)
    
    currentPop3<-pairing(pop=currentPop2,currentT = j,pars=pars)
    
    currentPop4<-recruitment(pop=currentPop3,currentT = j,pars=pars)
    
    currentPop5<-dispersal(pop=currentPop4,currentT = j,pars=pars)
    
    
    pop<-plyr::rbind.fill(pop,currentPop5)
    
  }
  
}