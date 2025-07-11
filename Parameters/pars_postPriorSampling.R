# Create a data frame of survival coefficients for adults

model_pars$bio$gen$dip_leth_eq<-prior_rng$diploid_eq[i]

NA_adj<-1/(model_pars$mgmt$release_year_cont%>%filter(is.na(release_time))%>%pull(yr_duration))
smr_adj<-1/(model_pars$mgmt$release_year_cont%>%filter(release_time=="Summer")%>%pull(yr_duration))
wtr_adj<-1/(model_pars$mgmt$release_year_cont%>%filter(release_time=="Winter")%>%pull(yr_duration))


model_pars$bio$surv_coeff <- 
  
  # Start by creating a data frame for adult survival coefficients
  data.frame(
    "Beta_sf" = prior_rng$b_ad_surv_supp_feed[i],             # effect of supplementary feeding
    "Beta_if" = prior_rng$b_ad_surv_imp_for[i],               # effect of improved foraging
    "Beta_b"  = prior_rng$b_ad_surv_imp_for_supp_feed[i]      # interaction effect
  ) %>%
  
  # Add rows for each adult age (from first breeding age to max age)
  merge(data.frame(
    age = model_pars$bio$inherent$age_first_breed:model_pars$bio$inherent$max_age
  )) %>%
  
  # Append survival coefficients for subadult stage (age 2)
  plyr::rbind.fill(data.frame(
    "Beta_sf" = prior_rng$b_sad_surv_supp_feed[i],
    "Beta_if" = prior_rng$b_sad_surv_imp_for[i],
    "Beta_b"  = prior_rng$b_sad_surv_imp_for_supp_feed[i],
    age = 2
  )) %>%
  
  # Append survival coefficients for first-year birds (age 1)
  plyr::rbind.fill(data.frame(
    "Beta_sf" = prior_rng$b_fst_yr_surv_supp_feed[i],
    "Beta_if" = prior_rng$b_fst_yr_surv_imp_for[i],
    "Beta_b"  = prior_rng$b_fst_yr_surv_imp_for_supp_feed[i],
    age = 1
  )) %>%
  
  # Sort all rows by age (1 = first-year, 2 = subadult, 3+ = adult)
  dplyr::arrange(age) %>%
  
  # Add a column for subpopulation (A to E)
  merge(data.frame(subpop = LETTERS[1:5])) %>%
  
  # Add intercept terms (Beta0) by life stage and subpopulation
  left_join(
    
    # Combine baseline survival for each age class and subpopulation
    plyr::rbind.fill(
      
      # First-year survival intercepts for subpops A–E
      data.frame(
        Beta0 = c(
          prior_rng$bl_fst_yr_surv_A[i],
          prior_rng$bl_fst_yr_surv_B[i],
          prior_rng$bl_fst_yr_surv_C[i],
          prior_rng$bl_fst_yr_surv_D[i],
          prior_rng$bl_fst_yr_surv_E[i]
        ),
        subpop = LETTERS[1:5],
        age = 1
      ),
      
      # Subadult survival intercepts (age 2)
      data.frame(
        Beta0 = c(
          prior_rng$bl_sad_surv_A[i],
          prior_rng$bl_sad_surv_B[i],
          prior_rng$bl_sad_surv_C[i],
          prior_rng$bl_sad_surv_D[i],
          prior_rng$bl_sad_surv_E[i]
        ),
        subpop = LETTERS[1:5],
        age = 2
      ),
      
      # Adult survival intercepts (age 3 and above)
      data.frame(
        Beta0 = c(
          prior_rng$bl_ad_surv_A[i],
          prior_rng$bl_ad_surv_B[i],
          prior_rng$bl_ad_surv_C[i],
          prior_rng$bl_ad_surv_D[i],
          prior_rng$bl_ad_surv_E[i]
        ),
        subpop = LETTERS[1:5]
      ) %>%
        # Add age values for adult range
        merge(data.frame(age = model_pars$bio$inherent$age_first_breed:model_pars$bio$inherent$max_age))
    )
  )%>%
  merge(data.frame(origin=c("Wild","Captive")))%>%
  merge(data.frame(release_meth=c(NA,"Staged","Immediate")))%>%
  merge(data.frame(release_time=c(NA,"Summer","Winter")))%>%
  merge(data.frame(habituation=c(NA,0,1)))%>%
  merge(data.frame(age_release=c(NA,1:model_pars$bio$inherent$max_age)))%>%
  # dplyr::filter(! (age_release==1 & (habituation==0 | release_meth=="Immediate")) )%>%
  dplyr::mutate(Beta0=case_when(
    age_release==1 & age==1 & origin == "Wild"    & release_time == "Summer" & habituation == 1 & release_meth == "Staged"~prior_rng$pr_fst_yr_survival_w_h_summer[i]^smr_adj,
    age_release==1 & age==1 & origin == "Wild"    & release_time == "Winter" & habituation == 1 & release_meth == "Staged"~prior_rng$pr_fst_yr_survival_w_h_winter[i]^wtr_adj,
    age_release==1 & age==1 & origin == "Captive" & release_time == "Summer" & habituation == 1 & release_meth == "Staged"~prior_rng$pr_fst_yr_survival_c_h_summer[i]^smr_adj,
    age_release==1 & age==1 & origin == "Captive" & release_time == "Winter" & habituation == 1 & release_meth == "Staged"~prior_rng$pr_fst_yr_survival_c_h_winter[i]^wtr_adj,
    TRUE~Beta0
  ))%>%
  dplyr::mutate(Beta_sf=case_when(
    age_release==1 & age==1 & !is.na(release_time) & !is.na(habituation) & !is.na(release_meth)~0,
    TRUE~Beta_sf
  ))


or_df<-plyr::rbind.fill(
  data.frame(OR_release=1/exp(
    c(prior_rng$logcor_sad_c_st_h[i],
      prior_rng$logcor_sad_c_st_nh[i],
      prior_rng$logcor_sad_w_st_h[i],
      prior_rng$logcor_sad_w_st_nh[i])),
    habituation=c(0,1,0,1),
    origin=c("Captive","Captive","Wild","Wild"),
    age_release=2,
    release_meth="Staged",
    release_time="Winter")%>%
    merge(data.frame(age=2:model_pars$bio$inherent$max_age)),
  data.frame(OR_release=1/exp(
    c(prior_rng$logcor_ad_c_st_h[i],
      prior_rng$logcor_ad_c_st_nh[i],
      prior_rng$logcor_ad_w_im_nh[i])),
    habituation=c(0,1,0),
    origin=c("Captive","Captive","Wild"),
    # age_release=2,
    release_meth=c("Staged","Staged","Immediate"),
    release_time="Winter")%>%
    merge(data.frame(
      age_release = model_pars$bio$inherent$age_first_breed:model_pars$bio$inherent$max_age
    ))%>%
    merge(data.frame(
      age = model_pars$bio$inherent$age_first_breed:model_pars$bio$inherent$max_age
    ))
  )

model_pars$bio$surv_coeff<-model_pars$bio$surv_coeff%>%
  dplyr::left_join(or_df)%>%
  dplyr::filter(!(is.na(OR_release) & !is.na(age_release)))%>%
  dplyr::mutate(OR_release=case_when(
  is.na(OR_release)~1,
  TRUE~OR_release
  ))%>%
  dplyr::ungroup()



model_pars$bio$brood_size_coeff<-data.frame("Beta_sf"=prior_rng$b_brood_size_supp_feed[i],
                                            "Beta_if"=prior_rng$b_brood_size_imp_for[i],
                                            "Beta_b"=prior_rng$b_brood_size_imp_for_supp_feed[i])%>%
  merge(data.frame(subpop=subpops))%>%
  left_join(data.frame("B0"=c(
    prior_rng$bl_brood_size_A[i],
    prior_rng$bl_brood_size_B[i],
    prior_rng$bl_brood_size_C[i],
    prior_rng$bl_brood_size_D[i],
    prior_rng$bl_brood_size_E[i]
  ),subpop=subpops))

model_pars$bio$nest_succ_coeff<-data.frame("Beta_sf"=prior_rng$b_nest_succ_supp_feed[i],
                    "Beta_if"=prior_rng$b_nest_succ_imp_for[i],
                    "Beta_b"=prior_rng$b_nest_succ_imp_for_supp_feed[i])%>%
  merge(data.frame(subpop=subpops))%>%
  left_join(model_pars$bio$nest_succ_df)


model_pars$mgmt$acc_period_df<-rbind.fill(
  data.frame(acc_period=c(prior_rng$acc_period_sad_c_st_h[i],
                          prior_rng$acc_period_sad_c_st_nh[i],
                          prior_rng$acc_period_sad_w_st_h[i],
                          prior_rng$acc_period_sad_w_st_nh[i]),
             habituation=c(0,1,0,1),
             origin=c("Captive","Captive","Wild","Wild"),
             age_release=2,
             release_meth="Staged",
             release_time="Winter"),
  data.frame(acc_period=c(prior_rng$acc_period_ad_c_st_h[i],
                          prior_rng$acc_period_ad_c_st_nh[i],
                          prior_rng$acc_period_ad_w_im_nh[i]),
             habituation=c(0,1,0),
             origin=c("Captive","Captive","Wild"),
             release_meth=c("Staged","Staged","Immediate"),
             release_time="Winter")%>%
    merge(data.frame(age_release = model_pars$bio$inherent$age_first_breed:model_pars$bio$inherent$max_age)))


model_pars$mgmt$transp_fl_OR<-exp(prior_rng$logcor_egg_fledg[i])

model_pars$mgmt$prob_nest_aband<-prior_rng$prob_nest_aband[i]


model_pars$mgmt$prob_for_imp <- prior_rng$prob_imp_for[i] 
model_pars$mgmt$year_for_imp <- prior_rng$year_imp_for[i] 



startK


