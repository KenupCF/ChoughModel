# Define the quantile sampling function from the linear-pooled or refitted distributions
# qFUN <- agg_info$LP$mc_q_functions 

# Number of samples to draw from each quantile function (used for Monte Carlo simulation)
n_samples_mc <- model_pars$sim$n_samples_quantile_function

## -----------------------------
## Basline Demographic Parameters
## -----------------------------

prior_rng$bl_fst_yr_surv_A<-qFUN$`surv A_1`(x = prior_rng$Q_bl_surv)
prior_rng$bl_fst_yr_surv_B<-qFUN$`surv B_1`(x = prior_rng$Q_bl_surv)
prior_rng$bl_fst_yr_surv_C<-qFUN$`surv C_1`(x = prior_rng$Q_bl_surv)
prior_rng$bl_fst_yr_surv_D<-qFUN$`surv D_1`(x = prior_rng$Q_bl_surv)
prior_rng$bl_fst_yr_surv_E<-qFUN$`surv E_1`(x = prior_rng$Q_bl_surv)

prior_rng$bl_sad_surv_A<-qFUN$`surv A_2`(x = prior_rng$Q_bl_surv)
prior_rng$bl_sad_surv_B<-qFUN$`surv B_2`(x = prior_rng$Q_bl_surv)
prior_rng$bl_sad_surv_C<-qFUN$`surv C_2`(x = prior_rng$Q_bl_surv)
prior_rng$bl_sad_surv_D<-qFUN$`surv D_2`(x = prior_rng$Q_bl_surv)
prior_rng$bl_sad_surv_E<-qFUN$`surv E_2`(x = prior_rng$Q_bl_surv)

prior_rng$bl_ad_surv_A<-qFUN$`surv A_ad`(x = prior_rng$Q_bl_surv)
prior_rng$bl_ad_surv_B<-qFUN$`surv B_ad`(x = prior_rng$Q_bl_surv)
prior_rng$bl_ad_surv_C<-qFUN$`surv C_ad`(x = prior_rng$Q_bl_surv)
prior_rng$bl_ad_surv_D<-qFUN$`surv D_ad`(x = prior_rng$Q_bl_surv)
prior_rng$bl_ad_surv_E<-qFUN$`surv E_ad`(x = prior_rng$Q_bl_surv)

prior_rng$bl_ad_surv_A<-qFUN$`surv A_ad`(x = prior_rng$Q_bl_surv)
prior_rng$bl_ad_surv_B<-qFUN$`surv B_ad`(x = prior_rng$Q_bl_surv)
prior_rng$bl_ad_surv_C<-qFUN$`surv C_ad`(x = prior_rng$Q_bl_surv)
prior_rng$bl_ad_surv_D<-qFUN$`surv D_ad`(x = prior_rng$Q_bl_surv)
prior_rng$bl_ad_surv_E<-qFUN$`surv E_ad`(x = prior_rng$Q_bl_surv)

prior_rng$bl_brood_size_A<-qFUN$brood_A(x = prior_rng$Q_bl_brood) %>% log
prior_rng$bl_brood_size_B<-qFUN$brood_B(x = prior_rng$Q_bl_brood) %>% log
prior_rng$bl_brood_size_C<-qFUN$brood_C(x = prior_rng$Q_bl_brood) %>% log
prior_rng$bl_brood_size_D<-qFUN$brood_D(x = prior_rng$Q_bl_brood) %>% log
prior_rng$bl_brood_size_E<-qFUN$brood_E(x = prior_rng$Q_bl_brood) %>% log

## -----------------------------
## Habitat improvement forecasting
## -----------------------------

prior_rng$prob_imp_for <- qFUN$`improved_foraging_Probability of improved conditions`(prior_rng$Q_improved_foraging)
prior_rng$year_imp_for <- qFUN$`foraging_improvement_year_Improved conditions`(prior_rng$Q_improved_foraging_year)

## -----------------------------
## Habitat-related Demographic Parameters
## -----------------------------

# Juvenile survival under different habitat management scenarios
prior_rng$fst_yr_surv_no_mgmt    <- qFUN$`juv_survival_mgmt_No management`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% logit
prior_rng$fst_yr_surv_supp_feed  <- qFUN$`juv_survival_mgmt_Supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% logit
prior_rng$fst_yr_surv_imp_for    <- qFUN$`juv_survival_mgmt_Restored foraging`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% logit
prior_rng$fst_yr_surv_both       <- qFUN$`juv_survival_mgmt_Restored foraging + supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% logit

# Calculate coefficient 
prior_rng$b_fst_yr_surv_supp_feed         <- prior_rng$fst_yr_surv_supp_feed - prior_rng$fst_yr_surv_no_mgmt
prior_rng$b_fst_yr_surv_imp_for           <- prior_rng$fst_yr_surv_imp_for   -  prior_rng$fst_yr_surv_no_mgmt
prior_rng$b_fst_yr_surv_imp_for_supp_feed <- prior_rng$fst_yr_surv_both - prior_rng$fst_yr_surv_supp_feed - prior_rng$fst_yr_surv_imp_for + prior_rng$fst_yr_surv_no_mgmt

# Subadult survival under different habitat management scenarios
prior_rng$sad_surv_no_mgmt       <- qFUN$`subadult_survival_mgmt_No management`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% logit
prior_rng$sad_surv_supp_feed     <- qFUN$`subadult_survival_mgmt_Supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% logit
prior_rng$sad_surv_imp_for       <- qFUN$`subadult_survival_mgmt_Restored foraging`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% logit
prior_rng$sad_surv_both          <- qFUN$`subadult_survival_mgmt_Restored foraging + supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% logit

# Calculate coefficient 
prior_rng$b_sad_surv_supp_feed         <- prior_rng$sad_surv_supp_feed - prior_rng$sad_surv_no_mgmt
prior_rng$b_sad_surv_imp_for           <- prior_rng$sad_surv_imp_for   -  prior_rng$sad_surv_no_mgmt
prior_rng$b_sad_surv_imp_for_supp_feed <- prior_rng$sad_surv_both - prior_rng$sad_surv_supp_feed - prior_rng$sad_surv_imp_for + prior_rng$sad_surv_no_mgmt

# Adult survival under different habitat management scenarios
prior_rng$ad_surv_no_mgmt        <- qFUN$`adult_survival_mgmt_No management`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$ad_surv_supp_feed      <- qFUN$`adult_survival_mgmt_Supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$ad_surv_imp_for        <- qFUN$`adult_survival_mgmt_Restored foraging`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$ad_surv_both           <- qFUN$`adult_survival_mgmt_Restored foraging + supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)


# Calculate coefficient 
prior_rng$b_ad_surv_supp_feed         <- prior_rng$ad_surv_supp_feed - prior_rng$ad_surv_no_mgmt
prior_rng$b_ad_surv_imp_for           <- prior_rng$ad_surv_imp_for   -  prior_rng$ad_surv_no_mgmt
prior_rng$b_ad_surv_imp_for_supp_feed <- prior_rng$ad_surv_both - prior_rng$ad_surv_supp_feed - prior_rng$ad_surv_imp_for + prior_rng$ad_surv_no_mgmt

# Nesting success under different habitat management scenarios
prior_rng$nest_succ_no_mgmt      <- qFUN$`nesting_success_No management`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% logit
prior_rng$nest_succ_supp_feed    <- qFUN$`nesting_success_Supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% logit
prior_rng$nest_succ_imp_for      <- qFUN$`nesting_success_Restored foraging`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% logit
prior_rng$nest_succ_both         <- qFUN$`nesting_success_Restored foraging + supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% logit

# Calculate coefficient 
prior_rng$b_nest_succ_supp_feed         <- prior_rng$nest_succ_supp_feed - prior_rng$nest_succ_no_mgmt
prior_rng$b_nest_succ_imp_for           <- prior_rng$nest_succ_imp_for   -  prior_rng$nest_succ_no_mgmt
prior_rng$b_nest_succ_imp_for_supp_feed <- prior_rng$nest_succ_both - prior_rng$nest_succ_supp_feed - prior_rng$nest_succ_imp_for + prior_rng$nest_succ_no_mgmt

# Conditional brood size under different habitat management scenarios
prior_rng$brood_size_no_mgmt     <- qFUN$`conditional_brood_size_No management`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% log
prior_rng$brood_size_supp_feed   <- qFUN$`conditional_brood_size_Supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% log
prior_rng$brood_size_imp_for     <- qFUN$`conditional_brood_size_Restored foraging`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% log
prior_rng$brood_size_both        <- qFUN$`conditional_brood_size_Restored foraging + supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc) %>% log

# Calculate coefficient 
prior_rng$b_brood_size_supp_feed         <- prior_rng$brood_size_supp_feed - prior_rng$brood_size_no_mgmt
prior_rng$b_brood_size_imp_for           <- prior_rng$brood_size_imp_for   -  prior_rng$brood_size_no_mgmt
prior_rng$b_brood_size_imp_for_supp_feed <- prior_rng$brood_size_both - prior_rng$brood_size_supp_feed - prior_rng$brood_size_imp_for + prior_rng$brood_size_no_mgmt


## -----------------------------
## Post-Release Demographic Parameters
## -----------------------------


prior_rng$pr_fst_yr_survival_c_h_summer<-(qFUN$juv_surv_kent_strat_Captive(prior_rng$Q_bl_surv)) %>% logit
prior_rng$pr_fst_yr_survival_w_h_summer<-(qFUN$juv_surv_kent_strat_Wild(prior_rng$Q_bl_surv)) %>% logit
prior_rng$pr_fst_yr_survival_c_h_winter<-(qFUN$juv_surv_kent_strat_winter_Captive(prior_rng$Q_bl_surv)) %>% logit
prior_rng$pr_fst_yr_survival_w_h_winter<-(qFUN$juv_surv_kent_strat_winter_Wild(prior_rng$Q_bl_surv)) %>% logit



### Acclimation period (converted to years from months)

# Adults
prior_rng$acc_period_ad_w_im_nh  <- qFUN$`acc_period_adult_Wild-Im`(prior_rng$Q_acc_period, n_samples = n_samples_mc) / 12
prior_rng$acc_period_ad_c_st_h   <- qFUN$`acc_period_adult_Captive-St-H`(prior_rng$Q_acc_period, n_samples = n_samples_mc) / 12
prior_rng$acc_period_ad_c_st_nh  <- qFUN$`acc_period_adult_Captive-St-NH`(prior_rng$Q_acc_period, n_samples = n_samples_mc) / 12

# Subadults
prior_rng$acc_period_sad_w_st_h  <- qFUN$`acc_period_subadult_Wild-St-H`(prior_rng$Q_acc_period, n_samples = n_samples_mc) / 12
prior_rng$acc_period_sad_w_st_nh <- qFUN$`acc_period_subadult_Wild-St-NH`(prior_rng$Q_acc_period, n_samples = n_samples_mc) / 12
prior_rng$acc_period_sad_c_st_h  <- qFUN$`acc_period_subadult_Captive-St-H`(prior_rng$Q_acc_period, n_samples = n_samples_mc) / 12
prior_rng$acc_period_sad_c_st_nh <- qFUN$`acc_period_subadult_Captive-St-NH`(prior_rng$Q_acc_period, n_samples = n_samples_mc) / 12


### Cost of Release (log odds of post-release survival correction)

# Adults
prior_rng$logcor_ad_w_im_nh      <- qFUN$`adult_pr_survival_cor_Wild-Im`(prior_rng$Q_surv_cor, n_samples = n_samples_mc)
prior_rng$logcor_ad_c_st_h       <- qFUN$`adult_pr_survival_cor_Captive-St-H`(prior_rng$Q_surv_cor, n_samples = n_samples_mc)
prior_rng$logcor_ad_c_st_nh      <- qFUN$`adult_pr_survival_cor_Captive-St-NH`(prior_rng$Q_surv_cor, n_samples = n_samples_mc)

# Subadults
prior_rng$logcor_sad_w_st_h      <- qFUN$`subadult_pr_survival_cor_Wild-St-H`(prior_rng$Q_surv_cor, n_samples = n_samples_mc)
prior_rng$logcor_sad_w_st_nh     <- qFUN$`subadult_pr_survival_cor_Wild-St-NH`(prior_rng$Q_surv_cor, n_samples = n_samples_mc)
prior_rng$logcor_sad_c_st_h      <- qFUN$`subadult_pr_survival_cor_Captive-St-H`(prior_rng$Q_surv_cor, n_samples = n_samples_mc)
prior_rng$logcor_sad_c_st_nh     <- qFUN$`subadult_pr_survival_cor_Captive-St-NH`(prior_rng$Q_surv_cor, n_samples = n_samples_mc)

# Egg-to-fledgling success correction
prior_rng$logcor_egg_fledg       <- qFUN$`egg_fl_prob_cor_Implanted eggs fledging odds`(prior_rng$Q_surv_cor, n_samples = n_samples_mc)


prior_rng$prob_nest_aband        <- qFUN$`nest_aband_prob_Nest abandonment`(q = prior_rng$Q_nest_abandonment,n_samples = n_samples_mc)
