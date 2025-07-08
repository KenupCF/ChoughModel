# Define the quantile sampling function from the linear-pooled or refitted distributions
qFUN <- agg_info$LP$mc_q_functions 

# Number of samples to draw from each quantile function (used for Monte Carlo simulation)
n_samples_mc <- model_pars$sim$n_samples_quantile_function

## -----------------------------
## Habitat-related Demographic Parameters
## -----------------------------

# Juvenile survival under different habitat management scenarios
prior_rng$fst_yr_surv_no_mgmt    <- qFUN$`juv_survival_mgmt_No management`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$fst_yr_surv_supp_feed  <- qFUN$`juv_survival_mgmt_Supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$fst_yr_surv_imp_for    <- qFUN$`juv_survival_mgmt_Restored foraging`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$fst_yr_surv_both       <- qFUN$`juv_survival_mgmt_Restored foraging + supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)

# Subadult survival under different habitat management scenarios
prior_rng$sad_surv_no_mgmt       <- qFUN$`subadult_survival_mgmt_No management`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$sad_surv_supp_feed     <- qFUN$`subadult_survival_mgmt_Supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$sad_surv_imp_for       <- qFUN$`subadult_survival_mgmt_Restored foraging`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$sad_surv_both          <- qFUN$`subadult_survival_mgmt_Restored foraging + supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)

# Adult survival under different habitat management scenarios
prior_rng$ad_surv_no_mgmt        <- qFUN$`adult_survival_mgmt_No management`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$ad_surv_supp_feed      <- qFUN$`adult_survival_mgmt_Supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$ad_surv_imp_for        <- qFUN$`adult_survival_mgmt_Restored foraging`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$ad_surv_both           <- qFUN$`adult_survival_mgmt_Restored foraging + supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)

# Nesting success under different habitat management scenarios
prior_rng$nest_succ_no_mgmt      <- qFUN$`nesting_success_No management`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$nest_succ_supp_feed    <- qFUN$`nesting_success_Supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$nest_succ_imp_for      <- qFUN$`nesting_success_Restored foraging`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$nest_succ_both         <- qFUN$`nesting_success_Restored foraging + supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)

# Conditional brood size under different habitat management scenarios
prior_rng$brood_size_no_mgmt     <- qFUN$`conditional_brood_size_No management`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$brood_size_supp_feed   <- qFUN$`conditional_brood_size_Supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$brood_size_imp_for     <- qFUN$`conditional_brood_size_Restored foraging`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)
prior_rng$brood_size_both        <- qFUN$`conditional_brood_size_Restored foraging + supplementary feeding`(prior_rng$Q_habitat_effect_size, n_samples = n_samples_mc)

## -----------------------------
## Post-Release Demographic Parameters
## -----------------------------

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
