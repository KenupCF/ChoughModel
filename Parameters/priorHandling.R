qFUN<-agg_info$LP$mc_q_functions #define the quantile sampling function (can be from the linear-pooled distributions or refitted ones)
n_samples_mc<-model_pars$sim$n_samples_quantile_function


#### Post-Release Info

### Acclimation period

prior_rng$acc_period_ad_w_im_nh<-qFUN$`acc_period_adult_Wild-Im`(prior_rng$Q_acc_period,n_samples = n_samples_mc)
prior_rng$acc_period_ad_c_st_h<-qFUN$`acc_period_adult_Captive-St-H`(prior_rng$Q_acc_period,n_samples = n_samples_mc)
prior_rng$acc_period_ad_c_st_nh<-qFUN$`acc_period_adult_Captive-St-NH`(prior_rng$Q_acc_period,n_samples = n_samples_mc)

prior_rng$acc_period_sad_w_st_h<-qFUN$`acc_period_subadult_Wild-St-H`(prior_rng$Q_acc_period,n_samples = n_samples_mc)
prior_rng$acc_period_sad_w_st_nh<-qFUN$`acc_period_subadult_Wild-St-NH`(prior_rng$Q_acc_period,n_samples = n_samples_mc)
prior_rng$acc_period_sad_c_st_h<-qFUN$`acc_period_subadult_Captive-St-H`(prior_rng$Q_acc_period,n_samples = n_samples_mc)
prior_rng$acc_period_sad_c_st_nh<-qFUN$`acc_period_subadult_Captive-St-NH`(prior_rng$Q_acc_period,n_samples = n_samples_mc)

### Cost of Release

prior_rng$logcor_ad_w_im_nh<-qFUN$`adult_pr_survival_cor_Wild-Im`(prior_rng$Q_surv_cor,n_samples = n_samples_mc)
prior_rng$logcor_ad_c_st_h<-qFUN$`adult_pr_survival_cor_Captive-St-H`(prior_rng$Q_surv_cor,n_samples = n_samples_mc)
prior_rng$logcor_ad_c_st_nh<-qFUN$`adult_pr_survival_cor_Captive-St-NH`(prior_rng$Q_surv_cor,n_samples = n_samples_mc)

prior_rng$logcor_sad_w_st_h<-qFUN$`subadult_pr_survival_cor_Wild-St-H`(prior_rng$Q_surv_cor,n_samples = n_samples_mc)
prior_rng$logcor_sad_w_st_nh<-qFUN$`subadult_pr_survival_cor_Wild-St-NH`(prior_rng$Q_surv_cor,n_samples = n_samples_mc)
prior_rng$logcor_sad_c_st_h<-qFUN$`subadult_pr_survival_cor_Captive-St-H`(prior_rng$Q_surv_cor,n_samples = n_samples_mc)
prior_rng$logcor_sad_c_st_nh<-qFUN$`subadult_pr_survival_cor_Captive-St-NH`(prior_rng$Q_surv_cor,n_samples = n_samples_mc)

prior_rng$logcor_egg_fledg<-qFUN$`egg_fl_prob_cor_Implanted eggs fledging odds`(prior_rng$Q_surv_cor,n_samples = n_samples_mc)
