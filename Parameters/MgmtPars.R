

model_pars$priors$Q_acc_period<-qrunif_template

model_pars$priors$Q_surv_cor<-qrunif_template

model_pars$priors$Q_nest_abandonment<-qrunif_template

model_pars$priors$Q_habitat_effect_size<-qrunif_template

model_pars$priors$Q_bl_surv  <- qrunif_template
model_pars$priors$Q_bl_brood <- qrunif_template
model_pars$priors$Q_bl_nest  <- qrunif_template

model_pars$priors$Q_improved_foraging      <- qrunif_template
model_pars$priors$Q_improved_foraging_year <- qrunif_template


model_pars$mgmt$release_year_cont<-data.frame(release_time=c(NA,"Summer","Winter"),yr_duration=c(1,1,1/2))
