
model_pars$sim<-list()

model_pars$sim$n_years<-50
model_pars$sim$n_iter<-5
model_pars$sim$parametric_uncertainty<-FALSE
model_pars$sim$n_samples_quantile_function<-1e4

# template for quantile extraction (0 to 1 means all quantiles are sampled, 0.5 means to always get the median)
if(model_pars$sim$parametric_uncertainty){
  qrunif_template<-data.frame(min=0.0,max=1.0,dist="unif")
}else{
  qrunif_template<-data.frame(min=0.5,max=0.5,dist="unif")
}



qFUN <- agg_info$LP$mc_q_functions 

