require(gtools)
require(stringr)
require(plyr)
require(dplyr)
require(tidyr)
require(magrittr)
require(lubridate)
require(kinship2)



model_pars<-list(priors=list(),sim=list(),bio=list(),mgmt=list())


load("./Data/Expert_Elicitation_Aggregation.RData")

devtools::source_url("https://raw.githubusercontent.com/KenupCF/Kenup-Repo/master/multiUserPERTplot.R")

devtools::source_url("https://raw.githubusercontent.com/KenupCF/Kenup-Repo/master/phd_experimental_functions.R")

devtools::source_url("https://raw.githubusercontent.com/KenupCF/Kenup-Repo/master/Quick%20Functions.R")

source("./Parameters/SimPars.R")
source("./Parameters/BioPars.R")
source("./Parameters/MgmtPars.R")
source("./Functions/FUN.R")
source("./Functions/FUNgen.R")

  
pars<-list(StartN=StartN,
           sex_ratio=0.5,
           no_age_classes=model_pars$bio$inherent$max_age,
           max_age=model_pars$bio$inherent$max_age,
           breeding_age=model_pars$bio$inherent$age_first_breed,
           nesting_success_df=nest_success_df,
           dispersalMat=dispersalMat,
           Fp=model_pars$bio$gen$starting_inbreeding)


### Sample uncertain parameters, given the description of their distribution
prior_rng<-priorSampling(model_pars$priors,
                         seed = 26051991,
                         size=1e3)%>%
                         # size=model_pars$sim$n_iter)%>%
  dplyr::mutate(p = 1:n())

source("./Parameters/priorHandling.R")

mgmt_options<-expand.grid(SuppFeed=c(
  "No",
  "Current"
  ,"Provisional"
  ),ReleaseStrat=rel_strats$r)%>%
  dplyr::mutate(a=1:n())

prior_rng<-merge(prior_rng,mgmt_options)%>%
  dplyr::arrange(p,a)%>%
  dplyr::mutate(i=1:n())


init_pop<-init_population(pars)
init_pop<-pairing(pop=init_pop,currentT = 0,pars=pars)
pop<-init_pop

# for( i in 1:nrow(prior_rng)){
i<-1
source("./Parameters/pars_postPriorSampling.R")

start_conditions<-list(init_pop=pop)
model_pars$mgmt$supp_feeding_df<-model_pars$mgmt$supp_feeding_master[[prior_rng$SuppFeed[i]]]

model_pars$mgmt$release_schedule<-model_pars$mgmt$release_schedule_master%>%
  dplyr::filter(r==prior_rng$ReleaseStrat[i])

# profvis({
resu<-run_model(start_conditions=start_conditions,model_pars=model_pars)
# })



resu%>%
  dplyr::group_by(t)%>%
  dplyr::summarise(N=sum(alive))%>%
  dplyr::pull(N)%>%
  barplot()
