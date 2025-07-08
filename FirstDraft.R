require(plyr)
require(dplyr)
require(tidyr)
require(magrittr)

model_pars<-list(priors=list(),sim=list())


source("./Parameters/BioPars.R")
source("./Parameters/MgmtPars.R")
source("./Parameters/SimPars.R")
source("./Functions/FUN.R")






pars<-list(StartN=rep(10,5),
           sex_ratio=0.5,
           no_age_classes=17,
           max_age=17,
           breeding_age=3,
           nesting_success_df=nest_success_df,
           dispersalMat=dispersalMat,
           dispersalAges=2,
           max_brood_size=5,
           av_brood_size=2.6
           # ,phi_df=mort_long
           )




### Sample uncertain parameters, given the description of their distribution
prior_rng<-priorSampling(model_pars$priors,
                         seed = 26051991,
                         # size=2)%>%
                         size=model_pars$sim$n_iter)


init_pop<-init_population(pars)
init_pop<-pairing(pop=init_pop,currentT = 0,pars=pars)
pop<-init_pop

suppressMessages({
for(j in 1:50){

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
})
pedigree_df <- pop %>%
  dplyr::filter(!duplicated(id))%>%
  dplyr::select(id, mother_id, father_id,sex)


pop%>%
  dplyr::group_by(t)%>%
  dplyr::summarise(N=sum(alive))%>%
  dplyr::pull(N)%>%
  barplot()
