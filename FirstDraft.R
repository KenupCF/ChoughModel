require(plyr)
require(dplyr)
require(tidyr)
require(magrittr)

model_pars<-list(priors=list(),sim=list(),bio=list(),mgmt=list())


load("./Data/Expert_Elicitation_Aggregation.RData")

devtools::source_url("https://raw.githubusercontent.com/KenupCF/Kenup-Repo/master/multiUserPERTplot.R")

devtools::source_url("https://raw.githubusercontent.com/KenupCF/Kenup-Repo/master/phd_experimental_functions.R")

devtools::source_url("https://raw.githubusercontent.com/KenupCF/Kenup-Repo/master/Quick%20Functions.R")

source("./Parameters/SimPars.R")
source("./Parameters/BioPars.R")
source("./Parameters/MgmtPars.R")
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
source("./Parameters/priorHandling.R")

init_pop<-init_population(pars)
init_pop<-pairing(pop=init_pop,currentT = 0,pars=pars)
pop<-init_pop

# for( i in 1:nrow(prior_rng)){
i<-1
source("./Parameters/pars_postPriorSampling.R")

start_conditions<-list(init_pop=pop)  
  
### wrap this into a model function  
  
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
  
}
pedigree_df <- pop %>%
  dplyr::filter(!duplicated(id))%>%
  dplyr::select(id, mother_id, father_id,sex)


pop%>%
  dplyr::group_by(t)%>%
  dplyr::summarise(N=sum(alive))%>%
  dplyr::pull(N)%>%
  barplot()
