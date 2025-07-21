# Set working directory based on available paths
wd <- "/models/ChoughModel"
if(!dir.exists(wd)){
  wd <- "C:/Users/caiok/Dropbox/03-Work/01-Science/00-Research Projects/ChoughModel"
}
if(!dir.exists(wd)){
  wd <- "C:/Users/Caio.Kenup/ChoughModel"
}
setwd(wd)  # Set the working directory

# Load required packages
source("packageLoader.R")
require(ggplot2)
require(duckdb)

# Connect to DuckDB database
db_path <- "D:/03-Work/01-Science/00-Research Projects/RB Chough Results/results_bigv3b.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)

# Load tables from the DuckDB database
summary <- dbGetQuery(con, "SELECT * FROM summary")
run_pars <- dbGetQuery(con, "SELECT * FROM run_pars")


hb_labels<-c("No habitat improvement","Habitat improved")

# Add habitat scenario labels
run_pars <- run_pars %>%
  dplyr::mutate(Habitat_Scenario = case_when(
    prob_imp_for == 0 ~  hb_labels[1],
    prob_imp_for == 1 ~  hb_labels[2]
  ))

# Load management strategies
mgmt <- dbGetQuery(con, "SELECT * FROM mgmt")

dbDisconnect(con, shutdown = TRUE)

# Derive additional labels and release strategy names
mgmt <- mgmt %>%
  dplyr::mutate(
    Individuals_Released = case_when(
      noEggsReleased > 0 ~ noEggsReleased,
      TRUE ~ release_size
    ),
    Age_Release_String = case_when(
      Individuals_Released == 0 ~ "No releases",
      noEggsReleased > 0 & nest_aband_allowed ~ "Eggs",
      noEggsReleased > 0 & !nest_aband_allowed ~ "Eggs+",
      age_release == 1 ~ "First year",
      age_release == 2 ~ "Sub-adults",
      age_release == 3 ~ "Adults"
    ),
    Origin_String = substr(origin, 1, 1),
    Method_String = substr(release_meth, 1, 2),
    Habituation_String = case_when(
      habituation == 0 ~ "NH",
      habituation == 1 ~ "H"
    )
  ) %>%
  dplyr::mutate(
    Release_Strategy_Name = case_when(
      Individuals_Released == 0 ~ "No releases",
      str_detect(Age_Release_String, "Eggs") ~ Age_Release_String,
      age_release == 1 ~ paste0(Age_Release_String, " - ", release_time, 
                                " (", Origin_String, "-", Method_String, "-", Habituation_String, ")"),
      age_release %in% c(2, 3) ~ paste0(Age_Release_String, 
                                        " (", Origin_String, "-", Method_String, "-", Habituation_String, ")"),
      TRUE ~ NA
    ),
    Release_Time_String = case_when(
      release_size == 0 & noEggsReleased == 0 ~ "No releases",
      wait_for_habitat ~ "Wait",
      !wait_for_habitat ~ "Now"
    ),
    SuppFeed = case_when(
      SuppFeed == "Current" ~ "Continuous",
      SuppFeed == "Provisional" ~ "Temporary"
    )
  )

# Define custom order for plotting strategies
release_order <- c(
  "No releases",
  "Eggs", "Eggs+",
  "First year - Summer (C-St-H)", "First year - Summer (W-St-H)",
  "First year - Winter (C-St-H)", "First year - Winter (W-St-H)",
  "Sub-adults (C-St-H)", "Sub-adults (W-St-H)",
  "Sub-adults (C-St-NH)", "Sub-adults (W-St-NH)",
  "Adults (C-St-H)", "Adults (C-St-NH)", "Adults (W-Im-NH)"
)

# Define custom order for plotting strategies
age_order <- c(
  "No releases",
  "Eggs", "Eggs+",
  "First year",
  "Sub-adults",
  "Adults"
)

# Prepare trimmed version of mgmt data with selected variables
mgmt_trim <- mgmt %>%
  dplyr::select(Release_Strategy_Name, Release_Time_String, SuppFeed,release_time,
                Individuals_Released, i, folder, Age_Release_String) %>%
  dplyr::left_join(run_pars %>%
                     dplyr::select(i, folder, Habitat_Scenario, alt))

# Get one row per alternative to summarize management strategies
mgmt_summ <- mgmt_trim %>%
  dplyr::filter(!duplicated(alt)) %>%
  dplyr::select(alt, Release_Strategy_Name, Release_Time_String, release_time,
                SuppFeed, Age_Release_String, Habitat_Scenario)

# Identify strategies paired by habitat scenario
scenario_alt_map <- mgmt_summ %>%
  dplyr::group_by(across(-c(alt, Habitat_Scenario))) %>%
  dplyr::mutate(alt_hs = dplyr::cur_group_id()) %>%
  dplyr::ungroup()%>%
  dplyr::select(alt,alt_hs)%>%
  dplyr::ungroup()

# Identify strategies paired by supplementary feeding
supp_feed_alt_map <- mgmt_summ %>%
  dplyr::group_by(across(-c(alt, SuppFeed))) %>%
  dplyr::mutate(alt_sf = dplyr::cur_group_id()) %>%
  dplyr::ungroup()%>%
  dplyr::select(alt,alt_sf)

supp_release_time_map <- mgmt_summ %>%
  dplyr::group_by(across(-c(alt, Release_Time_String))) %>%
  dplyr::mutate(alt_rt = dplyr::cur_group_id()) %>%
  dplyr::ungroup()%>%
  dplyr::select(alt,alt_rt)


mgmt_summ<-mgmt_summ%>%
  left_join(scenario_alt_map)%>%
  left_join(supp_feed_alt_map)%>%
  left_join(supp_release_time_map)

mgmt_summ$Release_Strategy_Name<-factor(mgmt_summ$Release_Strategy_Name,release_order)  
mgmt_summ$Age_Release_String<-factor(mgmt_summ$Age_Release_String,age_order)  


mgmt_summ$kept<-mgmt_summ$SuppFeed=="Continuous" & mgmt_summ$Age_Release_String!="Eggs"

# Merge summary data with management and run indexes
dat <- summary %>%
  dplyr::left_join(mgmt_trim%>%select(i,alt))%>%
  dplyr::left_join(mgmt_summ)%>%
  dplyr::left_join(run_pars %>% dplyr::select(i, p, alt))
  

# Summarise results by strategy, habitat and feeding
resu <- dat %>%
  dplyr::group_by(alt) %>%
  dplyr::summarise(
    noRuns=n(),
    probPersist = 1 - mean(extinct),
    probExtinct = 1 - probPersist,
    propPopulationsDeclining = mean(trend < 1, na.rm = TRUE),
    avTrend = mean(trend, na.rm = TRUE),
    lclTrend = quantile(trend, 0.025, na.rm = TRUE),
    uclTrend = quantile(trend, 0.975, na.rm = TRUE),
    avFinalN = mean(finalN * (!extinct)),
    lclFinalN = quantile(finalN * (!extinct), 0.025, na.rm = TRUE),
    uclFinalN = quantile(finalN * (!extinct), 0.975, na.rm = TRUE),
    avDeathReleases = mean(release_deaths, na.rm = TRUE),
    lclDeathReleases = quantile(release_deaths, 0.025, na.rm = TRUE),
    uclDeathReleases = quantile(release_deaths, 0.975, na.rm = TRUE),
    avFp = mean(Fp),
    lclFp = quantile(Fp, 0.025, na.rm = TRUE),
    uclFp = quantile(Fp, 0.975, na.rm = TRUE),
    avSp = mean(Sp),
    lclSp = quantile(Sp, 0.025, na.rm = TRUE),
    uclSp = quantile(Sp, 0.975, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    reportTrend = sprintf("%.2f (%.2f-%.2f)", avTrend, lclTrend, uclTrend),
    reportFinalN = sprintf("%.2f (%.2f-%.2f)", avFinalN, lclFinalN, uclFinalN),
    reportDeaths = sprintf("%.0f (%.0f-%.0f)", avDeathReleases, lclDeathReleases, uclDeathReleases),
    reportFp = sprintf("%.2f (%.2f-%.2f)", avFp, lclFp, uclFp),
    reportSp = sprintf("%.2f (%.2f-%.2f)", avSp, lclSp, uclSp)
  )

# Join back to mgmt summary for contextual labels
resu <- resu %>%
  dplyr::left_join(mgmt_summ)

# Ensure proper ordering of factor levels for plotting
resu$Release_Strategy_Name <- factor(resu$Release_Strategy_Name, levels = release_order)


#### evaluating dominance of supplementary feeding

dat<-dat%>%
  left_join(mgmt_summ)



supp_feed_dat<-dat%>%
  dplyr::group_by(alt_sf,p)%>%
  dplyr::arrange(alt_sf,p,desc(SuppFeed))%>%
  dplyr::summarise(diffPersist=(extinct[2]-extinct[1])*-1,
                   diffSp=Sp[2]-Sp[1],
                   diffDeaths=release_deaths[2]-release_deaths[1], 
                   diffTrend=trend[2]-trend[1],
                   diffFinalN=finalN[2]-finalN[1])%>%
  dplyr::group_by(alt_sf)%>%
  dplyr::summarise(diffPersist=(mean(diffPersist)),
                   diffDeaths=mean(diffDeaths),
                   diffSp=(mean(diffSp)),
                   diffTrend=mean(diffTrend),
                   diffFinalN=mean(diffFinalN))%>%
  left_join(mgmt_summ%>%
              select(alt_sf,Release_Strategy_Name,Release_Time_String,Habitat_Scenario)%>%
              filter(!duplicated(alt_sf)))%>%
  dplyr::arrange(Release_Strategy_Name,Release_Time_String,Habitat_Scenario)

supp_feed_summary<-resu%>%
  dplyr::group_by(alt_sf)%>%
  dplyr::arrange(alt_sf,desc(SuppFeed))%>%
  dplyr::summarise(diffPersist=probPersist[2]-probPersist[1],
                   diffSp=avSp[2]-avSp[1],
                   diffTrend=avTrend[2]-avTrend[1],
                   diffFinalN=avFinalN[2]-avFinalN[1])%>%
  left_join(mgmt_summ%>%
              select(alt_sf,Release_Strategy_Name,Release_Time_String,Habitat_Scenario)%>%
              filter(!duplicated(alt_sf)))%>%
  dplyr::arrange(Release_Strategy_Name,Release_Time_String,Habitat_Scenario)

write.csv(supp_feed_dat,file="supp_feed_summary.csv")


releasing_time_summ<-resu%>%
  dplyr::filter(SuppFeed=="Continuous",Release_Strategy_Name!="No releases")%>%
  dplyr::group_by(alt_rt)%>%
  dplyr::arrange(alt_rt,Release_Time_String)%>%
  dplyr::summarise(diffPersist=probPersist[2]-probPersist[1],
                   diffTrend=avTrend[2]-avTrend[1],
                   diffFinalN=avFinalN[2]-avFinalN[1])%>%
  left_join(mgmt_summ%>%
              select(alt_rt,Release_Strategy_Name,SuppFeed,Habitat_Scenario)%>%
              filter(!duplicated(alt_rt)))%>%
  dplyr::arrange(Release_Strategy_Name,SuppFeed,Habitat_Scenario)


write.csv(releasing_time_summ,file="releasing_time_summ.csv")

write.csv(resu,file="full_consequence_table.csv")

ggplot(data=resu%>%filter(kept),mapping = aes(x=avSp,y=probPersist,color=Age_Release_String,pch=Release_Time_String))+
  geom_point(size=3)+
  facet_wrap(~Habitat_Scenario)



y<-dat%>%filter(Release_Time_String=="No releases")%>%
  dplyr::mutate(Release_Time_String=NULL)

y<-y%>%
  merge(mgmt_summ%>%
          filter(!duplicated(data.frame(Release_Time_String)))%>%
          filter(Release_Time_String!="No releases")%>%
          select(Habitat_Scenario,Release_Time_String))
        
dat_plot<-plyr::rbind.fill(
  dat%>%filter(Release_Time_String!="No releases"),
  dat%>%filter(Release_Time_String=="No releases")%>%
    mutate(Release_Time_String="Wait"),
  dat%>%filter(Release_Time_String=="No releases")%>%
    mutate(Release_Time_String="Now"))

resu_plot<-plyr::rbind.fill(
  resu%>%filter(Release_Time_String!="No releases"),
  resu%>%filter(Release_Time_String=="No releases")%>%
    mutate(Release_Time_String="Wait"),
  resu%>%filter(Release_Time_String=="No releases")%>%
    mutate(Release_Time_String="Now"))

age_strat_plot<-ggplot(data=dat_plot%>%filter(kept),aes(x=trend,group=Age_Release_String,color=Age_Release_String))+
  geom_density(aes(fill=Age_Release_String),color=NA,alpha=.5,bw=.01)+
  facet_grid(Habitat_Scenario~Release_Time_String)+
  geom_vline(xintercept = 1,lty="dotted")

ggplot(data = resu_plot%>%filter(kept),
       aes(y=probPersist,x=Release_Strategy_Name,fill=Age_Release_String))+
  geom_bar(stat="identity",color=NA)+
  facet_grid(Habitat_Scenario~Release_Time_String)



for(age in age_order[-1]){
  
  temp<-dat_plot%>%filter(Age_Release_String==age,kept)
  
  temp_summ<-resu%>%filter(Age_Release_String==age,kept)
  
  ggplot(data = temp,aes(x=trend))+
    geom_density(aes(fill=Release_Strategy_Name),color=NA,alpha=.5,bw=.01)+
    facet_grid(Habitat_Scenario~Release_Time_String)+
    geom_vline(xintercept = 1,lty="dotted")
  
  ggplot(data = temp_summ,aes(y=probPersist,x=Release_Strategy_Name,fill=release_time))+
    geom_bar(stat="identity",color=NA)+
    facet_grid(Habitat_Scenario~Release_Time_String)
  
  
}



#### using probability uncertainty to merge nodes on the decision tree
if(6==9){
  
  ### Import expert elicited info
  load("./Data/Expert_Elicitation_Aggregation.RData")
  devtools::source_url("https://raw.githubusercontent.com/KenupCF/Kenup-Repo/master/phd_experimental_functions.R")
  devtools::source_url("https://raw.githubusercontent.com/KenupCF/Kenup-Repo/master/Quick%20Functions.R")
  
  ### Parameter setting
  model_pars<-list(priors=list(),sim=list(),bio=list(),mgmt=list())
  source("./Parameters/SimPars.R")
  source("./Parameters/BioPars.R")
  source("./Parameters/MgmtPars.R")
  prior_rng_seed<-19910526
  
  ### Sample uncertain parameters, given the description of their distribution
  prior_rng<-priorSampling(model_pars$priors,
                           method="lhs",
                           seed = 26051991,
                           # size=1e3)%>%
                           size=model_pars$sim$n_iter)%>%
    dplyr::mutate(p = 1:n())

p_vec<-agg_info$LP$mc_q_functions$`improved_foraging_Probability of improved conditions`(prior_rng$Q_improved_foraging)

risk_df<-data.frame(succ=p_vec,fail=1-p_vec)%>%
  dplyr::mutate(p=1:n())

risk_long <- risk_df |>
  pivot_longer(cols = c(succ, fail),
               names_to = "Habitat_Scenario",
               values_to = "prob")%>%
  dplyr::mutate(Habitat_Scenario=case_when(Habitat_Scenario=="succ"~"Habitat improved",
                                           Habitat_Scenario=="fail"~"No habitat improvement"))

dat2<-dat%>%
  dplyr::left_join(risk_long)


risk_outcomes<-dat2%>%
  dplyr::filter(kept)%>%
  dplyr::group_by(p,alt_hs)%>%
  dplyr::summarise(Mtrend=sum(trend*prob),
                   Btrend=max(trend),
                   Wtrend=min(trend),
                   Mpersist=sum((1-extinct)*prob),
                   Bpersist=max(1-extinct),
                   Wpersist=min(1-extinct),
                   Mdeaths=sum(release_deaths*prob),
                   Bdeaths=min(release_deaths),
                   Wdeaths=max(release_deaths))%>%
  dplyr::left_join(mgmt_summ%>%filter(!duplicated(alt_hs)))


risk_outcomes_summ<-risk_outcomes%>%
  dplyr::group_by(alt_hs)%>%
  dplyr::summarise(
                   LCLRNtrend=quantile(Mtrend,.025),
                   AvRNtrend=mean(Mtrend),
                   UCLRNtrend=quantile(Mtrend,.975),
                   # LCLRNpersist=quantile(Mpersist,.025),
                   AvRNpersist=mean(Mpersist),
                   # UCLRNpersist=quantile(Mpersist,.975),
                   LCLRStrend=quantile(Btrend,.025),
                   AvRStrend=mean(Btrend),
                   UCLRStrend=quantile(Btrend,.975),
                   # LCLRSpersist=quantile(Bpersist,.025),
                   AvRSpersist=mean(Bpersist),
                   # UCLRSpersist=quantile(Bpersist,.975),
                   LCLRAtrend=quantile(Wtrend,.025),
                   AvRAtrend=mean(Wtrend),
                   UCLRAtrend=quantile(Wtrend,.975),
                   # LCLRApersist=quantile(Wpersist,.025),
                   AvRApersist=mean(Wpersist),
                   # UCLRApersist=quantile(Wpersist,.975),
                   AvRNdeaths=mean(Mdeaths),
                   AvRAdeaths=mean(Wdeaths),
                   AvRSdeaths=mean(Bdeaths))%>%
  dplyr::select(AvRNpersist,AvRApersist,AvRSpersist,AvRNdeaths,AvRAdeaths,AvRSdeaths,alt_hs)%>%
  dplyr::left_join(mgmt_summ%>%filter(!duplicated(alt_hs)))
  
}

