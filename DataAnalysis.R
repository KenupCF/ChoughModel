wd<-"/models/ChoughModel"

if(!dir.exists(wd)){
  wd<-"C:/Users/caiok/Dropbox/03-Work/01-Science/00-Research Projects/ChoughModel"
  
}
if(!dir.exists(wd)){
  wd<-"C:/Users/Caio.Kenup/ChoughModel"
  
}
setwd(wd)

source("packageLoader.R")
require(duckdb)

# CONNECT TO DUCKDB
db_path <- "D:/03-Work/01-Science/00-Research Projects/RB Chough Results/results_bigv2.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)


summary<-dbGetQuery(con, "SELECT * FROM summary")

run_pars<-dbGetQuery(con, "SELECT * FROM run_pars")

run_pars<-run_pars%>%
  dplyr::mutate(Habitat_Scenario=case_when(prob_imp_for==0~"No habitat improvement",
                                           prob_imp_for==1~"Habitat improved"))

mgmt<-dbGetQuery(con, "SELECT * FROM mgmt")

mgmt<-mgmt%>%
  dplyr::mutate(Individuals_Released=case_when(noEggsReleased>0~noEggsReleased,
                                                               TRUE~release_size),
                Age_Release_String=case_when(Individuals_Released==0 ~ "No releases",
                                             noEggsReleased>0 & nest_aband_allowed   ~"Eggs",
                                             noEggsReleased>0 & !nest_aband_allowed  ~"Eggs+",
                                             age_release==1~"First year",
                                             age_release==2~"Sub-adults",
                                             age_release==3~"Adults"),

                Origin_String=substr(origin,1,1),
                Method_String=substr(release_meth,1,2),
                Habituation_String=case_when(habituation==0~"NH",
                                             habituation==1~"H"))%>%
  dplyr::mutate(Release_Strategy_Name=case_when(Individuals_Released==0~"No releases",
                                        str_detect(Age_Release_String,"Eggs")~Age_Release_String,
                                        age_release==1~
                                          paste0(Age_Release_String," - ",release_time," (",Origin_String,"-",Method_String,"-",Habituation_String,")"),
                                        age_release%in%(2:3)~
                                          paste0(Age_Release_String," (",Origin_String,"-",Method_String,"-",Habituation_String,")"),
                                        TRUE~NA),
                Release_Time_String=case_when(release_size==0 & noEggsReleased==0~"",
                                                        wait_for_habitat~"Wait for habitat improvement",
                                                        !wait_for_habitat~"Right away"))%>%
  dplyr::mutate(SuppFeed=case_when(SuppFeed=="Current"~"Continuous",
                                   SuppFeed=="Provisional"~"Temporary"))

release_order <- c(
  "No releases",
  "Eggs",
  "Eggs+",
  "First year - Summer (C-St-H)",
  "First year - Summer (W-St-H)",
  "First year - Winter (C-St-H)",
  "First year - Winter (W-St-H)",
  "Sub-adults (C-St-H)",
  "Sub-adults (W-St-H)",
  "Sub-adults (C-St-NH)",
  "Sub-adults (W-St-NH)",
  "Adults (C-St-H)",
  "Adults (C-St-NH)",
  "Adults (W-Im-NH)"
)

mgmt_trim<-mgmt%>%
  dplyr::select(Release_Strategy_Name,Release_Time_String,SuppFeed,Individuals_Released,i,folder,Age_Release_String)%>%
  dplyr::left_join(run_pars%>%
                     dplyr::select(i,folder,Habitat_Scenario,alt))

mgmt_summ<-mgmt_trim%>%
  dplyr::filter(!duplicated(alt))%>%
  dplyr::select(alt,Release_Strategy_Name,Release_Time_String,SuppFeed,Age_Release_String,Habitat_Scenario)

dat<-summary%>%
  dplyr::left_join(mgmt_trim)%>%
  dplyr::left_join(run_pars%>%dplyr::select(i,p,alt))

resu<-dat%>%
  dplyr::group_by(Habitat_Scenario,Release_Time_String,SuppFeed,Release_Strategy_Name,Individuals_Released)%>%
  dplyr::summarise(probPersist=1-mean(extinct),
                   probExtinct=1-probPersist,
                   propPopulationsDeclining=mean(trend<1,na.rm=T),
                   avTrend=mean(trend,na.rm=T),
                   lclTrend=quantile(trend,.025,na.rm=T),
                   uclTrend=quantile(trend,.975,na.rm=T),
                   avFinalN=mean(finalN*(!extinct)),
                   lclFinalN=quantile(finalN*(!extinct),.025,na.rm=T),
                   uclFinalN=quantile(finalN*(!extinct),.975,na.rm=T),
                   avDeathReleases=mean(release_deaths,na.rm=T),
                   lclDeathReleases=quantile(release_deaths,.025,na.rm=T),
                   uclDeathReleases=quantile(release_deaths,.975,na.rm=T),
                   avFp=mean(Fp),
                   lclFp=quantile(Fp,.025,na.rm=T),
                   uclFp=quantile(Fp,.975,na.rm=T),
                   avSp=mean(Sp),
                   lclSp=quantile(Sp,.025,na.rm=T),
                   uclSp=quantile(Sp,.975,na.rm=T))%>%
  dplyr::mutate(
    reportTrend = sprintf("%.2f (%.2f-%.2f)", avTrend, lclTrend, uclTrend),
    reportFinalN = sprintf("%.2f (%.2f-%.2f)", avFinalN, lclFinalN, uclFinalN),
    reportDeaths = sprintf("%.0f (%.0f-%.0f)", avDeathReleases, lclDeathReleases,uclDeathReleases),
    reportFp = sprintf("%.2f (%.2f-%.2f)", avFp, lclFp,uclFp),
    reportSp = sprintf("%.2f (%.2f-%.2f)", avSp, lclSp,uclSp)
  )

resu<-resu%>%
  dplyr::left_join(mgmt_summ)

colnames(resu)
resu$Release_Strategy_Name<-factor(resu$Release_Strategy_Name,levels=release_order)

write.csv(resu,file="draft_consequence_table.csv")


ggplot(data=resu,mapping = aes(x=avSp,y=probPersist,color=Age_Release_String,pch=Release_Time_String))+
  geom_point(size=3)+
  facet_wrap(~Habitat_Scenario)







#### using probability uncertainty to merge nodes on the decision tree
if(6==9){
load("./Data/Expert_Elicitation_Aggregation.RData")

sampled_quantiles_prob_imp_foraging<-runif(n = 1e3,min = 0,max = 1)

p_vec<-agg_info$LP$mc_q_functions$`improved_foraging_Probability of improved conditions`(sampled_quantiles_prob_imp_foraging)

risk_df<-data.frame(succ=p_vec,fail=1-p_vec)%>%
  dplyr::mutate(q=1:n())
risk_long <- risk_df |>
  pivot_longer(cols = c(succ, fail),
               names_to = "Habitat_Scenario",
               values_to = "prob_habitat_improved")%>%
  dplyr::mutate(Habitat_Scenario=case_when(Habitat_Scenario=="succ"~"Habitat improvement",
                                           Habitat_Scenario=="fail"~"No habitat improvement"))



full_uncertainty_result<-data.frame()



for()

for(i in seq_len(nrow(dat))){
  
  alt<-dat$alt[i]
  p<-dat$p[i]
  i<-dat$i[i]
  
  trend<-(risk_df$succ*dat$trend[i])+
  
  
}


str(temp)
str(risk_long)
merged_df <- risk_long %>%
  inner_join(temp, by = "Habitat_Scenario",relationship = "many-to-many")

}

