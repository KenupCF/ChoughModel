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
  dplyr::mutate(Age_Release_String=case_when(noEggsReleased>0 & nest_aband_allowed   ~"Eggs",
                                             noEggsReleased>0 & !nest_aband_allowed  ~"Eggs+",
                                             age_release==1~"First year",
                                             age_release==2~"Sub-adults",
                                             age_release==3~"Adults"),
                Individuals_Released=case_when(noEggsReleased>0~noEggsReleased,
                                              TRUE~release_size),
                Origin_String=substr(origin,1,1),
                Method_String=substr(release_meth,1,2),
                Habituation_String=case_when(habituation==0~"NH",
                                             habituation==1~"H"))%>%
  dplyr::mutate(Release_Strategy_Name=case_when(release_size==0 & noEggsReleased==0~"No releases",
                                        str_detect(Age_Release_String,"Eggs")~Age_Release_String,
                                        age_release==1~
                                          paste0(Age_Release_String," - ",release_time," (",Origin_String,"-",Method_String,"-",Habituation_String,")"),
                                        age_release%in%(2:3)~
                                          paste0(Age_Release_String," (",Origin_String,"-",Method_String,"-",Habituation_String,")"),
                                        TRUE~NA),
                Release_Time_String=case_when(release_size==0 & noEggsReleased==0~"",
                                                        wait_for_habitat~"Wait for habitat improvement",
                                                        !wait_for_habitat~"Right away"))

mgmt_trim<-mgmt%>%
  dplyr::select(Strategy_Name,Release_Time_String,SuppFeed,Individuals_Released,i,folder)%>%
  dplyr::left_join(run_pars%>%
                     dplyr::select(i,folder,Habitat_Scenario))




release_order <- c(
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



resu<-summary%>%
  dplyr::left_join(mgmt_trim)%>%
  dplyr::group_by(Habitat_Scenario,Release_Time_String,SuppFeed,Strategy_Name,Individuals_Released)%>%
  dplyr::summarise(probPersist=1-mean(extinct),
                   propPopulationsDeclining=mean(trend<1),
                   avTrend=mean(trend),
                   finalN=mean(finalN*(!extinct)),
                   finalFp=mean(Fp),
                   finalSp=mean(Sp))

resu$Strategy_Name<-factor(resu$Strategy_Name,levels=release_order)

resu<-resu%>%dplyr::filter(scenario=="Habitat Improved")

require(ggplot2)

ggplot(data=resu)+
  geom_line(aes(x=Individuals_Released,y=probPersist))+
  facet_grid(scenario~Strategy_Name,scales="free_y")


ggplot(data=resu%>%
         dplyr::filter(str_detect(Strategy_Name,"Eggs")))+
  geom_line(aes(x=Individuals_Released,y=probPersist))+
  facet_grid(scenario~Strategy_Name,scales="free_y")


ggplot(data=resu%>%
         dplyr::filter(str_detect(Strategy_Name,"First")))+
  geom_line(aes(x=Individuals_Released,y=probPersist))+
  facet_grid(scenario~Strategy_Name,scales="free_y")

ggplot(data=resu%>%
         dplyr::filter(str_detect(Strategy_Name,"Sub")))+
  geom_line(aes(x=Individuals_Released,y=probPersist))+
  facet_grid(scenario~Strategy_Name,scales="free_y")

ggplot(data=resu%>%
         dplyr::filter(str_detect(Strategy_Name,"Adu")))+
  geom_line(aes(x=Individuals_Released,y=probPersist))+
  facet_grid(scenario~Strategy_Name,scales="free_y")


