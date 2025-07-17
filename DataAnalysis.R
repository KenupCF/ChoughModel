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
db_path <- "D:/03-Work/01-Science/00-Research Projects/RB Chough Results/results_sizes_altered.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)


summary<-dbGetQuery(con, "SELECT * FROM summary")

run_pars<-dbGetQuery(con, "SELECT * FROM run_pars")

mgmt<-dbGetQuery(con, "SELECT * FROM mgmt")

mgmt<-mgmt%>%
  dplyr::mutate(Age_Release_String=case_when(noEggsReleased>0~"Eggs",
                                             age_release==1~"First year",
                                             age_release==2~"Sub-adults",
                                             age_release==3~"Adults"),
                Individuals_Released=case_when(noEggsReleased>0~noEggsReleased,
                                              TRUE~release_size),
                Origin_String=substr(origin,1,1),
                Method_String=substr(release_meth,1,2),
                Habituation_String=case_when(habituation==0~"NH",
                                             habituation==1~"H"))%>%
  dplyr::mutate(Strategy_Name=case_when(Age_Release_String=="Eggs"~Age_Release_String,
                                        age_release==1~
                                          paste0(Age_Release_String," - ",release_time," (",Origin_String,"-",Method_String,"-",Habituation_String,")"),
                                        age_release%in%(2:3)~
                                          paste0(Age_Release_String," (",Origin_String,"-",Method_String,"-",Habituation_String,")"),
                                        TRUE~NA))

mgmt_trim<-mgmt%>%
  dplyr::select(Strategy_Name,Individuals_Released,i,folder)




release_order <- c(
  "Eggs",
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
  dplyr::group_by(folder,Strategy_Name,Individuals_Released)%>%
  dplyr::summarise(probPersist=1-mean(extinct))%>%
  dplyr::mutate(scenario=case_when(folder=="testReleaseSizesV1"~"No Habitat Improvement",
                                   folder=="testReleaseSizesV2"~"Habitat Improved"))


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


