wd<-"~/RedSquirrelRecoveryEngland/Biological_Model"

if(!dir.exists(wd)){
  wd<-"C:/Users/caiok/Dropbox/03-Work/01-Science/00-Research Projects/ChoughModel"
  
}
if(!dir.exists(wd)){
  wd<-"C:/Users/Caio.Kenup/ChoughModel"
  
}
setwd(wd)
source("packageLoader.R")
source("functionLoader.R")
devtools::source_url("https://raw.githubusercontent.com/KenupCF/Kenup-Repo/master/phd_experimental_functions.R")
devtools::source_url("https://raw.githubusercontent.com/KenupCF/Kenup-Repo/master/Quick%20Functions.R")

sheet_url <- "https://docs.google.com/spreadsheets/d/1PBwtQlmPivdoU9BqLpySnyaCU6FsRvaSMJvlPHhHyts/edit?gid=903185379#gid=903185379"
gs4_auth(path = "./.tokens/fresh-replica-344321-0e0618a3b5de.json")
require(stringr)

# setwd("./Results/BackedUp")
folder_extr<-"D:/03-Work/01-Science/00-Research Projects/RB Chough Results/bigRunsV3_LHS"

files<-list.files(path = folder_extr,pattern = ".RData",full.names = T)

idx<-gsub(x=files,".RData","")
idx<-gsub(x=idx," \\(.*","")

idx<-gsub(x=substr(idx,nchar(idx)-5,nchar(idx)),"_","")%>%as.numeric()

run_get<-data.frame(i=idx,Run=TRUE,Scheduled=FALSE)%>%
  dplyr::mutate(`Run Numeric`=as.numeric(Run),
                Allocated=as.numeric(Run | Scheduled),Iteration=i)%>%
  dplyr::filter(!duplicated(i))

sum(run_get$Run)

runs <- read_sheet(sheet_url,sheet="Runs")%>%
  replace_na_characters()%>%
  dplyr::filter(!duplicated(ID))

temp<-plyr::rbind.fill(run_get%>%
                         dplyr::select(Iteration,Run,Scheduled),
                       runs%>%
                         dplyr::mutate(Iteration=as.numeric(Iteration))%>%
                         dplyr::filter(!Iteration%in%run_get$Iteration)%>%
                         dplyr::select(Iteration,Run,Scheduled))%>%
  dplyr::arrange(Iteration)

run_col<-LETTERS[colnames(runs)=="Run"]
sch_col<-LETTERS[colnames(runs)=="Scheduled"]

run_range<-paste(paste(run_col,c(1,nrow(temp))+1,sep=""),collapse=":")
sch_range<-paste(paste(sch_col,c(1,nrow(temp))+1,sep=""),collapse=":")


range_write(data = temp%>%
              # mutate(Run="A")%>%
              dplyr::select(Run), range=run_range,ss=sheet_url, sheet = "Runs",col_names = F)

range_write(data = temp%>%
              # mutate(Run="A")%>%
              dplyr::select(Scheduled), range=sch_range,ss=sheet_url, sheet = "Runs",col_names = F)
# 
# # Join updated `Run` values from `temp`
# runs2 <- runs %>%
#   dplyr::left_join(temp %>% dplyr::select(Iteration, Run), by = "Iteration", suffix = c("", ".new")) %>%
#   dplyr::mutate(Run = coalesce(Run.new, Run)) %>%
#   dplyr::select(-Run.new)
# 
# # Write updated table back to sheet
# write_sheet(runs2, sheet_url, sheet = "Runs")
# # 
# # runs$Label<-as.character(runs$Label)
# # 
# # runs2<-left_join(runs%>%
# #                    dplyr::mutate(Scheduled=NULL,Run=NULL),
# #                  run_get%>%
# #                    dplyr::mutate(Iteration=i)%>%
# #                    dplyr::select(Iteration,Run,Scheduled,`Run Numeric`,Allocated)
# #                  # ,by=c("Label","Iteration"),all.x=TRUE,all.y=TRUE
# # )%>%
# #   dplyr::arrange(Label,Iteration)%>%
# #   dplyr::mutate(ID=paste(Label,zero_pad(Iteration,5),sep="_"))%>%
# #   dplyr::mutate(Scheduled=!Run)%>%
# #   dplyr::mutate(Scheduled=case_when(
# #     is.na(Scheduled)~TRUE,
# #     TRUE~Scheduled
# #   ),
# #   Run=case_when(
# #     is.na(Run)~FALSE,
# #     TRUE~Run
# #   ))
# # 
# # str(runs2)
# 
