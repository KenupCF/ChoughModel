
source("packageLoader.R")
source("functionLoader.R")
devtools::source_url("https://raw.githubusercontent.com/KenupCF/Kenup-Repo/master/phd_experimental_functions.R")
devtools::source_url("https://raw.githubusercontent.com/KenupCF/Kenup-Repo/master/Quick%20Functions.R")

sheet_url <- "https://docs.google.com/spreadsheets/d/1dCIkkofz0h2s9MWOtZfNqAN4DKY59Z2IqNIWlM3isMY/edit?gid=903185379#gid=903185379"
gs4_auth(path = "./.tokens/fresh-replica-344321-0e0618a3b5de.json")
require(stringr)

setwd("./Results/BackedUp")
setwd("G:/My Drive/03-Work/00-ZSL/RB Chough Results/testReleaseSizesV1")

files<-list.files(pattern = ".RData")

idx<-gsub(x=files,".RData","")
idx<-gsub(x=idx," \\(.*","")

idx<-substr(idx,nchar(idx)-4,nchar(idx))%>%as.numeric()

run_get<-data.frame(i=idx,Run=TRUE,Scheduled=FALSE)

runs<- read_sheet(sheet_url,sheet="Runs")%>%
  replace_na_characters()
runs$Label<-as.character(runs$Label)

runs2<-full_join(runs%>%
                   dplyr::mutate(Scheduled=NULL,Run=NULL),
                 run_get%>%
                   dplyr::mutate(Iteration=i)%>%
                   dplyr::select(Iteration,Run,Scheduled)
                 # ,by=c("Label","Iteration"),all.x=TRUE,all.y=TRUE
)%>%
  dplyr::arrange(Label,Iteration)%>%
  dplyr::mutate(ID=paste(Label,zero_pad(Iteration,5),sep="_"))%>%
  dplyr::mutate(Scheduled=!Run)%>%
  dplyr::mutate(Scheduled=case_when(
    is.na(Scheduled)~TRUE,
    TRUE~Scheduled
  ),
  Run=case_when(
    is.na(Run)~FALSE,
    TRUE~Run
  ))

write_sheet(runs2, sheet_url, sheet = "Runs")
