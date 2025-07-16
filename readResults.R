require(dplyr)
library(progress)
process_result_file<-function(filename){
  
  suppressMessages({
  load(filename)
  
  idx = unique(output$pop$i)
  label = output$run_label
  
  N_df<-output$pop%>%
    dplyr::group_by(t,i)%>%
    dplyr::summarise(N=sum(alive))
  
  # output$egg_fate
  Ns<-N_df%>%
    dplyr::pull(N)
  
  Ns_2<-Ns
  if(last(Ns_2)==0){Ns_2<-Ns_2[-length(Ns_2)]}
  lambda<-Ns_2[-1]/Ns_2[-length(Ns_2)]
  trend<-prod(lambda)^(1/length(lambda))
  
  
  
  finalN<-N_df%>%
    dplyr::pull(N)%>%
    tail(1)
  
  extinct<-finalN<=2  
  
  time_extinct<-ifelse(extinct,length(Ns),Inf)
  
  summ<-data.frame(extinct=extinct,finalN=finalN,trend=trend,time_extinct,i=idx,label=label)
  
  run_pars<-output$run_pars
  
  egg_fate<-output$egg_fate
  
  release_sch<-output$model_pars$mgmt$release_schedule
  mgmt<-release_sch%>%cbind(SuppFeed=as.character(run_pars$SuppFeed))%>%
  dplyr::mutate(i=idx)
  })
  resu<-list(summary=summ,N_series=N_df,egg_fate=egg_fate,mgmt=mgmt,run_pars=run_pars)
  
  return(resu)
}



folder_extr<-("G:/My Drive/03-Work/00-ZSL/RB Chough Results/testReleaseSizesV1")
folder_extr<-"D:/03-Work/01-Science/00-Research Projects/RB Chough Results/testReleaseSizesV1"
r_files<-list.files(path = folder_extr,pattern = ".RData",full.names = T)
idx<-gsub(x=r_files,".RData","")
idx<-gsub(x=idx," \\(.*","")
idx<-substr(idx,nchar(idx)-4,nchar(idx))%>%as.numeric()

files_df<-data.frame(file=r_files,i=idx)%>%
  dplyr::mutate(nchar=nchar(file))

duplicated_files<-files_df%>%
  dplyr::arrange(desc(nchar))%>%
  dplyr::filter(duplicated(i,fromLast = T))%>%
  dplyr::pull(file)

sapply(duplicated_files,file.remove)

files_df<-files_df%>%
  dplyr::filter(!duplicated(i))

pb <- progress_bar$new(
  total = nrow(files_df),
  format = "  Processing [:bar] :percent ETA: :eta",
  clear = FALSE,
  width = 60
)

resu_list <- list()
for (f in files_df$file) {
  resu_list <- append(resu_list, list(process_result_file(filename = f)))
  pb$tick()
}


  