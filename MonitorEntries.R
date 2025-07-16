
wd<-"~/ChoughModel"
if(!dir.exists(wd)){
  wd<-"C:/Users/caiok/Dropbox/03-Work/01-Science/00-Research Projects/ChoughModel"
  
}
if(!dir.exists(wd)){
  wd<-"C:/Users/Caio.Kenup/ChoughModel"
  
}
setwd(wd)


require(googledrive)
source("./Functions/FUN.R")

cat("\nHello world\n")

### Acessing Google Drive
token <- readRDS(".tokens/token.rds")
drive_auth(token = token)

# Create backup directory if it doesn't exist
backup_dir <- "./Results/BackedUp"
if (!dir.exists(backup_dir)) {
  dir.create(backup_dir, recursive = TRUE)
}

cat("\nChecking for files\n")

tryCatch({
  output_clean_up()
}, error = function(e) {
  cat("\nError in output_clean_up: ", conditionMessage(e), "\n")
})

wait_time_mins<-5

i<-1
while(1==1){
  
  cat(paste0("\nWaiting ",i,"\n"))
  Sys.sleep(time = wait_time_mins*60)
  cat(paste0("\nChecking for files, check number ",i,"\n"))
  tryCatch({
    output_clean_up()
  }, error = function(e) {
    cat("\nError in output_clean_up: ", conditionMessage(e), "\n")
  })
  
  i<-i+1
  gc()
  
}
