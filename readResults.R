library(dplyr)
library(progress)
library(duckdb)
library(stringr)
library(lubridate)

options(warn = 2)

# SETUP
folder_extr <- "D:/03-Work/01-Science/00-Research Projects/RB Chough Results/bigRunsV3_LHS"
folderID    <- gsub(x=folder_extr,"^.*/","")
loopSize    <- 1e3
time_limit_secs <- 60*60*(40/60)  #3 hours

# Define processing function
process_result_file <- function(filename,folder_id){
  suppressMessages({
    load(filename)
    
    idx <- unique(output$pop$i)
    label <- output$run_label
    
    output$pop0<-output$pop
    
    zeroNs<-which(output$pop0%>%
      dplyr::group_by(t)%>%
      dplyr::summarise(N=sum(alive))%>%
      dplyr::pull(N)==0)    
    
    suppressWarnings({
      zeroN<-min(zeroNs)
    })
    
    output$pop<-output$pop%>%
      dplyr::filter(t<zeroN)
    output$egg_fate<-output$egg_fate%>%
      dplyr::filter(t<zeroN)
    output$envir_stoch<-output$envir_stoch%>%
      dplyr::filter(t<zeroN)
    
    released_individuals<-output$pop%>%
      filter(!is.na(age_release))%>%
      pull(id)%>%
      unique
    
    if(length(released_individuals)>0){
    release_deaths<-output$pop%>%
      dplyr::filter(id%in%released_individuals,!alive)%>%
      dplyr::mutate(dead_before_first_june=case_when(age_release==0~age==1,
                                              age_release>0~tsr<=1))%>%
      dplyr::left_join(output$model_pars$mgmt$release_year_cont)%>%
      dplyr::group_by(age_release)%>%
      dplyr::summarise(
        mortality=mean(dead_before_first_june),
        expectedDeaths=sum(dead_before_first_june)^unique(yr_duration))%>%
      pull(expectedDeaths)%>%
      sum()
    }else{release_deaths<-0}
      
    
    gen_final_outcome<-output$pop%>%
      dplyr::filter(t%in%(max(t):(max(t)-5)),alive)%>%
      dplyr::group_by(t)%>%
      dplyr::summarise(N=sum(alive),Sp=mean(scot_heritage),Fp=mean(Fi),.groups="drop")%>%
      dplyr::mutate(Nw=N/sum(N))%>%
      dplyr::summarise(Sp=sum(Sp*Nw),Fp=sum(Fp*Nw))
    
    N_df <- output$pop %>%
      dplyr::group_by(t, i) %>%
      dplyr::summarise(N = sum(alive),
                Fp = sum(Fi*alive)/sum(alive),
                Sp = sum(scot_heritage*alive)/sum(alive))
    
    Ns <- pull(N_df, N)
    Ns_2 <- Ns
    if (last(Ns_2) == 0) Ns_2 <- Ns_2[-length(Ns_2)]
    
    lambda <- (Ns_2[-1]) / (Ns_2[-length(Ns_2)])
    
    trend <- prod(lambda)^(1 / length(lambda))
    
    finalN <- tail(Ns, 1)
    extinct <- finalN <= 2
    time_extinct <- ifelse(extinct, length(Ns), NA)
    
    summ <- data.frame(
      extinct = extinct,
      finalN = finalN,
      trend = trend,
      time_extinct = time_extinct,
      release_deaths=release_deaths,
      i = idx,
      label = label
    )%>%
      merge(gen_final_outcome)
    
    run_pars <- output$run_pars
    egg_fate <- output$egg_fate
    release_sch <- output$model_pars$mgmt$release_schedule
    
    mgmt <- release_sch %>%
      cbind(SuppFeed = as.character(run_pars$SuppFeed)) %>%
      mutate(i = idx)
  })
  
  resu<-list(
    summary = summ,
    N_series = N_df,
    egg_fate = egg_fate,
    mgmt = mgmt,
    run_pars = as.data.frame(run_pars)
  )
  
  resu<-lapply(resu,function(x){
    x$folder<-folder_id
    x$filename<-filename
    x$i<-idx
    return(x)})

  return(resu)    
  
}


r_files <- list.files(path = folder_extr, pattern = ".RData", full.names = TRUE)
idx <- r_files %>%
  gsub(".RData", "", .) %>%
  gsub(" \\(.*", "", .) %>%
  substr(nchar(.) - 5, nchar(.)) %>%
  gsub("_","",.)%>%
  as.numeric()

files_df <- data.frame(file = r_files, i = idx) %>%
  mutate(nchar = nchar(file)) %>%
  arrange(desc(nchar))

# Remove duplicates
duplicated_files <- files_df %>%
  filter(duplicated(i, fromLast = TRUE)) %>%
  pull(file)
sapply(duplicated_files, file.remove)

files_df <- files_df %>% filter(!duplicated(i))

# CONNECT TO DUCKDB
db_path <- "D:/03-Work/01-Science/00-Research Projects/RB Chough Results/results_bigv3.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)

# Check which files have already been processed
if (!"summary" %in% dbListTables(con)) {
  already_imported <- numeric(0)
} else {
  already_imported <- dbGetQuery(con, "SELECT * FROM summary")%>%
    dplyr::filter(folder==folderID)%>%
    pull(i)
}

# Filter to new files only
files_df <- files_df %>% filter(!i %in% already_imported)

cat(paste0(nrow(files_df), " files still left to read."))

files_df<-files_df[1:min(loopSize,nrow(files_df)),]

# PROGRESS BAR
pb <- progress_bar$new(
  total = nrow(files_df),
  format = "  Processing [:bar] :percent ETA: :eta",
  clear = FALSE,
  width = 60
)
buffer_size<-20
counter<-0

# Prepare buffers
buffer <- list(
  summary = vector("list", length = buffer_size),    # Preallocate 10 slots for summary
  N_series = vector("list", length = buffer_size),    # Preallocate 10 slots for N_series
  egg_fate = vector("list", length = buffer_size),    # Preallocate 10 slots for egg_fate
  mgmt = vector("list", length = buffer_size),        # Preallocate 10 slots for mgmt
  run_pars = vector("list", length = buffer_size)     # Preallocate 10 slots for run_pars
)
start_time <- Sys.time()
# LOOP OVER NEW FILES
for (f in seq_len(nrow(files_df))) {
  
  
  result <- tryCatch({
    process_result_file(files_df$file[f], folder_id = folderID)
  }, error = function(e) {
    message(sprintf("Error processing file %s: %s", files_df$file[f], e$message))
    return(NULL)
  })
  
  if (is.null(result)) next
  
  buffer$summary[[f]]   <- result$summary
  buffer$N_series[[f]]  <- result$N_series
  buffer$egg_fate[[f]]  <- result$egg_fate
  buffer$mgmt[[f]]      <- result$mgmt
  buffer$run_pars[[f]]  <- result$run_pars
  
  
  if(6==9){
  # if (f %% buffer_size == 0 || f == nrow(files_df)) {
    dbWriteTable(con, "summary",   bind_rows(buffer$summary), append = TRUE)
    dbWriteTable(con, "N_series",  bind_rows(buffer$N_series), append = TRUE)
    dbWriteTable(con, "egg_fate",  bind_rows(buffer$egg_fate), append = TRUE)
    dbWriteTable(con, "mgmt",      bind_rows(buffer$mgmt), append = TRUE)
    dbWriteTable(con, "run_pars",  bind_rows(buffer$run_pars), append = TRUE)
    
    buffer <- list(summary = list(), N_series = list(), egg_fate = list(), mgmt = list(), run_pars = list())
  }
  
  counter<-counter+1
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (elapsed_time > time_limit_secs) {
    message("Time limit reached. Stopping loop.")
    break
  }
  
  
  pb$tick()
}

time_diff<-difftime(now(),start_time)
cat(paste0("Imported ",counter," entries in ",round(time_diff,2)," ",attr(time_diff, "units")))


# CLEANUP
dbDisconnect(con, shutdown = TRUE)

