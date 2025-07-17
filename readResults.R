library(dplyr)
library(progress)
library(duckdb)

# SETUP
folder_extr <- "D:/03-Work/01-Science/00-Research Projects/RB Chough Results/testReleaseSizesV2"
folderID    <- gsub(x=folder_extr,"^.*/","")
loopSize    <- 1e6

# Define processing function
process_result_file <- function(filename,folder_id){
  suppressMessages({
    load(filename)
    
    idx <- unique(output$pop$i)
    label <- output$run_label
    
    N_df <- output$pop %>%
      dplyr::group_by(t, i) %>%
      dplyr::summarise(N = sum(alive),
                Fp = sum(Fi*alive)/sum(alive),
                Sp = sum(scot_heritage*alive)/sum(alive))
    
    Ns <- pull(N_df, N)
    Ns_2 <- Ns
    if (last(Ns_2) == 0) Ns_2 <- Ns_2[-length(Ns_2)]
    lambda <- Ns_2[-1] / Ns_2[-length(Ns_2)]
    trend <- prod(lambda)^(1 / length(lambda))
    
    finalN <- tail(Ns, 1)
    extinct <- finalN <= 2
    time_extinct <- ifelse(extinct, length(Ns), NA)
    
    summ <- data.frame(
      extinct = extinct,
      finalN = finalN,
      trend = trend,
      time_extinct = time_extinct,
      i = idx,
      label = label
    )
    
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
    run_pars = as.data.frame(run_pars) %>% mutate(i = idx)
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
  substr(nchar(.) - 4, nchar(.)) %>%
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
db_path <- "D:/03-Work/01-Science/00-Research Projects/RB Chough Results/results_sizes_altered.duckdb"
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

files_df<-files_df[1:min(loopSize,nrow(files_df)),]

# PROGRESS BAR
pb <- progress_bar$new(
  total = nrow(files_df),
  format = "  Processing [:bar] :percent ETA: :eta",
  clear = FALSE,
  width = 60
)

# Prepare buffers
buffer <- list(summary = list(), N_series = list(), egg_fate = list(), mgmt = list(), run_pars = list())

# LOOP OVER NEW FILES
for (f in seq_len(nrow(files_df))) {
  result <- process_result_file(files_df$file[f],folder_id = folderID)
  
  buffer$summary[[f]]   <- result$summary
  buffer$N_series[[f]]  <- result$N_series
  buffer$egg_fate[[f]]  <- result$egg_fate
  buffer$mgmt[[f]]      <- result$mgmt
  buffer$run_pars[[f]]  <- result$run_pars
  
  if (f %% 50 == 0 || f == nrow(files_df)) {
    dbWriteTable(con, "summary",   bind_rows(buffer$summary), append = TRUE)
    dbWriteTable(con, "N_series",  bind_rows(buffer$N_series), append = TRUE)
    dbWriteTable(con, "egg_fate",  bind_rows(buffer$egg_fate), append = TRUE)
    # dbWriteTable(con, "mgmt",      bind_rows(buffer$mgmt), append = TRUE)
    dbWriteTable(con, "run_pars",  bind_rows(buffer$run_pars), append = TRUE)
    
    buffer <- list(summary = list(), N_series = list(), egg_fate = list(), mgmt = list(), run_pars = list())
  }
  
  pb$tick()
}

# CLEANUP
dbDisconnect(con, shutdown = TRUE)

