# Usage:
# Rscript move_unique_files.R /path/to/source /path/to/destination

# args <- commandArgs(trailingOnly = TRUE)

# if (length(args) != 2) {
  # stop("Please provide exactly two arguments: source_folder and destination_folder")
# }

source_folder <- "C:/Users/caiok/Dropbox/03-Work/01-Science/00-Research Projects/ChoughModel/Results/BackedUp"
destination_folder <- ("G:/My Drive/03-Work/00-ZSL/RB Chough Results/testReleaseSizesV1")

# Check if folders exist
if (!dir.exists(source_folder)) {
  stop("Source folder does not exist: ", source_folder)
}
if (!dir.exists(destination_folder)) {
  stop("Destination folder does not exist: ", destination_folder)
}


# Check if folders exist
if (!dir.exists(source_folder)) {
  stop("Source folder does not exist: ", source_folder)
}
if (!dir.exists(destination_folder)) {
  stop("Destination folder does not exist: ", destination_folder)
}

# List file names (only files, not subdirectories)
source_files <- list.files(source_folder, full.names = FALSE)
dest_files <- list.files(destination_folder, full.names = FALSE)

# Identify files not already in destination
files_to_move <- setdiff(source_files, dest_files)

if (length(files_to_move) == 0) {
  message("No new files to move.")
} else {
  # Initialize progress bar
  pb <- txtProgressBar(min = 0, max = length(files_to_move), style = 3)
  
  for (i in seq_along(files_to_move)) {
    file_name <- files_to_move[i]
    src_path <- file.path(source_folder, file_name)
    dest_path <- file.path(destination_folder, file_name)
    
    success <- file.rename(src_path, dest_path)
    if (!success) {
      warning("Failed to move file: ", file_name)
    }
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  message("Done moving ", length(files_to_move), " file(s).")
}
