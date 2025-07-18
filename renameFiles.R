# Define your folder path
# folder_path <- "path/to/your/folder"
folder_path <- "D:/03-Work/01-Science/00-Research Projects/RB Chough Results/testReleaseSizesV2"

# List only files with "(" in the name
files <- list.files(folder_path, full.names = TRUE, pattern = "\\(")

# Loop through matching files
for (file in files) {
  filename <- basename(file)
  
  # Remove " (number)" before extension
  new_filename <- sub(" \\(\\d+\\)(?=\\.[^\\.]+$)", "", filename, perl = TRUE)
  
  # Rename only if changed
  if (filename != new_filename) {
    file.rename(file, file.path(folder_path, new_filename))
  }
}

