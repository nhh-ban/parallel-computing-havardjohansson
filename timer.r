# Load the tictoc package
library(tictoc)

# List all the R script files in the "scripts" directory
script_files <- list.files(path = "scripts/", pattern = "\\.R$", full.names = TRUE)

# Loop through each file and source it, timing each one
for (file in script_files) {
  cat(sprintf("Timing: %s\n", file))
  
  tic(file)
  source(file)
  toc()
}
