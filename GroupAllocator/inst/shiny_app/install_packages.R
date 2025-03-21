# Install required packages
# Run this script once before using the application

# List of required packages
required_packages <- c(
  "shiny", 
  "shinyjs", 
  "DT",
  "dplyr", 
  "tidyr", 
  "jsonlite",
  "slam", 
  "ompr", 
  "ompr.roi", 
  "ROI.plugin.glpk"
)

# Check which ones are not installed
packages_to_install <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

# Install missing packages
if (length(packages_to_install) > 0) {
  message("Installing the following packages: ", paste(packages_to_install, collapse = ", "))
  install.packages(packages_to_install)
} else {
  message("All required packages are already installed.")
}

# Load all packages
lapply(required_packages, library, character.only = TRUE)

message("Setup complete! You can now run the application.")