#--------------------------------------------------------------
# Read Project Parameters from UI inputs
#--------------------------------------------------------------
read_project_params <- function(team_size = 4, num_solutions = 3) {
  # This function accepts direct parameters instead of reading from CSV
  list(
    c_team = team_size,            # Team capacity 
    x_topic_teams = 1,             # Set to 1 as we're not using topic-based allocation
    diversity_weight = 0.7,        # Weight for diversity (can be adjusted)
    num_solutions = num_solutions  # Number of solutions to find
  )
}

#--------------------------------------------------------------
# Read and Process CSV Survey File Uploaded By The User
#--------------------------------------------------------------
process_survey_data <- function(student_data_path) {
  if (!file.exists(student_data_path)) {
    stop("Uploaded student CSV not found at: ", student_data_path)
  }
  
  # Read the CSV file
  df <- read.csv(student_data_path, 
                 stringsAsFactors = FALSE, 
                 na.strings = c("", "NA"))  # Ensure empty strings are NA
  
  # Log column names for debugging
  message("Actual column names in dataframe:")
  message(paste(names(df), collapse = ", "))
  
  # Check for required columns - case insensitive check for student_id 
  # to accommodate both "student_id" and "Student_ID" formats
  student_id_col <- grep("student_id", names(df), value = TRUE, ignore.case = TRUE)
  
  if (length(student_id_col) == 0) {
    stop("Missing required student_id column in the CSV")
  }
  
  # If the column isn't called "student_id", rename it for consistency
  if (!"student_id" %in% names(df)) {
    # Rename the first matching column to "student_id"
    names(df)[names(df) == student_id_col[1]] <- "student_id"
    message("Renamed column '", student_id_col[1], "' to 'student_id' for consistency")
  }
  
  return(df)
}

#--------------------------------------------------------------
# Extract Diversity Attributes from Survey Data
#--------------------------------------------------------------
extract_diversity_attributes <- function(survey_data) {
  # Identify diversity columns (case insensitive check)
  diversity_cols <- grep("diversity_category", names(survey_data), value = TRUE, ignore.case = TRUE)
  
  if (length(diversity_cols) == 0) {
    warning("No diversity columns found with 'diversity_category' pattern. Looking for potential diversity columns.")
    # Fallback to columns that might contain diversity data
    potential_cols <- c("gender", "major", "background", "expertise", "year", "nationality")
    diversity_cols <- names(survey_data)[tolower(names(survey_data)) %in% potential_cols]
    
    if (length(diversity_cols) == 0) {
      stop("No diversity-related columns found in the survey data.")
    }
  }
  
  message("Diversity columns found: ", paste(diversity_cols, collapse = ", "))
  
  # For each diversity column, extract unique categories
  diversity_categories <- list()
  
  for (col in diversity_cols) {
    unique_values <- unique(survey_data[[col]][!is.na(survey_data[[col]]) & survey_data[[col]] != ""])
    diversity_categories[[col]] <- unique_values
    message("Column '", col, "' has ", length(unique_values), " unique values: ", 
            paste(unique_values, collapse = ", "))
  }
  
  # Return both the category list and column names
  list(
    categories = diversity_categories,
    columns = diversity_cols
  )
}

#--------------------------------------------------------------
# Create Diversity Matrix between Students
#--------------------------------------------------------------
create_diversity_matrix <- function(survey_data, diversity_data) {
  library(dplyr)
  
  diversity_cols <- diversity_data$columns
  n_students <- nrow(survey_data)
  
  # Create a diversity matrix (higher values = more diverse)
  diversity_matrix <- matrix(0, nrow = n_students, ncol = n_students)
  
  # For each pair of students
  for (i in 1:(n_students-1)) {
    for (j in (i+1):n_students) {
      # Count how many diversity attributes differ
      diversity_count <- 0
      total_attributes <- 0
      
      for (col in diversity_cols) {
        # Skip missing values
        if (is.na(survey_data[[col]][i]) || is.na(survey_data[[col]][j])) {
          next
        }
        
        total_attributes <- total_attributes + 1
        
        # Compare categories
        if (survey_data[[col]][i] != survey_data[[col]][j]) {
          diversity_count <- diversity_count + 1
        }
      }
      
      # Set the matrix symmetrically (normalized by total attributes if available)
      if (total_attributes > 0) {
        # Normalize to 0-1 range
        normalized_diversity <- diversity_count / total_attributes
        diversity_matrix[i, j] <- normalized_diversity
        diversity_matrix[j, i] <- normalized_diversity
      }
    }
  }
  
  return(diversity_matrix)
}

#--------------------------------------------------------------
# Consolidated Function to Read All Data for the MIP Model
#--------------------------------------------------------------
read_data <- function(student_data_path, team_size = 4, num_solutions = 3) {
  # Check if required packages are installed
  required_packages <- c("dplyr", "tidyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' needed for this function. Please install it.")
    }
  }
  
  # Load required packages
  library(dplyr)
  library(tidyr)
  
  message("Reading data from: ", student_data_path)
  
  # For backward compatibility, check if we need to read from user_inputs.csv
  try({
    if (file.exists("user_inputs.csv")) {
      # Read from file to get parameters
      user_inputs <- read.csv("user_inputs.csv", header = FALSE)
      if (ncol(user_inputs) >= 2) {
        # Extract team size from the file
        team_size <- as.numeric(user_inputs[nrow(user_inputs), 1])
        message("Using team size from user_inputs.csv: ", team_size)
      }
    }
  }, silent = TRUE)
  
  # 1) Read project parameters from function inputs
  project_params <- read_project_params(team_size, num_solutions)
  
  # 2) Read survey data (uploaded CSV)
  survey_data <- process_survey_data(student_data_path)
  
  # 3) Extract diversity attributes
  diversity_data <- extract_diversity_attributes(survey_data)
  
  # 4) Create diversity matrix (diversity between students)
  diversity_matrix <- create_diversity_matrix(survey_data, diversity_data)
  
  # Some useful parameters
  n_students <- nrow(survey_data)
  n_teams <- ceiling(n_students / project_params$c_team)
  
  # Log key information
  message("Number of students: ", n_students)
  message("Team size: ", project_params$c_team)
  message("Expected number of teams: ", n_teams)
  message("Diversity attributes found: ", paste(diversity_data$columns, collapse = ", "))
  
  # Return a consolidated list of all parameters for the model
  list(
    c_team = project_params$c_team,                # Team capacity
    num_solutions = project_params$num_solutions,  # Number of solutions to find
    diversity_weight = project_params$diversity_weight,  # Weight for diversity
    n_students = n_students,
    n_teams = n_teams,
    student_ids = survey_data$student_id,
    diversity_data = diversity_data,
    diversity_matrix = diversity_matrix,
    survey_data = survey_data
  )
}