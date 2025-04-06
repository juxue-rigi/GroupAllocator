#--------------------------------------------------------------
# Read Project Parameters from UI inputs
#--------------------------------------------------------------
read_project_params <- function(team_size = 4, num_solutions = 3) {
  # This function now accepts direct parameters instead of reading from CSV
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
# Build Diversity Coefficient Matrix for Each Attribute
#--------------------------------------------------------------
build_diversity_coefficients <- function(survey_data, diversity_data) {
  # Prepare empty matrices for each diversity attribute
  diversity_cols <- diversity_data$columns
  n_students <- nrow(survey_data)
  
  # Create matrices to store diversity coefficients
  diversity_coefficients <- list()
  
  # For each diversity attribute
  for (col in diversity_cols) {
    # Get unique categories for this attribute
    categories <- diversity_data$categories[[col]]
    
    if (length(categories) <= 1) {
      message("Skipping diversity attribute '", col, "' as it has only ", length(categories), " categories")
      next
    }
    
    # Create a student-by-category matrix (1 if student has that category, 0 otherwise)
    student_category_matrix <- matrix(0, nrow = n_students, ncol = length(categories))
    colnames(student_category_matrix) <- categories
    
    # Fill the matrix
    for (i in 1:n_students) {
      category_val <- survey_data[[col]][i]
      if (!is.na(category_val) && category_val %in% categories) {
        category_idx <- match(category_val, categories)
        student_category_matrix[i, category_idx] <- 1
      }
    }
    
    # Store this matrix
    diversity_coefficients[[col]] <- student_category_matrix
    
    # Log for debugging
    message("Created diversity coefficient matrix for attribute: ", col)
    message("  with categories: ", paste(categories, collapse = ", "))
  }
  
  # Return the list of coefficient matrices
  list(
    coefficients = diversity_coefficients,
    columns = diversity_cols
  )
}

#--------------------------------------------------------------
# Create Similarity/Dissimilarity Matrices between Students
#--------------------------------------------------------------
create_diversity_matrix <- function(survey_data, diversity_data) {
  library(dplyr)
  
  diversity_cols <- diversity_data$columns
  n_students <- nrow(survey_data)
  
  # Create a similarity matrix (higher values = more similar, less diverse)
  # and a dissimilarity matrix (higher values = more diverse)
  similarity_matrix <- matrix(0, nrow = n_students, ncol = n_students)
  diversity_matrix <- matrix(0, nrow = n_students, ncol = n_students)
  
  # For each pair of students
  for (i in 1:(n_students-1)) {
    for (j in (i+1):n_students) {
      # Count how many diversity attributes match
      match_count <- 0
      diversity_count <- 0
      
      for (col in diversity_cols) {
        # Skip missing values
        if (is.na(survey_data[[col]][i]) || is.na(survey_data[[col]][j])) {
          next
        }
        
        # Compare categories
        if (survey_data[[col]][i] == survey_data[[col]][j]) {
          match_count <- match_count + 1
        } else {
          diversity_count <- diversity_count + 1
        }
      }
      
      # Set the matrices symmetrically
      similarity_matrix[i, j] <- match_count
      similarity_matrix[j, i] <- match_count
      
      diversity_matrix[i, j] <- diversity_count
      diversity_matrix[j, i] <- diversity_count
    }
  }
  
  # Scale to 0-1 range if there are any non-zero values
  if (max(similarity_matrix) > 0) {
    similarity_matrix <- similarity_matrix / max(similarity_matrix)
  }
  if (max(diversity_matrix) > 0) {
    diversity_matrix <- diversity_matrix / max(diversity_matrix)
  }
  
  # Return both matrices
  list(
    similarity = similarity_matrix,    # Higher = more similar
    diversity = diversity_matrix       # Higher = more diverse
  )
}

#--------------------------------------------------------------
# Consolidated Function to Read All Data for the Diversity Optimization Model
#--------------------------------------------------------------
read_data <- function(student_data_path, team_size = 4, num_solutions = 3) {
  # Check if required packages are installed
  required_packages <- c("dplyr", "tidyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' needed for this function. Please install it.")
    }
  }
  
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
  
  # 4) Build diversity coefficient matrices
  diversity_coefficients <- build_diversity_coefficients(survey_data, diversity_data)
  
  # 5) Create diversity matrices (similarity/dissimilarity between students)
  diversity_matrices <- create_diversity_matrix(survey_data, diversity_data)
  
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
    diversity_coef = diversity_coefficients,
    diversity_matrices = diversity_matrices,
    survey_data = survey_data,
    
    # For compatibility with other models
    topics = c("team"),
    valid_subteams = c("member"),
    pref_array = array(0) # Placeholder pref array
  )
}