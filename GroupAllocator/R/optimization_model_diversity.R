#===========================================================================
# DIVERSITY-FOCUSED STUDENT TEAM ALLOCATION
#===========================================================================
# This code optimizes student team allocation to maximize diversity
# within teams, using a greedy algorithm approach that's robust and efficient.
#===========================================================================

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
    survey_data = survey_data
  )
}

#' Calculate diversity score for a solution
#'
#' @param assignments Student assignment data
#' @param diversity_data The diversity information extracted from survey data
#' @param survey_data Original survey data with student information
#' @return A list with various diversity metrics
calculate_diversity_score <- function(assignments, diversity_data, survey_data) {
  library(dplyr)
  
  # Get unique teams
  teams <- unique(assignments$team_number)
  
  # Get diversity columns
  diversity_cols <- diversity_data$columns
  
  # Prepare results list
  diversity_scores <- list()
  
  # For each team, calculate diversity metrics
  for (team in teams) {
    team_students <- assignments$student_id[assignments$team_number == team]
    
    # Skip if team is empty
    if (length(team_students) == 0) next
    
    team_scores <- list()
    
    # For each diversity attribute
    for (col in diversity_cols) {
      # Get categories for students in this team
      student_indices <- match(team_students, survey_data$student_id)
      categories <- survey_data[[col]][student_indices]
      categories <- categories[!is.na(categories)]
      
      # Skip if no data
      if (length(categories) == 0) {
        team_scores[[col]] <- list(unique_count = 0, entropy = 0, normalized_entropy = 0)
        next
      }
      
      # Count unique categories
      unique_count <- length(unique(categories))
      
      # Calculate entropy (more diverse = higher entropy)
      category_table <- table(categories)
      category_probs <- category_table / sum(category_table)
      entropy <- -sum(category_probs * log(category_probs))
      
      # Calculate a normalized diversity score (0-1)
      max_entropy <- -log(1/length(category_table))
      norm_entropy <- if (max_entropy > 0) entropy / max_entropy else 0
      
      # Store metrics
      team_scores[[col]] <- list(
        unique_count = unique_count,
        entropy = entropy,
        normalized_entropy = norm_entropy
      )
    }
    
    diversity_scores[[paste0("team_", team)]] <- team_scores
  }
  
  # Calculate overall solution diversity score
  overall_scores <- list()
  for (col in diversity_cols) {
    col_scores <- sapply(diversity_scores, function(team) {
      if (!is.null(team[[col]])) team[[col]]$normalized_entropy else 0
    })
    overall_scores[[col]] <- mean(col_scores, na.rm = TRUE)
  }
  diversity_scores$overall <- overall_scores
  
  return(diversity_scores)
}

#' Run the Optimization with Diversity Focus and Find Top-K Solutions
#'
#' @param student_data_path Path to the CSV file containing student data
#' @param team_size Size of teams to form (default: 4)
#' @param k_solutions Number of top solutions to find (default: 3)
#' @param score_gap_percent Maximum percentage below optimal score for solutions (default: 5.0)
#' @return A list with elements: solver_results and assignments_list
#' @export
run_diversity_optimization <- function(student_data_path = "survey_data.csv", 
                                       team_size = 4,
                                       k_solutions = 3,
                                       score_gap_percent = 5.0) {
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

  #---------------------------------------------------------------------------
  # Data Loading and Parameter Extraction with Type Checking
  #---------------------------------------------------------------------------
  
  # Read all data with error handling
  tryCatch({
    data_list <- read_data(student_data_path, team_size, k_solutions)
  }, error = function(e) {
    stop("Error reading data: ", e$message)
  })
  
  # Ensure we have student data
  if (is.null(data_list$survey_data) || nrow(data_list$survey_data) == 0) {
    stop("No student data found in the uploaded CSV")
  }
  
  # Get student IDs
  student_ids <- data_list$survey_data$student_id
  n_students <- length(student_ids)
  message("Number of students detected: ", n_students)
  
  # Get diversity categories
  diversity_cols <- grep("diversity_category", names(data_list$survey_data), value = TRUE, ignore.case = TRUE)
  if (length(diversity_cols) == 0) {
    stop("No diversity categories found in the uploaded CSV. Make sure column names include 'diversity_category'")
  }
  
  message("Diversity columns found: ", paste(diversity_cols, collapse = ", "))
  
  # Calculate team count
  team_size <- as.numeric(team_size)
  n_teams <- ceiling(n_students / team_size)
  
  # Print model parameters for debugging
  message("Model parameters:")
  message(sprintf("Team size: %d, Number of students: %d, Number of teams: %d", 
                  team_size, n_students, n_teams))

  # Use a greedy algorithm to assign students to teams
  # This avoids complex optimization and quadratic expressions
  
  message("Using greedy algorithm for team assignment...")
  
  #---------------------------------------------------------------------------
  # Create multiple solutions using different random seeds
  #---------------------------------------------------------------------------
  
  all_assignments_list <- list()
  all_objective_values <- numeric(k_solutions)
  all_diversity_scores <- list()
  
  # Function to calculate diversity between two students
  calc_diversity <- function(student1_idx, student2_idx, survey_data, diversity_cols) {
    # Exit early if indices are invalid
    if (student1_idx > nrow(survey_data) || student2_idx > nrow(survey_data) ||
        student1_idx < 1 || student2_idx < 1) {
      return(0)
    }
    
    # Count diversity attributes that differ
    diversity_count <- 0
    
    for (col in diversity_cols) {
      # Skip if data is missing
      if (is.na(survey_data[[col]][student1_idx]) || is.na(survey_data[[col]][student2_idx])) {
        next
      }
      
      # Add 1 to diversity count if values differ
      if (survey_data[[col]][student1_idx] != survey_data[[col]][student2_idx]) {
        diversity_count <- diversity_count + 1
      }
    }
    
    return(diversity_count)
  }
  
  # Generate multiple solutions
  for (solution_idx in 1:k_solutions) {
    message(sprintf("Creating solution %d of %d", solution_idx, k_solutions))
    
    # Set seed for reproducibility, but different for each solution
    set.seed(42 + solution_idx)
    
    # Initialize empty teams
    teams <- vector("list", n_teams)
    for (i in 1:n_teams) {
      teams[[i]] <- integer(0)
    }
    
    # Randomize student order for variety between solutions
    student_order <- sample(1:n_students)
    
    # First, place one student in each team as "seeds"
    for (t in 1:min(n_teams, n_students)) {
      teams[[t]] <- c(teams[[t]], student_order[t])
    }
    
    # Assign remaining students
    if (n_students > n_teams) {
      remaining_students <- student_order[(n_teams+1):length(student_order)]
      
      for (student_idx in remaining_students) {
        # Find best team for this student (maximizes diversity)
        best_team <- 1
        best_diversity <- -Inf
        
        for (t in 1:n_teams) {
          # Skip teams that are full
          if (length(teams[[t]]) >= team_size) {
            next
          }
          
          # Calculate diversity if student joins this team
          team_diversity <- 0
          if (length(teams[[t]]) > 0) {
            for (member_idx in teams[[t]]) {
              team_diversity <- team_diversity + 
                calc_diversity(student_idx, member_idx, data_list$survey_data, diversity_cols)
            }
            team_diversity <- team_diversity / length(teams[[t]])  # Normalize
          }
          
          if (team_diversity > best_diversity) {
            best_diversity <- team_diversity
            best_team <- t
          }
        }
        
        # Assign student to best team
        teams[[best_team]] <- c(teams[[best_team]], student_idx)
      }
    }
    
    # Calculate total diversity score for this solution
    total_diversity <- 0
    team_count <- 0
    
    for (t in 1:n_teams) {
      if (length(teams[[t]]) >= 2) {
        team_count <- team_count + 1
        team_diversity <- 0
        pairs_count <- 0
        
        # Calculate diversity between all pairs in the team
        for (i in 1:(length(teams[[t]])-1)) {
          for (j in (i+1):length(teams[[t]])) {
            student1_idx <- teams[[t]][i]
            student2_idx <- teams[[t]][j]
            
            team_diversity <- team_diversity + 
              calc_diversity(student1_idx, student2_idx, data_list$survey_data, diversity_cols)
            pairs_count <- pairs_count + 1
          }
        }
        
        # Normalize by number of pairs
        if (pairs_count > 0) {
          team_diversity <- team_diversity / pairs_count
          total_diversity <- total_diversity + team_diversity
        }
      }
    }
    
    # Normalize by number of teams
    obj_value <- if (team_count > 0) total_diversity / team_count * 100 else 0
    message(sprintf("Solution %d has diversity score: %.2f", solution_idx, obj_value))
    
    # Create assignment dataframe
    results <- data.frame(
      student_id = character(0),
      project_team = character(0),
      subteam = character(0),
      team_number = integer(0),
      solution_number = integer(0),
      stringsAsFactors = FALSE
    )
    
    # Populate assignment dataframe
    for (t in 1:n_teams) {
      for (student_idx in teams[[t]]) {
        results <- rbind(results, data.frame(
          student_id = data_list$survey_data$student_id[student_idx],
          project_team = paste0("Team_", t),
          subteam = "member",  # Default subteam for compatibility
          team_number = t,
          solution_number = solution_idx,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Skip empty solutions
    if (nrow(results) == 0) {
      message("Solution ", solution_idx, " is empty. Skipping.")
      next
    }
    
    # Add solution score and individual score
    results$solution_score <- obj_value
    results$individual_score <- obj_value / nrow(results)
    results$group_id <- results$team_number
    
    # Store this solution
    all_assignments_list[[solution_idx]] <- results
    all_objective_values[solution_idx] <- obj_value
    
    # Calculate diversity metrics for this solution
    diversity_scores <- calculate_diversity_score(results, data_list$diversity_data, data_list$survey_data)
    all_diversity_scores[[solution_idx]] <- diversity_scores
  }
  
  # Check if we found any valid solutions
  valid_indices <- which(!sapply(all_assignments_list, is.null))
  if (length(valid_indices) == 0) {
    stop("No valid solutions were generated. Please check your data.")
  }
  
  # Select only valid solutions
  all_assignments_list <- all_assignments_list[valid_indices]
  all_objective_values <- all_objective_values[valid_indices]
  all_diversity_scores <- all_diversity_scores[valid_indices]
  
  # Find the best solution
  best_solution_index <- which.max(all_objective_values)
  
  # Create a dataframe with diversity metrics
  overall_diversity_metrics <- lapply(1:length(valid_indices), function(idx) {
    scores <- all_diversity_scores[[idx]]
    if (!is.null(scores$overall)) {
      overall_score <- mean(unlist(scores$overall), na.rm = TRUE)
      return(data.frame(
        solution_number = idx,
        diversity_score = overall_score,
        objective_value = all_objective_values[idx]
      ))
    } else {
      return(data.frame(
        solution_number = idx,
        diversity_score = 0,
        objective_value = all_objective_values[idx]
      ))
    }
  })
  
  overall_diversity_df <- do.call(rbind, overall_diversity_metrics)
  
  # Calculate score differences
  best_score <- max(all_objective_values)
  score_diffs <- best_score - all_objective_values
  
  # Renumber solutions to be sequential
  for (i in 1:length(all_assignments_list)) {
    all_assignments_list[[i]]$solution_number <- i
  }
  
  # Return results in the expected format
  list(
    solver_results = NULL, # We're not using a formal solver
    assignments_list = all_assignments_list,
    objective_values = all_objective_values,
    score_diffs = score_diffs,
    diversity_scores = all_diversity_scores,
    overall_diversity = overall_diversity_df,
    best_solution_index = best_solution_index
  )
}