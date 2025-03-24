#===========================================================================
# TABLE OF CONTENTS
#===========================================================================
# 1. OPTIMIZATION FUNCTIONS
#    - run_multi_solution_optimization: Main optimization workflow
#
# 2. HELPER FUNCTIONS
#    - process_solution: Convert solution variables to assignments
#===========================================================================

#===========================================================================
# 1. OPTIMIZATION FUNCTIONS
#===========================================================================

#' Run the Optimization and Find Top-K Solutions
#'
#' Reads user inputs and survey data,
#' builds and solves the MIP model to find the top-k solutions by score,
#' and processes the solution into final assignments.
#'
#' @param student_data_path Path to the CSV file containing student survey data.
#' @param k_solutions Number of top solutions to find (default: 3)
#' @param score_gap_percent Maximum percentage below optimal score for solutions (default: 5.0)
#' @return A list with elements: solver_results (a list of raw optimization results) and
#'         assignments_list (a list of data frames with student assignments for each solution).
#' @export
run_multi_solution_optimization <- function(student_data_path = "survey_data.csv", 
                                           k_solutions = 3,
                                           score_gap_percent = 5.0) {
  # Check if required packages are installed
  required_packages <- c("dplyr", "tidyr", "slam", "ompr", "ompr.roi", "ROI.plugin.glpk")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' needed for this function. Please install it.")
    }
  }
  
  # Load required packages
  library(dplyr)
  library(tidyr)
  library(slam)        
  library(ompr)  
  library(ompr.roi)
  library(ROI.plugin.glpk)

  #---------------------------------------------------------------------------
  # Data Loading and Parameter Extraction
  #---------------------------------------------------------------------------
  
  # 1) Read all data
  data_list <- read_data(student_data_path)
  
  # 2) Extract parameters from the returned list
  c_team         <- data_list$c_team
  b_subteam      <- data_list$b_subteam
  x_topic_teams  <- data_list$x_topic_teams
  n_topics       <- data_list$n_topics
  n_subteams     <- data_list$n_subteams
  n_groups       <- data_list$n_groups
  group_size     <- data_list$group_size
  topics         <- data_list$topics
  valid_subteams <- data_list$valid_subteams
  pref_array     <- data_list$pref_array
  survey_data    <- data_list$survey_data

  # 3) Constants for the model
  p_penalty   <- 0.5  # Penalty for empty slots
  minCapacity <- 1    # Minimum capacity
  
  # Print out extensive diagnostic information
  message("Model parameters:")
  message(sprintf("Team size: %d, Subteam size: %d, Max teams per topic: %d", c_team, b_subteam, x_topic_teams))
  message(sprintf("Number of groups: %d, Number of topics: %d, Number of subteams: %d", n_groups, n_topics, n_subteams))
  
  # Calculate total students to help diagnose capacity issues
  total_students <- sum(group_size)
  max_possible_teams <- floor(total_students / minCapacity)
  message(sprintf("Total student count: %d, Minimum capacity: %d, Maximum possible teams: %d", 
                 total_students, minCapacity, max_possible_teams))
  
  # Print topic and subteam counts to verify data
  message("Topics (", n_topics, "): ", paste(topics, collapse = ", "))
  message("Subteams (", n_subteams, "): ", paste(valid_subteams, collapse = ", "))
  
  #---------------------------------------------------------------------------
  # Base Model Definition
  #---------------------------------------------------------------------------
  
  # Initialize lists to store results
  all_solver_results <- list()
  all_assignments <- list()
  all_objective_values <- c()
  all_solutions <- list()  # To store solution variables
  
  # Create the base MIP model (this will be reused with modifications)
  base_model <- MIPModel() %>%
    # Define variables
    add_variable(A[g, t, j, s],
                g = 1:n_groups,
                t = 1:n_topics,
                j = 1:x_topic_teams,
                s = 1:n_subteams,
                type = "binary") %>%
    add_variable(slack[t, j, s],
                t = 1:n_topics,
                j = 1:x_topic_teams,
                s = 1:n_subteams,
                lb = 0,
                type = "integer") %>%
    add_variable(Z[t, j],
                t = 1:n_topics,
                j = 1:x_topic_teams,
                type = "binary") %>%
    
    # Objective function with modified weights
    set_objective(
      # Main preference score
      sum_expr(pref_array[g, t, s] * A[g, t, j, s],
              g = 1:n_groups, t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams) +
        
      # Topic coverage bonus
      50 * sum_expr(Z[t, 1], t = 1:n_topics) -
        
      # Penalty for empty slots
      p_penalty * sum_expr(slack[t, j, s],
                          t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams),
      sense = "max"
    ) %>%
    
    # Constraints
    # (1) Each group is assigned exactly once
    add_constraint(
      sum_expr(A[g, t, j, s],
              t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams) == 1,
      g = 1:n_groups
    ) %>%
    
    # (2) If team (t,j) is formed, then at least one group is assigned to each subteam s
    add_constraint(
      sum_expr(A[g, t, j, s], g = 1:n_groups) >= Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    ) %>%
    
    # (3) Sub-team capacity constraint with flexibility
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups) <= b_subteam,
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    ) %>%
    
    # Add slack to represent empty spots in activated subteams
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups) + slack[t, j, s] == b_subteam * Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    ) %>%

    # Force slack to be 0 for non-activated teams
    add_constraint(
      slack[t, j, s] <= b_subteam * Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    ) %>%
    
    # (4) Tie assignments to team activation
    add_constraint(
      sum_expr(A[g, t, j, s], g = 1:n_groups, s = 1:n_subteams) <= n_groups * Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams
    ) %>%
    
    # (5) Relaxed minimum team capacity
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups, s = 1:n_subteams) >= 1 * Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams
    ) %>%
    
    # (6) Relaxed subteam threshold
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups) >= Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    )

    # Add constraint to prioritize filling j=1 before activating j=2 for the same topic
    for (t in 1:n_topics) {
      for (j in 2:x_topic_teams) {
        base_model <- base_model %>% add_constraint(
          Z[t, j] <= Z[t, j-1],
          t = t, j = j
        )
      }
    }
  
  #---------------------------------------------------------------------------
  # Find Many Possible Solutions
  #---------------------------------------------------------------------------
  
  # First, find the optimal solution
  message("Finding optimal solution...")
  
  result <- tryCatch({
    solve_model(
      base_model,
      with_ROI(
        solver = "glpk",
        verbose = TRUE,
        control = list(tm_limit = 1800)  # 30 minutes time limit
      )
    )
  }, error = function(e) {
    message("Solver error: ", e$message)
    return(list(status = "error", error_message = e$message))
  })
  
  # Check if optimal solution was found
  if (!result$status %in% c("optimal", "relaxed", "integer optimal", "success", "time limit exceeded")) {
    stop("Could not find any feasible solution. Please check your data and constraints.")
  }
  
  # Get the optimal objective value
  optimal_obj_val <- objective_value(result)
  message("Optimal solution found with objective value: ", optimal_obj_val)
  
  # Store the first solution
  solution_idx <- 1
  solution_A <- get_solution(result, A[g, t, j, s])
  all_solver_results[[solution_idx]] <- result
  all_solutions[[solution_idx]] <- solution_A
  all_objective_values[solution_idx] <- optimal_obj_val
  
  # Process the optimal solution assignments
  optimal_assignments <- process_solution(solution_A, survey_data, topics, valid_subteams, 
                                         group_size, optimal_obj_val, solution_idx, pref_array)
  all_assignments[[solution_idx]] <- optimal_assignments
  
  # Set a minimum acceptable score for alternatives
  min_acceptable_score <- optimal_obj_val * (1 - score_gap_percent/100)
  message("Will accept solutions with scores above: ", min_acceptable_score)
  
  # Set maximum number of attempts to find solutions
  max_attempts <- min(k_solutions * 3, 20)  # Cap at 20 to prevent excessive computation
  
  # Try to find more solutions (we'll try to find more than k, then select the best k)
  found_solutions <- 1
  for (attempt in 1:max_attempts) {
    # If we already have enough solutions, stop
    if (found_solutions >= 2*k_solutions) {
      message("Found enough potential solutions (", found_solutions, "). Stopping search.")
      break
    }
    
    message(paste("Finding additional solution - attempt", attempt))
    
    # Create a new model that excludes all previous solutions
    model <- base_model
    
    # Add constraints to exclude all previous solutions
    for (prev_sol_idx in 1:found_solutions) {
      prev_A <- all_solutions[[prev_sol_idx]]
      
      # Find which variables were 1 in the previous solution
      active_vars <- prev_A %>% 
        filter(value > 0.5) %>%
        select(g, t, j, s)
      
      if (nrow(active_vars) == 0) {
        message("Warning: Previous solution has no active variables")
        next
      }
      
      # Add an integer cut constraint
      model <- model %>%
        add_constraint(
          sum_expr(A[active_vars$g[i], active_vars$t[i], active_vars$j[i], active_vars$s[i]],
                  i = 1:nrow(active_vars)) <= nrow(active_vars) - 1
        )
    }
    
    # Add minimum score constraint
    model <- model %>%
      add_constraint(
        sum_expr(pref_array[g, t, s] * A[g, t, j, s],
                g = 1:n_groups, t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams) +
          50 * sum_expr(Z[t, 1], t = 1:n_topics) -
          p_penalty * sum_expr(slack[t, j, s],
                              t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams) >= min_acceptable_score
      )
    
    # Solve the model
    result <- tryCatch({
      solve_model(
        model,
        with_ROI(
          solver = "glpk",
          verbose = TRUE,
          control = list(tm_limit = 1800)  # 30 minutes time limit
        )
      )
    }, error = function(e) {
      message("Solver error: ", e$message)
      return(list(status = "error", error_message = e$message))
    })
    
    # Check if a feasible solution was found
    if (!result$status %in% c("optimal", "relaxed", "integer optimal", "success", "time limit exceeded")) {
      message("Could not find more solutions. Stopping search.")
      break
    }
    
    # Get the objective value
    obj_val <- tryCatch({
      objective_value(result)
    }, error = function(e) {
      message("Error getting objective value: ", e$message)
      next
    })
    
    message(paste("Found solution with objective value:", obj_val))
    
    # Store this solution
    found_solutions <- found_solutions + 1
    solution_A <- get_solution(result, A[g, t, j, s])
    all_solver_results[[found_solutions]] <- result
    all_solutions[[found_solutions]] <- solution_A
    all_objective_values[found_solutions] <- obj_val
    
    # Process the solution assignments
    solution_assignments <- process_solution(solution_A, survey_data, topics, valid_subteams, 
                                           group_size, obj_val, found_solutions, pref_array)
    all_assignments[[found_solutions]] <- solution_assignments
  }
  
  #---------------------------------------------------------------------------
  # Select Top-K Solutions by Score
  #---------------------------------------------------------------------------
  
  # Remove any NULL entries
  valid_indices <- which(!sapply(all_assignments, is.null))
  
  if (length(valid_indices) == 0) {
    stop("No valid solutions were found. Please check your data and parameters.")
  }
  
  message("Found ", length(valid_indices), " valid solutions.")
  
  # Keep only valid solutions
  filtered_assignments <- all_assignments[valid_indices]
  filtered_objective_values <- all_objective_values[valid_indices]
  filtered_solver_results <- all_solver_results[valid_indices]
  
  # Sort solutions by score (highest first)
  sorted_indices <- order(filtered_objective_values, decreasing = TRUE)
  
  # Take only the top k solutions (or all if fewer than k)
  k_actual <- min(k_solutions, length(sorted_indices))
  top_k_indices <- sorted_indices[1:k_actual]
  
  message("Returning top ", k_actual, " solutions by score.")
  
  # Selected solutions for return
  assignments_list <- filtered_assignments[top_k_indices]
  objective_values <- filtered_objective_values[top_k_indices]
  solver_results <- filtered_solver_results[top_k_indices]
  
  # Fix solution numbers to be sequential (1, 2, 3, etc.)
  # This is important because the UI expects solutions to be numbered from 1
  for (i in 1:length(assignments_list)) {
    if (!is.null(assignments_list[[i]])) {
      assignments_list[[i]]$solution_number <- i
    }
  }
  
  # Calculate score differences from best solution
  best_score <- max(objective_values)
  score_diffs <- best_score - objective_values
  
  # Return results 
  list(
    solver_results = solver_results,
    assignments_list = assignments_list,
    objective_values = objective_values,
    score_diffs = score_diffs,
    best_solution_index = which.max(objective_values)  # Always 1 since we sorted
  )
}

#===========================================================================
# 2. HELPER FUNCTIONS
#===========================================================================

#' Process a solution into assignments
#'
#' Helper function to process solution variables into assignment data frames
#'
#' @param solution_A The solution variables from the optimization
#' @param survey_data The original survey data
#' @param topics List of topic names
#' @param valid_subteams List of subteam names
#' @param group_size Vector of group sizes
#' @param objective_value The objective value for this solution
#' @param solution_number The solution index
#' @return A data frame of assignments
process_solution <- function(solution_A, survey_data, topics, valid_subteams, 
                            group_size, objective_value, solution_number, pref_array = NULL) {

  # Filter to keep only assignments (A==1)
  assigned <- subset(solution_A, value > 0.5)
  
  # Check if we have assignments
  if (nrow(assigned) == 0) {
    message("Warning: Solution found but no assignments were made")
    return(NULL)
  }
  
  # Map indices to labels
  assigned$topic_name <- topics[assigned$t]
  assigned$subteam_name <- valid_subteams[assigned$s]
  assigned$project_team <- paste0(assigned$topic_name, "_team", assigned$j)
  
  # Retrieve student IDs for each group
  group_id_cols <- grep("Student_ID", names(survey_data), value = TRUE)
  
  final_output_list <- lapply(seq_len(nrow(assigned)), function(k) {
    # Extract the assigned row
    g_val <- assigned$g[k]
    project_team_val <- assigned$project_team[k]
    subteam_val <- assigned$subteam_name[k]
    
    # Get all student IDs in that group
    row_ids <- survey_data[g_val, group_id_cols, drop = FALSE]
    
    # Filter out NA
    student_ids <- unlist(row_ids[!is.na(row_ids)])
    
    # Build a data frame: one row per student
    if (length(student_ids) == 0) {
      return(NULL)  # no valid IDs
    }
    
    data.frame(
      student_id = student_ids,
      project_team = project_team_val,
      subteam = subteam_val,
      solution_number = solution_number,
      solution_score = objective_value,
      group_id = g_val,
      stringsAsFactors = FALSE
    )
  })
  
  # Filter out NULL entries
  final_output_list <- Filter(Negate(is.null), final_output_list)
  
  # Check if we have any valid outputs
  if (length(final_output_list) == 0) {
    message("Warning: No valid student assignments could be created for solution ", solution_number)
    return(NULL)
  }
  
  # Combine them all
  final_output <- do.call(rbind, final_output_list)
  
  # Remove duplicates
  final_output <- unique(final_output)
  
  # Sort by project team
  final_output <- final_output[order(final_output$project_team), ]
  
  # Add individual scores
  final_output$individual_score <- 0

  if (is.null(pref_array)) {
        # Try to get it from the parent environment
        if (exists("pref_array", envir = parent.frame())) {
            pref_array <- get("pref_array", envir = parent.frame())
        } else {
            # If not available, leave individual scores as 0
            warning("pref_array not available; individual scores will be set to 0")
        }
    }
  
  # Calculate individual score for each student
  for (i in 1:nrow(final_output)) {
    student_id <- final_output$student_id[i]
    project_team <- final_output$project_team[i]
    subteam <- final_output$subteam[i]
    
    # Extract topic from project_team
    topic_parts <- strsplit(project_team, "_team")[[1]]
    if (length(topic_parts) > 0) {
      topic <- topic_parts[1]
    
      # Find the group in survey data
      student_cols <- grep("Student_ID", names(survey_data), value = TRUE)
      group_indices <- sapply(1:nrow(survey_data), function(row_idx) {
        row_data <- survey_data[row_idx, student_cols, drop = FALSE]
        any(row_data == student_id, na.rm = TRUE)
      })
      
      if (any(group_indices)) {
        group_idx <- which(group_indices)[1]
        topic_idx <- match(topic, topics)
        subteam_idx <- match(subteam, valid_subteams)
        
        if (!is.na(topic_idx) && !is.na(subteam_idx)) {
          # Get preference score from the array
          student_score <- pref_array[group_idx, topic_idx, subteam_idx]
          final_output$individual_score[i] <- student_score
        }
      }
    }
  }
  
  # Print a summary
  message(paste("Solution", solution_number, "assigned", nrow(final_output), "students to", 
                length(unique(final_output$project_team)), "project teams"))
  
  # Return the assignments with individual scores
  return(final_output)
}