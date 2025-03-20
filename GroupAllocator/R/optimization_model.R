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
  
  # Initialize lists to store results
  solver_results <- list()
  assignments_list <- list()
  objective_values <- c()
  solutions <- list()
  
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
    
    # Add slack to represent empty spots in subteams
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups) + slack[t, j, s] == b_subteam,
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
    
    # (6) Make topic team requirement optional (handled via objective)
    
    # (7) Relaxed subteam threshold
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups) >= Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    )
  
  # Step 1: Find the optimal solution first
  message("Finding optimal solution...")
  
  # Solve with no additional constraints to get the optimal solution
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
  
  # Check if a feasible solution was found
  if (!result$status %in% c("optimal", "relaxed", "integer optimal", "success", "time limit exceeded")) {
    stop("Could not find any feasible solution. Please check your data and constraints.")
  }
  
  # Get the optimal objective value
  optimal_obj_val <- tryCatch({
    objective_value(result)
  }, error = function(e) {
    message("Error getting objective value: ", e$message)
    stop("Could not extract objective value from optimal solution.")
  })
  
  message("Optimal solution found with objective value: ", optimal_obj_val)
  
  # Process the optimal solution
  solver_results[[1]] <- result
  objective_values[1] <- optimal_obj_val
  
  # Extract and store the optimal solution variables
  solution_A <- get_solution(result, A[g, t, j, s])
  solutions[[1]] <- solution_A
  
  # Process the solution assignments
  optimal_assignments <- process_solution(solution_A, survey_data, topics, valid_subteams, 
                                         group_size, objective_values[1], 1)
  
  # Store the assignments
  assignments_list[[1]] <- optimal_assignments
  
  # Calculate the minimum acceptable score (based on the gap)
  min_acceptable_score <- optimal_obj_val * (1 - score_gap_percent/100)
  message("Will accept solutions with scores above: ", min_acceptable_score)
  
  # Step 2: Find additional solutions using integer cuts
  # We'll add a constraint to exclude known solutions, but we won't enforce
  # a minimum difference - just get the next best solution
  for (sol_idx in 2:k_solutions) {
    message(paste("Finding solution", sol_idx, "of", k_solutions))
    
    # Start with the base model
    model <- base_model
    
    # Add constraints to exclude all previous solutions
    for (prev_sol in 1:(sol_idx-1)) {
      prev_A <- solutions[[prev_sol]]
      
      # Find which variables were 1 in the previous solution
      active_vars <- prev_A %>% 
        filter(value > 0.5) %>%
        select(g, t, j, s)
      
      if (nrow(active_vars) == 0) {
        message("Warning: Previous solution has no active variables")
        next
      }
      
      # Add an integer cut constraint
      # Sum of all previous 1's + sum of all previous 0's must be less than total
      model <- model %>%
        add_constraint(
          sum_expr(A[active_vars$g[i], active_vars$t[i], active_vars$j[i], active_vars$s[i]],
                  i = 1:nrow(active_vars)) <= nrow(active_vars) - 1
        )
    }
    
    # Add a constraint to enforce minimum score
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
      message(paste("Could not find solution", sol_idx, "- stopping at", sol_idx-1, "solutions"))
      break
    }
    
    # Get the objective value
    obj_val <- tryCatch({
      objective_value(result)
    }, error = function(e) {
      message("Error getting objective value: ", e$message)
      next
    })
    
    message(paste("Solution", sol_idx, "objective value:", obj_val))
    
    # Store the result
    solver_results[[sol_idx]] <- result
    objective_values[sol_idx] <- obj_val
    
    # Extract the solution variables
    solution_A <- get_solution(result, A[g, t, j, s])
    solutions[[sol_idx]] <- solution_A
    
    # Process the solution assignments
    solution_assignments <- process_solution(solution_A, survey_data, topics, valid_subteams, 
                                           group_size, obj_val, sol_idx)
    
    # Store the assignments
    assignments_list[[sol_idx]] <- solution_assignments
  }
  
  # Remove any NULL entries
  valid_indices <- which(!sapply(assignments_list, is.null))
  
  if (length(valid_indices) == 0) {
    stop("No valid solutions were found. Please check your data and parameters.")
  }
  
  assignments_list <- assignments_list[valid_indices]
  solver_results <- solver_results[valid_indices]
  objective_values <- objective_values[valid_indices]
  
  # Calculate score differences from best solution
  best_score <- max(objective_values)
  score_diffs <- best_score - objective_values
  
  message("Found", length(valid_indices), "valid solutions out of", k_solutions, "requested")
  message("Best solution score:", best_score)
  
  # Return results
  list(
    solver_results = solver_results,
    assignments_list = assignments_list,
    objective_values = objective_values,
    score_diffs = score_diffs,
    best_solution_index = which.max(objective_values)
  )
}

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
                            group_size, objective_value, solution_number) {
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
  
  # Print a summary
  message(paste("Solution", solution_number, "assigned", nrow(final_output), "students to", 
                length(unique(final_output$project_team)), "project teams"))
  
  # Return the assignments
  return(final_output)
}
