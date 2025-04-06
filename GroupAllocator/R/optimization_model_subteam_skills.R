#===========================================================================
# TABLE OF CONTENTS
#===========================================================================
# 1. OPTIMIZATION FUNCTIONS
#    - run_multi_solution_optimization_subteam_skills: Main optimization workflow
#
# 2. HELPER FUNCTIONS
#    - process_solution: Convert solution variables to assignments
#    - display_solution_summary: Display a summary of the solution
#    - format_solution_for_export: Format solutions for export
#===========================================================================

#===========================================================================
# 1. OPTIMIZATION FUNCTIONS
#===========================================================================

#' Run the Optimization and Find Top-K Solutions
#'
#' Reads user inputs and survey data, builds and solves the MIP model to find 
#' the top-k solutions by score, and processes the solution into final assignments.
#'
#' @param student_data_path Path to the CSV file containing student survey data.
#' @param k_solutions Number of top solutions to find (default: 3)
#' @param score_gap_percent Maximum percentage below optimal score for solutions (default: 5.0)
#' @return A list with solver results and assignment data frames.
#' @export
run_multi_solution_optimization_subteam_skills <- function(student_data_path = "survey_data.csv", 
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
  data_list <- read_data_subteam_skills(student_data_path)
  
  # 2) Extract parameters from the returned list
  c_team         <- data_list$c_team
  b_subteam      <- data_list$b_subteam
  x_topic_teams  <- data_list$x_topic_teams
  skills_weight  <- data_list$skills_weight
  n_topics       <- data_list$n_topics
  n_subteams     <- data_list$n_subteams
  n_groups       <- data_list$n_groups
  group_size     <- data_list$group_size
  topics         <- data_list$topics
  subteams       <- data_list$subteams
  pref_array     <- data_list$pref_array
  survey_data    <- data_list$survey_data
  skills         <- data_list$skills
  skills_data    <- data_list$skills_data

  # 3) Constants for the model
  p_penalty   <- 0.5  # Penalty for empty slots
  minCapacity <- 1    # Minimum capacity
  skill_weight <- 0.1 # Weight for skill scores in objective function
  
  # Print diagnostic information
  message("Checking preference array dimensions...")
  message(sprintf("pref_array dimensions: %d x %d x %d", 
               dim(pref_array)[1], 
               dim(pref_array)[2], 
               dim(pref_array)[3]))
  message(sprintf("n_groups=%d, n_topics=%d, n_subteams=%d",
               n_groups, n_topics, n_subteams))
  
  # Additional debugging for skills data
  if (length(skills) > 0) {
    # Debug output - examine structure of first student
    if (length(names(skills_data)) > 0) {
      first_student <- names(skills_data)[1]
      message("First student skills: ", paste(names(skills_data[[first_student]]), collapse=", "))
      message("Example skill value: ", skills_data[[first_student]][[1]])
    }
    
    # Print sample skill scores
    for (k in 1:min(3, length(skills))) {
      message(sprintf("Checking skill scores for skill %s...", skills[k]))
      sample_students <- head(names(skills_data), 3)
      for (sid in sample_students) {
        if (!is.null(skills_data[[sid]][[skills[k]]])) {
          message(sprintf("  Student %s, skill %s = %f", 
                       sid, skills[k], 
                       skills_data[[sid]][[skills[k]]]))
        } else {
          message(sprintf("  Student %s is missing skill %s data", 
                       sid, skills[k]))
        }
      }
    }
  }
  
  # Print model parameters
  message("Model parameters:")
  message(sprintf("Team size: %d, Subteam size: %d, Max teams per topic: %d", c_team, b_subteam, x_topic_teams))
  message(sprintf("Skills matching weight: %f", skill_weight))
  message(sprintf("Number of groups: %d, Number of topics: %d, Number of subteams: %d", n_groups, n_topics, n_subteams))
  
  # Calculate total students to help diagnose capacity issues
  total_students <- sum(group_size)
  max_possible_teams <- floor(total_students / minCapacity)
  message(sprintf("Total student count: %d, Minimum capacity: %d, Maximum possible teams: %d", 
                 total_students, minCapacity, max_possible_teams))
  
  # Print topic and subteam counts to verify data
  message("Topics (", n_topics, "): ", paste(topics, collapse = ", "))
  message("Subteams (", n_subteams, "): ", paste(subteams, collapse = ", "))
  
  # Print skills information
  message("Skills (", length(skills), "): ", paste(skills, collapse = ", "))
  message("Students with skill data: ", length(names(skills_data)))
  
  #---------------------------------------------------------------------------
  # Initialize solution storage
  #---------------------------------------------------------------------------
  solution_storage <- list(
    all_solver_results = list(),
    all_solutions = list(),
    all_objective_values = c(),
    all_assignments = list()
  )

  #---------------------------------------------------------------------------
  # Building Skill Matrices
  #---------------------------------------------------------------------------
  if (length(skills) > 0) {
    # Pre-calculate the skill score contribution of each group
    group_skill_matrices <- list()
    
    for (k in 1:length(skills)) {
      group_skill_matrices[[k]] <- numeric(n_groups)
      
      for (g in 1:n_groups) {
        # Get student IDs in this group
        student_ids <- survey_data[g, grep("Student_ID", names(survey_data), value = TRUE)]
        student_ids <- student_ids[!is.na(student_ids) & student_ids != ""]
        
        # Sum skill scores for this group
        group_score <- 0
        for (sid in student_ids) {
            if (!is.null(skills_data[[sid]])) {
                if (k <= length(skills) && !is.null(skills_data[[sid]][[skills[k]]])) {
                   group_score <- group_score + skills_data[[sid]][[skills[k]]]
                } else if (k <= length(skills)) {
                  message(sprintf("Skill %s not found for student %s", skills[k], sid))
                }
            }
        }
        
        group_skill_matrices[[k]][g] <- group_score
      }
    }
  }
  
  
  #---------------------------------------------------------------------------
  # Base Model Definition - Modified to use soft skill constraints
  #---------------------------------------------------------------------------
  
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
    add_variable(skill_score[t, j, k],
                t = 1:n_topics,
                j = 1:x_topic_teams,
                k = 1:length(skills),
                lb = 0,
                type = "continuous") %>%

    # Objective Function with SOFT skill constraints
    set_objective(
        # Main preference score
        sum_expr(pref_array[g, t, s] * A[g, t, j, s],
                            g = 1:n_groups, t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams) +

        # Topic coverage bonus
        50 * sum_expr(Z[t, 1], t = 1:n_topics) -

        # Penalty for empty slots
        p_penalty * sum_expr(slack[t, j, s],
                          t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams) +
                          
        # Skill score reward - direct incentive instead of thresholds
        skill_weight * sum_expr(skill_score[t, j, k],
                t = 1:n_topics, j = 1:x_topic_teams, k = 1:length(skills)),
        
        sense = 'max'
    ) %>%

    # Constraints

    # (1) Each group is assigned exactly once
    add_constraint(
      sum_expr(A[g, t, j, s],
              t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams) == 1,
      g = 1:n_groups
    ) %>%
    
    # (2) If team (t,j) is formed, at least one group is assigned to each subteam s
    add_constraint(
      sum_expr(A[g, t, j, s], g = 1:n_groups) >= Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    ) %>%
    
    # (3) Sub-team capacity constraint - FIXED with proper indexing
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups) <= b_subteam,
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    ) %>%
    
    # (4) Add slack to represent empty spots in activated subteams
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups) + slack[t, j, s] == b_subteam * Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    ) %>%

    # (5) Force slack to be 0 for non-activated teams
    add_constraint(
      slack[t, j, s] <= b_subteam * Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    ) %>%
    
    # (6) Tie assignments to team activation
    add_constraint(
      sum_expr(A[g, t, j, s], g = 1:n_groups, s = 1:n_subteams) <= n_groups * Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams
    ) %>%
    
    # (7) Relaxed minimum team capacity
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups, s = 1:n_subteams) >= 1 * Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams
    ) %>%
    
    # (8) Relaxed subteam threshold
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups) >= Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    )

  # (9) Prioritize filling j=1 before activating j=2 for the same topic
  for (t in 1:n_topics) {
    for (j in 2:x_topic_teams) {
      base_model <- base_model %>% 
        add_constraint(
          Z[t, j] <= Z[t, j-1],
          t = t, j = j
        )
    }
  }

  # (10) Simplified Skill Constraints - Only define what the skill score is, 
  # no thresholds or binary variables
  if (length(skills) > 0) {
    for (t in 1:n_topics) {
      for (j in 1:x_topic_teams) {
        for (k in 1:length(skills)) {
          base_model <- base_model %>%
            add_constraint(
              skill_score[t, j, k] == sum_expr(group_skill_matrices[[k]][g] * A[g, t, j, s],
                                          g = 1:n_groups, s = 1:n_subteams),
              t = t, j = j, k = k
            )
        }
      }
    }
  }

  #---------------------------------------------------------------------------
  # Find Optimal Solution with increased time limit
  #---------------------------------------------------------------------------
  message("Finding optimal solution...")
  
  result <- tryCatch({
    solve_model(
      base_model,
      with_ROI(
        solver = "glpk",
        verbose = TRUE,
        control = list(
          tm_limit = 3600,  # Increased to 60 minutes
          mip_gap = 0.05,   # Accept solutions within 5% of optimal
          presolve = TRUE   # Enable presolving
        )
      )
    )
  }, error = function(e) {
    message("Solver error: ", e$message)
    stop(e$message)  # Re-throw the error
  })
  
  # Check if optimal solution was found
  if (!result$status %in% c("optimal", "relaxed", "integer optimal", "success", "time limit exceeded")) {
    stop("Could not find any feasible solution. Please check your data and constraints.")
  }
  
  # Get the solution values
  solution_A <- get_solution(result, A[g, t, j, s])
  obj_val <- objective_value(result)
  
  # Store results
  solution_storage$all_solver_results[[1]] <- result
  solution_storage$all_solutions[[1]] <- solution_A
  solution_storage$all_objective_values[1] <- obj_val
  
  # Process the solution assignments
  solution_assignments <- process_solution(
    solution_A, 
    survey_data, 
    topics, 
    subteams, 
    group_size, 
    obj_val,
    1, 
    pref_array, 
    skills, 
    skills_data, 
    rep(0, length(skills))  # No thresholds needed for processing
  )
  
  solution_storage$all_assignments[[1]] <- solution_assignments
  
  message(paste("Solution", 1, "found with objective value:", obj_val))
  
  # Set minimum acceptable score for alternatives
  optimal_obj_val <- solution_storage$all_objective_values[1]
  min_acceptable_score <- optimal_obj_val * (1 - score_gap_percent/100)
  message("Will accept solutions with scores above: ", min_acceptable_score)
  
  #---------------------------------------------------------------------------
  # Find Additional Solutions
  #---------------------------------------------------------------------------
  max_attempts <- min(k_solutions * 3, 20)  # Cap at 20 to prevent excessive computation
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
      prev_A <- solution_storage$all_solutions[[prev_sol_idx]]
      
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
          sum_expr(
            A[active_vars$g[i], active_vars$t[i], active_vars$j[i], active_vars$s[i]],
            i = 1:nrow(active_vars)
          ) <= nrow(active_vars) - 1
        )
    }
    
    # Add the constraint for minimum acceptable score
    model <- model %>%
      add_constraint(
        sum_expr(pref_array[g, t, s] * A[g, t, j, s],
                g = 1:n_groups, t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams) +
        50 * sum_expr(Z[t, 1], t = 1:n_topics) -
        p_penalty * sum_expr(slack[t, j, s],
                          t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams) +
        skill_weight * sum_expr(skill_score[t, j, k],
                    t = 1:n_topics, j = 1:x_topic_teams, k = 1:length(skills)) >= min_acceptable_score
      )
    
    # Solve the model
    result <- tryCatch({
      solve_model(
        model,
        with_ROI(
          solver = "glpk",
          verbose = TRUE,
          control = list(
            tm_limit = 3600,  # Increased to 60 minutes
            mip_gap = 0.05,   # Accept solutions within 5% of optimal
            presolve = TRUE   # Enable presolving
          )
        )
      )
    }, error = function(e) {
      message("Failed to find additional solution: ", e$message)
      return(NULL)
    })
    
    if (is.null(result)) break
    
    # Store this solution
    found_solutions <- found_solutions + 1
    
    # Get the solution values
    solution_A <- get_solution(result, A[g, t, j, s])
    obj_val <- objective_value(result)
    
    # Store results
    solution_storage$all_solver_results[[found_solutions]] <- result
    solution_storage$all_solutions[[found_solutions]] <- solution_A
    solution_storage$all_objective_values[found_solutions] <- obj_val
    
    # Process the solution assignments
    solution_assignments <- process_solution(
      solution_A, 
      survey_data, 
      topics, 
      subteams, 
      group_size, 
      obj_val,
      found_solutions, 
      pref_array, 
      skills, 
      skills_data, 
      rep(0, length(skills))  # No thresholds needed for processing
    )
    
    solution_storage$all_assignments[[found_solutions]] <- solution_assignments
    
    message(paste("Solution", found_solutions, "found with objective value:", obj_val))
  }
  
  #---------------------------------------------------------------------------
  # Select Top-K Solutions
  #---------------------------------------------------------------------------
  # Remove any NULL entries
  valid_indices <- which(!sapply(solution_storage$all_assignments, is.null))
  
  if (length(valid_indices) == 0) {
    stop("No valid solutions were found. Please check your data and parameters.")
  }
  
  message("Found ", length(valid_indices), " valid solutions.")
  
  # Keep only valid solutions
  filtered_assignments <- solution_storage$all_assignments[valid_indices]
  filtered_objective_values <- solution_storage$all_objective_values[valid_indices]
  filtered_solver_results <- solution_storage$all_solver_results[valid_indices]
  
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
    best_solution_index = which.max(objective_values)
  )
}
  

#===========================================================================
# 2. HELPER FUNCTIONS
#===========================================================================

#' Process a solution into a data frame
#' 
#' @param solution_A The 'A' variable solution from the optimization
#' @param survey_data Original survey data
#' @param topics List of topics
#' @param subteams List of subteams
#' @param group_size Array of group sizes
#' @param obj_value Objective value of the solution
#' @param solution_num Solution number
#' @param pref_array Preference array
#' @param skills List of skills
#' @param skills_data Skills data structure
#' @param skill_thresholds Skill threshold values
#' @return Data frame with assignment details
process_solution <- function(solution_A, survey_data, topics, subteams, group_size, 
                           obj_value, solution_num, pref_array, skills, skills_data, 
                           skill_thresholds) {
  library(dplyr)
  
  # Filter to just the active assignments
  active_assignments <- solution_A %>%
    filter(value > 0.5)
  
  if (nrow(active_assignments) == 0) {
    message("Warning: No active assignments in solution ", solution_num)
    return(NULL)
  }
  
  # Create a data frame with the assignments
  assignment_df <- active_assignments %>%
    mutate(
      topic_name = topics[t],
      subteam_name = subteams[s],
      # This stays the same for team_id, but we need to modify it later for student-level data
      team_id = paste0(topic_name, "_Team_", j, "_", subteam_name),
      solution_number = solution_num,
      group_size = group_size[g],
      preference_score = pref_array[cbind(g, t, s)]
    ) %>%
    select(group = g, topic = topic_name, team_number = j, 
           subteam = subteam_name, team_id, group_size, 
           preference_score, solution_number)
  
  # Add skill information if available
  if (length(skills) > 0) {
    # For each assignment, calculate skill totals
    skill_totals <- data.frame(team_id = character(0))
    
    for (i in 1:nrow(assignment_df)) {
      g <- assignment_df$group[i]
      team_id <- assignment_df$team_id[i]
      
      # Get student IDs for this group
      student_cols <- grep("^Student_ID", names(survey_data), value = TRUE)  # Add ^ to match beginning of string exactly
      student_ids <- as.character(unlist(survey_data[g, student_cols]))
      student_ids <- student_ids[!is.na(student_ids) & student_ids != ""]
      student_ids <- student_ids[grepl("^stu|^S[0-9]", student_ids)]
      
      # Calculate skill totals for this assignment
      skill_values <- rep(0, length(skills))
      names(skill_values) <- skills
      
      for (sid in student_ids) {
        if (!is.null(skills_data[[sid]])) {
          for (k in 1:length(skills)) {
            skill_name <- skills[k]
            if (!is.null(skills_data[[sid]][[skill_name]])) {
              skill_values[k] <- skill_values[k] + skills_data[[sid]][[skill_name]]
            }
          }
        }
      }
      
      # Create a row for this team
      skill_row <- as.data.frame(t(skill_values)) %>%
        mutate(team_id = team_id)
      
      # Add thresholds met flags
      for (k in 1:length(skills)) {
        threshold_met_col <- paste0(skills[k], "_threshold_met")
        skill_row[[threshold_met_col]] <- ifelse(
          skill_values[k] >= skill_thresholds[k], "Yes", "No"
        )
      }
      
      # Bind to the skill totals data frame
      skill_totals <- bind_rows(skill_totals, skill_row)
    }
    
    # Join skill information to the assignment data frame
    if (nrow(skill_totals) > 0) {
      assignment_df <- assignment_df %>%
        left_join(skill_totals, by = "team_id")
    }
  }
  
  # Create student-level assignments with proper project_team formatting
  student_assignments <- data.frame()
  
  for (i in 1:nrow(assignment_df)) {
    g <- assignment_df$group[i]
    
    # Get the team information
    topic <- assignment_df$topic[i]
    team_number <- assignment_df$team_number[i]
    subteam <- assignment_df$subteam[i]
    team_id <- assignment_df$team_id[i]
    
    # Create the project_team field in the expected format
    project_team <- paste0(topic, "_team", team_number)
    
    # Get student IDs for this group
    student_cols <- grep("^Student_ID", names(survey_data), value = TRUE)  # Add ^ to match beginning of string exactly
    student_ids <- as.character(unlist(survey_data[g, student_cols]))
    student_ids <- student_ids[!is.na(student_ids) & student_ids != ""]
    # Filter to only include IDs that start with 'stu' or 'S' followed by digits
    student_ids <- student_ids[grepl("^stu|^S[0-9]", student_ids)]
    
    # Create a row for each student in this group
    for (student_id in student_ids) {
      # Create individual student row
      student_row <- assignment_df[i, ] %>%
        mutate(
          student_id = student_id,
          project_team = project_team,
          individual_score = preference_score, # Individual score is same as group score initially
          group_id = g # Keep track of original group
        )
      
      # Add to the output dataframe
      student_assignments <- bind_rows(student_assignments, student_row)
    }
  }
  
  # If no students were found, create a dummy row with the assignment info
  if (nrow(student_assignments) == 0) {
    message("Warning: No students found in the survey data for solution ", solution_num)
    
    # Create dummy student assignments from the group assignments
    student_assignments <- assignment_df %>%
      mutate(
        student_id = paste0("Student_", row_number()),
        project_team = paste0(topic, "_team", team_number),
        individual_score = preference_score,
        group_id = group
      )
  }
  
  return(student_assignments)
}

#' Display a summary of the assignment solution
#' 
#' @param solution A solution from the optimization model
#' @param show_preferences Whether to show preference information
#' @param show_skills Whether to show skill information
#' @return NULL (prints to console)
display_solution_summary <- function(solution, show_preferences = TRUE, show_skills = TRUE) {
  if (is.null(solution)) {
    message("No valid solution to display")
    return(NULL)
  }
  
  # Group by topic and team
  team_summary <- solution %>%
    group_by(topic, team_number, subteam) %>%
    summarize(
      groups = list(group),
      total_students = sum(group_size),
      avg_preference = mean(preference_score),
      .groups = "drop"
    )
  
  # Print a summary
  message("\n=== Solution Summary ===")
  message(sprintf("Total groups assigned: %d", nrow(solution)))
  message(sprintf("Topics used: %d", length(unique(solution$topic))))
  message(sprintf("Total teams formed: %d", length(unique(solution$team_id))))
  
  # Show team details
  message("\n--- Team Details ---")
  for (i in 1:nrow(team_summary)) {
    message(sprintf("Team: %s_%d_%s", 
                 team_summary$topic[i], 
                 team_summary$team_number[i],
                 team_summary$subteam[i]))
    message(sprintf("  Groups: %s", 
                 paste(unlist(team_summary$groups[i]), collapse = ", ")))
    message(sprintf("  Total students: %d", team_summary$total_students[i]))
    
    if (show_preferences) {
      message(sprintf("  Avg preference score: %.2f", team_summary$avg_preference[i]))
    }
    
    # Add skill information if available
    if (show_skills) {
      skill_cols <- grep("^(?!.*threshold).*$", names(solution), perl = TRUE, value = TRUE)
      skill_cols <- skill_cols[!skill_cols %in% c("group", "topic", "team_number", "subteam", 
                                                 "team_id", "group_size", "preference_score", 
                                                 "solution_number")]
      
      if (length(skill_cols) > 0) {
        team_id <- paste0(team_summary$topic[i], "_Team_", 
                         team_summary$team_number[i], "_", 
                         team_summary$subteam[i])
        team_skills <- solution %>%
          filter(team_id == !!team_id) %>%
          select(all_of(skill_cols))
        
        if (ncol(team_skills) > 0) {
          message("  Skills:")
          for (skill in skill_cols) {
            threshold_col <- paste0(skill, "_threshold_met")
            threshold_met <- if (threshold_col %in% names(solution)) {
              solution %>%
                filter(team_id == !!team_id) %>%
                pull(threshold_col) %>%
                unique()
            } else {
              NA
            }
            
            skill_value <- team_skills %>% pull(skill) %>% unique()
            message(sprintf("    %s: %.1f %s", 
                         skill, 
                         skill_value,
                         ifelse(is.na(threshold_met), "", 
                               ifelse(threshold_met == "Yes", "✓", "✗"))))
          }
        }
      }
    }
    
    message("")
  }
}

#' Format solutions for export
#' 
#' @param solutions Solution list from select_top_solutions
#' @return List of data frames ready for export
format_solution_for_export <- function(solutions) {
  result <- list()
  
  for (i in 1:length(solutions$assignments_list)) {
    solution <- solutions$assignments_list[[i]]
    
    if (is.null(solution)) {
      message("Solution ", i, " is invalid, skipping")
      next
    }
    
    # Create a summary version for export
    export_df <- solution %>%
      select(group, topic, team_number, subteam, team_id, preference_score) %>%
      arrange(topic, team_number, subteam)
    
    # Skill columns
    skill_cols <- grep("^(?!.*threshold).*$", names(solution), perl = TRUE, value = TRUE)
    skill_cols <- skill_cols[!skill_cols %in% c("group", "topic", "team_number", "subteam", 
                                                "team_id", "group_size", "preference_score", 
                                                "solution_number")]
    
    if (length(skill_cols) > 0) {
      # Team level skill scores
      skill_summary <- solution %>%
        group_by(team_id) %>%
        summarize(across(all_of(skill_cols), unique), .groups = "drop")
      
      # Join with export data
      export_df <- export_df %>%
        left_join(skill_summary, by = "team_id")
      
      # Add threshold information
      threshold_cols <- grep("_threshold_met$", names(solution), value = TRUE)
      if (length(threshold_cols) > 0) {
        threshold_summary <- solution %>%
          group_by(team_id) %>%
          summarize(across(all_of(threshold_cols), unique), .groups = "drop")
        
        export_df <- export_df %>%
          left_join(threshold_summary, by = "team_id")
      }
    }
    
    # Add to result list
    result[[i]] <- export_df
  }
  
  return(result)
}