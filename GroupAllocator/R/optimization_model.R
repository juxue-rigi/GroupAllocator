#' Run the Optimization
#'
#' Reads user inputs and survey data, builds and solves the MIP model,
#' and processes the solution into final assignments.
#'
#' @param student_data_path Path to the CSV file containing student survey data.
#' @return A list with elements: solver_result (the raw optimization result) and
#'         assignments (a data frame of student assignments).
#' @export
run_optimization <- function(student_data_path = "survey_data.csv") {
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

  # 3) constants for the model
  p_penalty   <- 2
  minCapacity <- c_team - 2
  
  # 4) the MIP model
  model <- MIPModel() %>%
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
    set_objective(
      sum_expr(pref_array[g, t, s] * A[g, t, j, s],
               g = 1:n_groups, t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams) -
      p_penalty * sum_expr(slack[t, j, s],
                           t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams),
      sense = "max"
    ) %>%
    # (1) Each group is assigned exactly once
    add_constraint(
      sum_expr(A[g, t, j, s],
               t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams) == 1,
      g = 1:n_groups
    ) %>%
    # (2) If team (t,j) is formed, then at least one group is assigned
    add_constraint(
      sum_expr(A[g, t, j, s], g = 1:n_groups) >= Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    ) %>%
    # (3) Sub-team capacity (plus slack equals b_subteam)
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups) + slack[t, j, s] == b_subteam,
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    ) %>%
    # (4) Tie assignments to team activation
    add_constraint(
      sum_expr(A[g, t, j, s], g = 1:n_groups, s = 1:n_subteams) <= n_groups * Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams
    ) %>%
    # (5) Minimum team capacity if a team is active
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups, s = 1:n_subteams) >= minCapacity * Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams
    ) %>%
    # (6) At least one team per topic
    add_constraint(
      sum_expr(Z[t, j], j = 1:x_topic_teams) >= 1,
      t = 1:n_topics
    ) %>%
    # (7) Minimum threshold for each sub-team
    add_constraint(
      sum_expr(group_size[g] * A[g, t, j, s], g = 1:n_groups) >= (b_subteam - 1) * Z[t, j],
      t = 1:n_topics, j = 1:x_topic_teams, s = 1:n_subteams
    )
  
  # 5) Solve the model (using GLPK with a time limit)
  control <- list(tm_limit = 300)  # 5 minute time limit
  
  result <- solve_model(
    model,
    with_ROI(
      solver = "glpk",
      verbose = TRUE,
      control = control
    )
  )
  
  # 6) Process the solution:
  # 6a) Extract the solution for decision variable A[g,t,j,s]
  solution_A <- get_solution(result, A[g, t, j, s])
  
  # 6b) Filter to keep only assignments (A==1)
  assigned <- subset(solution_A, value > 0.5)
  
  # 6c) Map indices to labels using topics and valid_subteams
  assigned$topic_name   <- topics[assigned$t]
  assigned$subteam_name <- valid_subteams[assigned$s]
  assigned$project_team <- paste0(assigned$topic_name, "_team", assigned$j)
  
  # 6d) Retrieve student IDs for each group from survey data
  group_id_cols <- grep("Student_ID", names(survey_data), value = TRUE)
  
  final_output_list <- lapply(seq_len(nrow(assigned)), function(k) {
    # Extract the assigned row
    g_val <- assigned$g[k]
    project_team_val <- assigned$project_team[k]
    subteam_val      <- assigned$subteam_name[k]
    
    # Get all student IDs in that group (row g_val)
    row_ids <- survey_data[g_val, group_id_cols, drop = FALSE]
    # Filter out NA
    student_ids <- unlist(row_ids[!is.na(row_ids)])
    
    # Build a small data frame: one row per student
    if (length(student_ids) == 0) {
      return(NULL)  # no valid IDs
    }
    data.frame(
      student_id    = student_ids,
      project_team  = project_team_val,
      subteam       = subteam_val,
      stringsAsFactors = FALSE
    )
  })
  
  # Combine them all
  final_output <- do.call(rbind, final_output_list)
  # Remove duplicates in case they occur
  final_output <- unique(final_output)
  # Sort by project team
  final_output <- final_output[order(final_output$project_team), ]
  
  # Return results
  list(
    solver_result = result,
    assignments   = final_output
  )
}