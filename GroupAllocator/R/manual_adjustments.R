#===========================================================================
# TABLE OF CONTENTS
#===========================================================================
# 1. SCORING FUNCTIONS
#    - calculate_assignment_score: Calculate total preference score
#    - calculate_individual_scores: Calculate preference scores for individuals
#
# 2. VALIDATION FUNCTIONS
#    - validate_reallocation: Check if a manual reallocation is valid
#    - check_empty_subteams: Check for empty subteams after reallocation
#===========================================================================


#===========================================================================
# 1. SCORING FUNCTIONS
#===========================================================================

#' Calculate preference score for a given assignment
#'
#' This function calculates the total preference score for a given assignment
#' by matching each student to their preferences in the survey data.
#'
#' @param assignments Data frame of student assignments with student_id, project_team, and subteam
#' @param survey_data The original survey data from which preferences are extracted
#' @param topics List of available topics
#' @param valid_subteams List of valid subteams
#' @param pref_array 3D array of preference scores: [group, topic, subteam]
#' @return Numeric preference score
calculate_assignment_score <- function(assignments, survey_data, topics, valid_subteams, pref_array) {
  # Initialize score
  total_score <- 0
  team_bonus <- 50  # Bonus for each unique team formed
  
  # Process each student assignment
  for (i in 1:nrow(assignments)) {
    student_id <- assignments$student_id[i]
    project_team <- assignments$project_team[i]
    subteam <- assignments$subteam[i]
    
    # Extract topic from project_team (e.g., "TopicA_team1" -> "TopicA")
    topic <- strsplit(project_team, "_team")[[1]][1]
    
    # Find the group in survey data that contains this student_id
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
        total_score <- total_score + student_score
      }
    }
  }
  
  # Add team formation bonus (simplified version)
  unique_teams <- unique(assignments$project_team)
  total_score <- total_score + length(unique_teams) * team_bonus
  
  return(total_score)
}

#' Calculate preference score for individual students
#'
#' This function adds a column with individual preference scores to the assignments data frame.
#'
#' @param assignments Data frame of student assignments with student_id, project_team, and subteam
#' @param survey_data The original survey data from which preferences are extracted
#' @param topics List of available topics
#' @param valid_subteams List of valid subteams
#' @param pref_array 3D array of preference scores: [group, topic, subteam]
#' @return Data frame with individual_score column added
calculate_individual_scores <- function(assignments, survey_data, topics, valid_subteams, pref_array) {
  # Create a column for individual scores
  assignments$individual_score <- 0
  
  # Process each student
  for (i in 1:nrow(assignments)) {
    student_id <- assignments$student_id[i]
    project_team <- assignments$project_team[i]
    subteam <- assignments$subteam[i]
    
    # Extract topic from project_team (e.g., "TopicA_team1" -> "TopicA")
    topic <- strsplit(project_team, "_team")[[1]][1]
    
    # Find the group in survey data that contains this student_id
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
        assignments$individual_score[i] <- student_score
      }
    }
  }
  
  return(assignments)
}

#===========================================================================
# 2. VALIDATION FUNCTIONS
#===========================================================================

#' Validate a manual reallocation
#'
#' This function checks if a proposed reallocation is valid based on capacity constraints
#'
#' @param new_assignments Data frame of new student assignments
#' @param b_subteam Maximum subteam size
#' @return List with elements: valid (logical) and message (character)
validate_reallocation <- function(new_assignments, b_subteam) {
  # Check if any subteam would exceed capacity
  team_subteam_counts <- table(paste(new_assignments$project_team, new_assignments$subteam, sep = "|"))
  
  # Find any overloaded subteams
  overloaded <- team_subteam_counts[team_subteam_counts > b_subteam]
  
  if (length(overloaded) > 0) {
    overloaded_names <- names(overloaded)
    messages <- sapply(1:length(overloaded), function(i) {
      parts <- strsplit(overloaded_names[i], "\\|")[[1]]
      team <- parts[1]
      subteam <- parts[2]
      paste0("- ", team, " (", subteam, "): ", overloaded[i], " students (max ", b_subteam, ")")
    })
    
    return(list(
      valid = FALSE,
      message = paste("The following subteams exceed capacity:", 
                      paste(messages, collapse = "\n"), sep = "\n")
    ))
  }
  
  return(list(valid = TRUE, message = "Valid reallocation"))
}

#' Check for empty subteams 
#'
#' After a reallocation, some subteams may become empty. This function identifies them.
#'
#' @param new_assignments Data frame of new student assignments
#' @param all_teams List of all possible project teams
#' @param all_subteams List of all possible subteams
#' @return List with elements: has_empty (logical) and message (character)
check_empty_subteams <- function(new_assignments, all_teams, all_subteams) {
  # Create all possible team-subteam combinations
  all_combinations <- expand.grid(
    project_team = all_teams,
    subteam = all_subteams,
    stringsAsFactors = FALSE
  )
  
  # Check which combinations exist in the assignments
  existing_combinations <- unique(new_assignments[, c("project_team", "subteam")])
  
  # Find missing combinations (potential empty subteams)
  missing <- anti_join(all_combinations, existing_combinations, by = c("project_team", "subteam"))
  
  if (nrow(missing) > 0) {
    return(list(
      has_empty = TRUE,
      message = paste("Warning: Some subteams are now empty. This may impact team cohesion.")
    ))
  }
  
  return(list(has_empty = FALSE, message = "All subteams have members"))
}