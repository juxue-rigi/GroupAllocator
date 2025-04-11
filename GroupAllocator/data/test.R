calculate_obj_score <- function(assignments_path, 
                               b_subteam = 4, 
                               p_penalty = 0.5, 
                               topic_bonus = 50) {
  
  # Read assignments data
  cat("Reading assignment data from:", assignments_path, "\n")
  assignments <- read.csv(assignments_path, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Display the actual column names to help debug
  cat("Actual column names in CSV:", paste(names(assignments), collapse = ", "), "\n")
  
  # Check for expected column patterns
  user_id_col <- grep("[Uu]ser.*[Ii][Dd]", names(assignments))
  project_col <- grep("[Pp]roject", names(assignments))
  subgroup_col <- grep("[Ss]ub.*group|[Ss]ub-group", names(assignments))
  
  if (length(user_id_col) == 0 || length(project_col) == 0 || length(subgroup_col) == 0) {
    cat("Warning: Could not automatically identify expected columns.\n")
    cat("Please choose column indices manually:\n")
    cat(paste0(1:length(names(assignments)), ": ", names(assignments), "\n"))
    
    # Default to first three columns if can't find by pattern
    user_id_col <- 1
    project_col <- 2
    subgroup_col <- 3
  } else {
    # Use the first match if multiple matches are found
    user_id_col <- user_id_col[1]
    project_col <- project_col[1]
    subgroup_col <- subgroup_col[1]
  }
  
  cat("Using columns:\n")
  cat("User ID column:", names(assignments)[user_id_col], "\n")
  cat("Project column:", names(assignments)[project_col], "\n")
  cat("Subgroup column:", names(assignments)[subgroup_col], "\n")
  
  # Create consistent column names for processing
  assignments$user_id <- assignments[[user_id_col]]
  assignments$project_group <- assignments[[project_col]]
  assignments$sub_group <- assignments[[subgroup_col]]
  
  # Print summary of assignments data
  cat("Total students assigned:", nrow(assignments), "\n")
  
  # Extract topics from project-group format (assuming format like "01-topic")
  # First check if the format matches our expectation
  sample_project <- assignments$project_group[1]
  cat("Sample project group:", sample_project, "\n")
  
  topic_pattern <- "^\\d+-(.*)"
  if (grepl(topic_pattern, sample_project)) {
    assignments$topic <- sub(topic_pattern, "\\1", assignments$project_group)
    assignments$team_num <- as.integer(sub("-.*$", "", assignments$project_group))
  } else {
    # If format is different, just use the whole value as topic
    cat("Project group format is different than expected. Using whole value as topic.\n")
    assignments$topic <- assignments$project_group
    assignments$team_num <- 1:length(unique(assignments$project_group))
  }
  
  # Count unique topics, teams, and subteams
  topics <- unique(assignments$topic)
  cat("Topics:", paste(topics, collapse = ", "), "\n")
  
  # Calculate components of objective function
  
  # 1. Topic coverage bonus: 50 * number of topics
  topic_coverage_bonus <- topic_bonus * length(topics)
  cat("Topic coverage bonus:", topic_coverage_bonus, "\n")
  
  # 2. Subteam usage and empty slots
  subteam_counts <- aggregate(list(count = assignments$user_id), 
                             by = list(project = assignments$project_group, 
                                      subgroup = assignments$sub_group), 
                             FUN = length)
  
  total_subteams <- nrow(subteam_counts)
  cat("Total subteams activated:", total_subteams, "\n")
  
  # Calculate empty slots
  subteam_counts$empty_slots <- b_subteam - subteam_counts$count
  
  total_empty_slots <- sum(subteam_counts$empty_slots[subteam_counts$empty_slots > 0])
  cat("Total empty slots:", total_empty_slots, "\n")
  
  empty_slots_penalty <- p_penalty * total_empty_slots
  cat("Empty slots penalty:", empty_slots_penalty, "\n")
  
  # 3. Preference scores (not available)
  pref_score <- 0
  cat("No preference data provided. Preference scores not included in calculation.\n")
  
  # Calculate total score (excluding preferences)
  total_score <- topic_coverage_bonus - empty_slots_penalty + pref_score
  
  # Return the results
  cat("\nFinal score calculation:\n")
  cat("Topic coverage bonus:", topic_coverage_bonus, "\n")
  cat("Empty slots penalty:", empty_slots_penalty, "\n")
  cat("Preference score:", pref_score, "\n")
  cat("TOTAL OBJECTIVE VALUE:", total_score, "\n")
  
  return(list(
    total_score = total_score,
    topic_coverage_bonus = topic_coverage_bonus,
    empty_slots_penalty = empty_slots_penalty,
    preference_score = pref_score,
    topics = topics,
    total_students = nrow(assignments),
    total_subteams = total_subteams,
    total_empty_slots = total_empty_slots
  ))
}

# Run the calculation with full path
result <- calculate_obj_score("/Users/juxue/Desktop/fyp/GroupAllocator/data/anonymized_data_2120_result.csv")
print(result$total_score)