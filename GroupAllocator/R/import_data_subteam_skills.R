#--------------------------------------------------------------
# Read Project Parameters from Project Set-Up Page
#--------------------------------------------------------------
#' Read project configuration data from user inputs file
#'
#' Reads configuration parameters from user_inputs.csv and returns them as a list.
#' Uses the most recent entry in the file for the current configuration.
#'
#' @param file_path Path to the user inputs CSV file (default: "user_inputs.csv")
#' @return A list containing project configuration parameters
#' @export
read_project_data <- function(file_path = "user_inputs.csv") {
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop(paste0(file_path, " not found. Please ensure the file is in the correct directory."))
  }
  
  # Read the CSV file
  user_df <- tryCatch({
    read.csv(file_path, 
             stringsAsFactors = FALSE, 
             header = FALSE,  # No header in the file
             na.strings = c("", "NA", "N/A"))  # Handle various NA representations
  }, error = function(e) {
    stop(paste0("Error reading ", file_path, ": ", e$message))
  })

  # Check that we have at least one row
  if (nrow(user_df) == 0) {
    stop(paste0(file_path, " is empty. Please add configuration data."))
  }
  
  # Define expected columns and their data types
  expected_columns <- c("c_team", "b_subteam", "x_topic_teams", "skills_weight")
  
  # Check if we have enough columns
  if (ncol(user_df) < length(expected_columns)) {
    warning(paste0("Input file has fewer columns than expected. Some parameters will use default values."))
  }
  
  # Assign column names (up to the number of columns we have)
  colnames(user_df)[1:min(ncol(user_df), length(expected_columns))] <- 
    expected_columns[1:min(ncol(user_df), length(expected_columns))]
  
  # Use the most recent row of user inputs
  latest <- tail(user_df, 1)
  
  # Create the result list with defaults for missing values
  result <- list(
    c_team = ifelse(!is.null(latest$c_team) && !is.na(latest$c_team), latest$c_team, 4),
    b_subteam = ifelse(!is.null(latest$b_subteam) && !is.na(latest$b_subteam), latest$b_subteam, 2),
    x_topic_teams = ifelse(!is.null(latest$x_topic_teams) && !is.na(latest$x_topic_teams), latest$x_topic_teams, 2),
    skills_weight = ifelse(!is.null(latest$skills_weight) && !is.na(latest$skills_weight), latest$skills_weight, 10)
  )
  
  # Perform validation on the values
  if (result$c_team <= 0) warning("Team size (c_team) should be positive")
  if (result$b_subteam <= 0) warning("Subteam size (b_subteam) should be positive")
  if (result$x_topic_teams <= 0) warning("Max teams per topic (x_topic_teams) should be positive")
  if (result$skills_weight < 0) warning("Skills weight should be non-negative")
  
  # Print info about loaded configuration
  message("Project configuration loaded:")
  message(sprintf("  Team size: %d", result$c_team))
  message(sprintf("  Subteam size: %d", result$b_subteam))
  message(sprintf("  Max teams per topic: %d", result$x_topic_teams))
  message(sprintf("  Skills weight: %d", result$skills_weight))
  
  # Return the complete parameter list
  return(result)
}

#--------------------------------------------------------------
# Read and Process CSV Survey File Uploaded By The User
#--------------------------------------------------------------
process_survey_data <- function(student_data_path) {
  if (!file.exists(student_data_path)) {
    stop("Uploaded student survey CSV not found at: ", student_data_path)
  }
  
  df <- read.csv(student_data_path, 
                 stringsAsFactors = FALSE, 
                 na.strings = c("", "NA"))  # Ensure empty strings are NA
  
  # Log column names for debugging
  message("Actual column names in dataframe:")
  message(paste(names(df), collapse = ", "))
  
  return(df)
}

#--------------------------------------------------------------
# Compute Group Sizes from Survey Data
#--------------------------------------------------------------
get_group_size <- function(df) {
  # Identify columns that contain "Student_ID"
  group_id_cols <- grep("^Student_ID", names(df), value = TRUE)
  
  if (length(group_id_cols) == 0) {
    stop("No columns containing 'Student_ID' found in the survey data.")
  }
  
  # Compute group size: count non-NA entries per row in these columns
  group_sizes <- rowSums(!is.na(df[group_id_cols]))
  
  # Log the calculated group sizes
  message("Group sizes calculated: ", paste(group_sizes, collapse = ", "))
  
  return(group_sizes)
}

#--------------------------------------------------------------
# Extract Number of Valid Rows (Groups)
#--------------------------------------------------------------
get_n_groups <- function(df) {
  return(nrow(df))
}

#--------------------------------------------------------------
# Extract Unique Topic Names from Survey Data
#--------------------------------------------------------------
get_topics <- function(df) {
  # Find column containing topic preferences
  pref_col <- grep("Rank.*Topic.*Preference", names(df), value = TRUE, ignore.case = TRUE)
  
  if (length(pref_col) == 0) {
    stop("No column containing 'Rank Topic Preference' found in the survey data.")
  }
  
  # Extract all preference strings
  all_prefs <- unlist(strsplit(as.character(df[[pref_col]]), ";"))
  all_prefs <- trimws(all_prefs)  # Remove any leading/trailing whitespace
  
  # Split each preference by underscore and take the first part (topic)
  topic_parts <- sapply(strsplit(all_prefs, "_"), function(x) if(length(x) > 0) trimws(x[1]) else NA)
  
  # Filter out NA values and get unique topics
  topics <- unique(topic_parts[!is.na(topic_parts)])
  
  # Filter to only include strings containing "Topic"
  topics <- grep("Topic", topics, value = TRUE)
  
  if (length(topics) == 0) {
    stop("No topics found in preference data. Check format.")
  }
  
  message("Found topics: ", paste(topics, collapse = ", "))
  return(topics)
}

#--------------------------------------------------------------
# Extract Unique Subteam Names from Survey Data
#--------------------------------------------------------------
get_subteams <- function(df) {
  # Find column containing topic preferences
  pref_col <- grep("Rank.*Topic.*Preference", names(df), value = TRUE, ignore.case = TRUE)
  
  if (length(pref_col) == 0) {
    stop("No column containing 'Rank Topic Preference' found in the survey data.")
  }
  
  # Extract all preference strings
  all_prefs <- unlist(strsplit(as.character(df[[pref_col]]), ";"))
  all_prefs <- trimws(all_prefs)  # Remove any leading/trailing whitespace
  
  # Split each preference by underscore and take the second part (subteam)
  subteam_parts <- sapply(strsplit(all_prefs, "_"), function(x) if(length(x) > 1) trimws(x[2]) else NA)
  
  # Filter out NA values
  subteam_parts <- subteam_parts[!is.na(subteam_parts)]
  
  # Extract just the subteam identifier - more flexible approach
  # This will extract single letter or number subteam identifiers regardless of format
  subteams <- unique(gsub(".*([A-Za-z0-9])$", "\\1", subteam_parts))
  
  # If the above didn't work, try to extract the last character from each subteam part
  if (length(subteams) == 0 || all(nchar(subteams) > 1)) {
    message("Trying alternative subteam extraction method...")
    subteams <- unique(sapply(subteam_parts, function(x) {
      # Extract the last non-space character as the subteam identifier
      trimmed <- trimws(x)
      if (nchar(trimmed) > 0) {
        return(substr(trimmed, nchar(trimmed), nchar(trimmed)))
      } else {
        return(NA)
      }
    }))
    subteams <- subteams[!is.na(subteams)]
  }
  
  if (length(subteams) == 0) {
    stop("No subteams found in preference data. Check format.")
  }
  
  message("Found subteams: ", paste(subteams, collapse = ", "))
  return(subteams)
}


#--------------------------------------------------------------
# Extract topic and subteam preferences from survey data
#--------------------------------------------------------------

extract_preferences_from_survey <- function(survey_data) {
  # Find preference column
  pref_col <- grep("Rank.*Topic.*Preference", names(survey_data), value = TRUE, ignore.case = TRUE)
  
  if (length(pref_col) == 0) {
    stop("No column containing 'Rank Topic Preference' found in the survey data.")
  }
  
  message("Preference column found: ", paste(pref_col, collapse = ", "))
  
  # Debug: Print sample of raw preferences
  message("DEBUG: Sample of raw preferences:")
  print(head(survey_data[[pref_col]]))
  
  # Create a dataframe with preferences, preserving original row indices
  df_prefs <- survey_data %>%
    # Create row_id based on the actual row index in the survey
    mutate(row_id = row_number()) %>%
    # Select only the row_id and preference column
    select(row_id, all_of(pref_col)) %>%
    # Filter out rows with no preference data
    filter(!is.na(get(pref_col)) & get(pref_col) != "") %>%
    # Debug
    {
      message("DEBUG - After initial selection, filtered to ", nrow(.), " rows with preferences")
      .
    } %>%
    # Split the preference string and create multiple rows
    rowwise() %>%
    mutate(prefs_list = list(strsplit(get(pref_col), ";")[[1]])) %>%
    unnest(prefs_list) %>%
    # Important: group_by row_id to restart pref_rank for each response
    group_by(row_id) %>%
    # Clean up whitespace
    mutate(
      prefs_list = trimws(prefs_list),
      # Extract rank from position in the list - now properly grouped by original row
      pref_rank = row_number()
    ) %>%
    ungroup()
  
  # Debug - check the structure after splitting
  message("DEBUG - After splitting preferences by row:")
  print(df_prefs %>% group_by(row_id) %>% summarize(
    num_prefs = n(),
    .groups = "drop"
  ))
  
  # Create a more flexible extraction of topic and subteam values
  df_prefs <- df_prefs %>%
    mutate(
      # Split by underscore to separate topic and subteam
      parts = strsplit(prefs_list, "_"),
      # The first part is the topic
      topic_value = sapply(parts, function(p) if(length(p) > 0) trimws(p[1]) else NA),
      # The second part is the subteam
      subteam_part = sapply(parts, function(p) if(length(p) > 1) trimws(p[2]) else NA),
      # Extract just the subteam letter - now with a more flexible pattern
      # This will extract any single letter/number at the end
      subteam_value = gsub(".*([A-Za-z0-9])\\s*$", "\\1", subteam_part)
    ) %>%
    select(row_id, pref_rank, topic_value, subteam_value)
  
  # Debug: Show the extracted topic and subteam values
  message("DEBUG: Sample of extracted topic and subteam values:")
  print(head(df_prefs, 10))
  
  # Check for parsing errors
  parse_errors <- df_prefs %>% 
    filter(is.na(topic_value) | topic_value == "" | is.na(subteam_value) | subteam_value == "")
  
  if (nrow(parse_errors) > 0) {
    warning("Some preferences could not be parsed correctly. Check the format of your preference data.")
    message("Example of unparsed preference: ", 
            ifelse(nrow(parse_errors) > 0, parse_errors$prefs_list[1], "None"))
    
    # More detailed reporting on parse errors
    message("DEBUG: Sample of parse errors:")
    print(head(parse_errors, 5))
  }
  
  # Fill in "No Preference" for any empty subteam values
  df_prefs <- df_prefs %>%
    mutate(
      subteam_value = ifelse(is.na(subteam_value) | subteam_value == "", 
                           "No Preference", 
                           subteam_value)
    )
  
  # Debug output
  message("Extracted preferences for ", length(unique(df_prefs$row_id)), " groups")
  message("Number of preferences per group:")
  print(df_prefs %>% group_by(row_id) %>% summarize(
    num_prefs = n(),
    .groups = "drop"
  ))
  
  return(df_prefs)
}
#---------------------------------------------------------------------------
# Extract Skill Data
#---------------------------------------------------------------------------
extract_skills_data <- function(survey_data) {
  # Find all skill columns
  skill_cols <- grep("Skill.*Rating.*for.*Student_ID", names(survey_data), value = TRUE)
  
  if (length(skill_cols) == 0) {
    message("No skill rating columns found in the survey data.")
    return(list(skills = character(0), skills_data = list()))
  }
  
  message("Found ", length(skill_cols), " skill rating columns")
  
  # Find all student ID columns - these determine the maximum team size
  student_id_cols <- grep("^Student_ID", names(survey_data), value = TRUE)
  max_team_size <- length(student_id_cols)
  
  message("Found ", max_team_size, " student ID columns, indicating maximum team size")
  
  # Extract unique skills from column names
  skill_pattern <- "\\.([^.]+)$" # Pattern to extract skill name after last period
  skills <- unique(sapply(skill_cols, function(col) {
    matches <- regmatches(col, regexec(skill_pattern, col))
    if (length(matches[[1]]) > 1) {
      return(matches[[1]][2])  # Return the captured group
    } else {
      return(NA)
    }
  }))
  skills <- skills[!is.na(skills)]
  
  message("Skills found: ", paste(skills, collapse = ", "))
  
  # Calculate expected number of skill columns (positions × skills)
  expected_skill_cols <- max_team_size * length(skills)
  if (length(skill_cols) != expected_skill_cols) {
    warning(sprintf("Potential data format issue: Found %d skill columns but expected %d (%d positions × %d skills)",
                   length(skill_cols), expected_skill_cols, max_team_size, length(skills)))
  }
  
  # Create a mapping of skills and students
  skills_data <- list()
  
  # For each row in survey data (each group)
  for (g in 1:nrow(survey_data)) {
    # Get student IDs in this group
    student_ids <- as.character(unlist(survey_data[g, student_id_cols]))
    student_ids <- student_ids[!is.na(student_ids) & student_ids != ""]
    
    message("Group ", g, " has ", length(student_ids), " students: ", paste(student_ids, collapse = ", "))
    
    # Detect potential data issues
    if (length(student_ids) == 0) {
      warning(sprintf("Row %d has no valid student IDs", g))
    }
    
    # For each student position
    for (i in 1:length(student_ids)) {
      student_id <- student_ids[i]
      
      # Skip if empty
      if (is.na(student_id) || student_id == "") {
        warning(sprintf("Empty student ID in position %d of group %d", i, g))
        next
      }
      
      message("Processing student ", student_id, " (position ", i, ")")
      
      # Initialize skills for this student
      skills_data[[student_id]] <- list()
      
      # For each skill
      for (skill in skills) {
        # Two-step approach for robustness:
        # 1. Find columns for this position
        position_pattern <- paste0("Student_ID.*", i)
        position_cols <- grep(position_pattern, skill_cols, value = TRUE)
        
        # 2. Find the column for this skill among the position columns
        # Escape dots in skill name for regex pattern
        skill_escaped <- gsub("\\.", "\\\\.", skill)
        skill_end_pattern <- paste0(skill_escaped, "$")
        skill_col <- grep(skill_end_pattern, position_cols, value = TRUE)
        
        # Debug output (only for first student and first skill)
        if (g == 1 && i == 1 && skill == skills[1]) {
          message("DEBUG: Position pattern: ", position_pattern)
          message("DEBUG: Position columns found: ", paste(position_cols, collapse=", "))
          message("DEBUG: Skill end pattern: ", skill_end_pattern)
          message("DEBUG: Final skill column match: ", paste(skill_col, collapse=", "))
        }
        
        if (length(skill_col) > 0) {
          # Get skill value
          skill_value <- survey_data[g, skill_col[1]]  # Use first match if multiple
          skill_value <- as.numeric(as.character(skill_value))
          
          # Only store valid values
          if (!is.na(skill_value)) {
            skills_data[[student_id]][[skill]] <- skill_value
            message("Added skill ", skill, " = ", skill_value, " for student ", student_id)
          } else {
            warning(sprintf("Invalid skill value for student %s, skill %s", student_id, skill))
          }
        } else {
          warning(sprintf("Missing skill column for position %d, skill %s", i, skill))
        }
      }
      
      # Check if student has all expected skills
      if (length(names(skills_data[[student_id]])) < length(skills)) {
        warning(sprintf("Student %s is missing some skill ratings (%d/%d found)",
                       student_id, length(names(skills_data[[student_id]])), length(skills)))
      }
    }
  }
  
  message("Processed skills for ", length(names(skills_data)), " students")
  
  # Final data validation
  if (length(names(skills_data)) == 0) {
    warning("No student skills data was successfully extracted. Check your data format.")
  }
  
  return(list(
    skills = skills,
    skills_data = skills_data
  ))
}

#--------------------------------------------------------------
# Build a Preference Array
#--------------------------------------------------------------
build_pref_array <- function(df_prefs, topics, subteams, n_groups) {
  library(dplyr)
  library(tidyr)
  
  # Debug output - check inputs
  message("DEBUG: df_prefs has ", nrow(df_prefs), " rows")
  message("DEBUG: First few rows of df_prefs:")
  if (nrow(df_prefs) > 0) {
    print(head(df_prefs, 3))
  } else {
    message("DEBUG: df_prefs is empty!")
  }
  
  # Debug unique topics and subteams in the data vs parameters
  message("DEBUG: Topics in parameter: ", paste(topics, collapse=", "))
  message("DEBUG: Unique topic values in df_prefs: ", paste(unique(df_prefs$topic_value), collapse=", "))
  message("DEBUG: Subteams in parameter: ", paste(subteams, collapse=", "))
  message("DEBUG: Unique subteam values in df_prefs: ", paste(unique(df_prefs$subteam_value), collapse=", "))
  
  # NEW: Standardize topic and subteam values to match expected format
  # This is a key change to ensure format compatibility
  df_prefs <- df_prefs %>%
    mutate(
      # Ensure topics match the expected format
      # If your CSV has "Topic1" but array expects "Topic 1", use:
      topic_value = gsub("Topic(\\d+)", "Topic \\1", topic_value),
      
      # Ensure subteams are uppercase single letters
      subteam_value = toupper(subteam_value)
    )
  
  # Show the standardized values
  message("DEBUG: After standardization:")
  message("DEBUG: Unique standardized topic values: ", paste(unique(df_prefs$topic_value), collapse=", "))
  message("DEBUG: Unique standardized subteam values: ", paste(unique(df_prefs$subteam_value), collapse=", "))
  
  # Verify that the standardized values match the expected array dimensions
  topic_match <- all(unique(df_prefs$topic_value) %in% topics)
  subteam_match <- all(unique(df_prefs$subteam_value) %in% c(subteams, "No Preference"))
  
  if (!topic_match) {
    warning("Some topic values in preferences don't match expected topics. Check format.")
    missing_topics <- setdiff(unique(df_prefs$topic_value), topics)
    message("Topics in preferences not in expected list: ", paste(missing_topics, collapse=", "))
  }
  
  if (!subteam_match) {
    warning("Some subteam values in preferences don't match expected subteams. Check format.")
    missing_subteams <- setdiff(unique(df_prefs$subteam_value), c(subteams, "No Preference"))
    message("Subteams in preferences not in expected list: ", paste(missing_subteams, collapse=", "))
  }
  
  message("Building preference array with: ", n_groups, " groups, ", 
          length(topics), " topics, ", length(subteams), " subteams")
  
  # Assign preference scores based on rank
  df_prefs <- df_prefs %>%
    mutate(
      # Initial scoring: valid selections get a score based on rank
      preference_score = case_when(
        pref_rank == 1 ~ 100,  # First choice
        pref_rank == 2 ~ 80,   # Second choice
        pref_rank == 3 ~ 60,   # Third choice
        pref_rank == 4 ~ 50,   # Fourth choice
        pref_rank == 5 ~ 40,   # Fifth choice
        pref_rank <= 10 ~ 30,  # Later choices still have some value
        TRUE ~ 20              # Default value for very low preferences
      )
    )
  
  # Build the pref_array
  pref_array <- array(0,
                    dim = c(n_groups, length(topics), length(subteams)),
                    dimnames = list(
                      group   = 1:n_groups,
                      topic   = topics,
                      subteam = subteams
                    ))
  
  # Populate the pref_array using the scores from df_prefs
  valid_assignments <- 0
  invalid_assignments <- 0
  
  for (i in seq_len(nrow(df_prefs))) {
    g <- df_prefs$row_id[i]
    t_val <- df_prefs$topic_value[i]
    s_val <- df_prefs$subteam_value[i]
    score <- df_prefs$preference_score[i]
    
    # Skip "No Preference" subteam values
    if (s_val == "No Preference") {
      message("Skipping 'No Preference' subteam for group ", g, ", topic '", t_val, "'")
      next
    }
    
    # Find index of topic and subteam in our arrays
    t <- match(t_val, topics)
    s <- match(s_val, subteams)
    
    # Debug the matching
    if (i <= 10 || (i > nrow(df_prefs) - 5)) {
      message("DEBUG: Row ", i, ": group=", g, 
              ", topic='", t_val, "' (match=", ifelse(is.na(t), "NA", t), ")", 
              ", subteam='", s_val, "' (match=", ifelse(is.na(s), "NA", s), ")")
    }
    
    # Only assign if all indices are valid
    if (!is.na(g) && !is.na(t) && !is.na(s) && g <= n_groups) {
      pref_array[g, t, s] <- score
      valid_assignments <- valid_assignments + 1
      
      # Log a few assignments
      if (i <= 10 || i > nrow(df_prefs) - 5) {
        message("Assigning score ", score, " to group ", g, 
                ", topic '", t_val, "' (", t, "), subteam '", s_val, "' (", s, ")")
      }
    } else {
      invalid_assignments <- invalid_assignments + 1
      message("Invalid assignment skipped: group=", g, 
              ", topic='", t_val, "' (", t, 
              "), subteam='", s_val, "' (", s, ")")
    }
  }
  
  message("DEBUG: Valid assignments: ", valid_assignments, ", Invalid: ", invalid_assignments)
  
  # If many invalid assignments, alert the user
  if (invalid_assignments > nrow(df_prefs) / 2) {
    warning("More than half of preference assignments were invalid. Check topic/subteam formats.")
  }
  
  # NEW: Only add fallback scores if a group truly has no preferences
  # This is a key change to avoid overriding actual preferences
  for (g in 1:n_groups) {
    # Count non-zero scores for this group
    nonzero_count <- sum(pref_array[g, , ] > 0)
    
    # Only add default scores if the group has no preferences at all
    if (nonzero_count == 0) {
      message("Group ", g, " has no valid preferences. Adding default scores.")
      
      # For each topic-subteam combination
      for (t in 1:length(topics)) {
        for (s in 1:length(subteams)) {
          # Add a minimal default score
          pref_array[g, t, s] <- 10
        }
      }
    }
  }
  
  # Report total score as a sanity check
  message("Total preference score in array: ", sum(pref_array))
  
  return(pref_array)
}

#--------------------------------------------------------------
# Consolidated Function to Read All Data for the Optimization Model
#--------------------------------------------------------------
read_data_subteam_skills <- function(student_data_path) {
  # Check if required packages are installed
  required_packages <- c("dplyr", "tidyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' needed for this function. Please install it.")
    }
  }
  
  message("Reading data from: ", student_data_path)
  
  # 1) Read project parameters
  project_params <- read_project_data()
  
  # 2) Read survey data (uploaded CSV)
  survey_data <- process_survey_data(student_data_path)
  
  # 3) Calculate group sizes and filter out rows with zero size
  group_size <- get_group_size(survey_data)
  
  # Filter out rows with zero group size
  valid_rows <- group_size > 0
  if (sum(valid_rows) == 0) {
    stop("No valid groups found (all have zero group_size?). Please ensure Student_ID columns are populated.")
  }

  survey_data <- survey_data[valid_rows, ]
  group_size <- group_size[valid_rows]
  
  # 4) Process survey data: extract topic and subteam preferences
  df_pref <- extract_preferences_from_survey(survey_data)
  
  # 5) Extract unique topics and valid subteams directly from preference data
  topics <- unique(df_pref$topic_value)
  subteams <- unique(df_pref$subteam_value)
  
  # Remove any NA or empty values
  topics <- topics[!is.na(topics) & topics != ""]
  subteams <- subteams[!is.na(subteams) & subteams != "" & 
                                 tolower(subteams) != "no preference"]
  
  # Update n_groups after filtering
  n_groups <- nrow(survey_data)
  
  # Safety checks
  if (length(topics) == 0) {
    stop("No topics found in the preference data.")
  }
  if (length(subteams) == 0) {
    stop("No valid subteams found in the preference data.")
  }
  if (n_groups == 0) {
    stop("No valid groups found (all have zero group_size?).")
  }
  
  # Report key parameters for debugging
  message("Number of groups: ", n_groups)
  message("Group sizes: ", paste(group_size, collapse = ", "))
  message("Topics found: ", paste(topics, collapse = ", "))
  message("Valid subteams found: ", paste(subteams, collapse = ", "))
  
  # 6) Extract skills data from the survey
  skills_info <- extract_skills_data(survey_data)
  skills <- skills_info$skills
  skills_data <- skills_info$skills_data
  
  # Report skills found
  message("Skills found: ", paste(skills, collapse = ", "))
  message("Number of students with skill data: ", length(names(skills_data)))
  
  # 7) Build preference array
  pref_array <- build_pref_array(df_pref, topics, subteams, n_groups)
  
  # 8) Verify dimensions of pref_array
  if (dim(pref_array)[1] != n_groups || 
      dim(pref_array)[2] != length(topics) || 
      dim(pref_array)[3] != length(subteams)) {
    stop("Dimension mismatch in preference array. Check your data.")
  }
  
  # 9) Return a consolidated list of all parameters for the model
  list(
    # Project parameters
    c_team         = project_params$c_team,
    b_subteam      = project_params$b_subteam,
    x_topic_teams  = project_params$x_topic_teams,
    skills_weight  = project_params$skills_weight,
    
    # Data dimensions
    n_groups       = n_groups,
    n_topics       = length(topics),
    n_subteams     = length(subteams),
    group_size     = group_size,
    
    # Preference data
    topics         = topics,
    subteams       = subteams,
    pref_array     = pref_array,
    
    # Skills data
    skills         = skills,
    skills_data    = skills_data,
    
    # Original data
    survey_data    = survey_data
  )
}