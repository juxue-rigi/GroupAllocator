#--------------------------------------------------------------
# Read Project Parameters from Project Set-Up Page
#--------------------------------------------------------------
read_project_data <- function() {
  # We assume user_inputs.csv is in the same folder as this script or your app
  user_inputs_path <- "user_inputs.csv"
  if (!file.exists(user_inputs_path)) {
    stop("user_inputs.csv not found. Please ensure the file is in the correct directory.")
  }
  
  user_df <- read.csv(user_inputs_path, 
                      stringsAsFactors = FALSE, 
                      header = FALSE)# No header in the file

  # Manually assign the column names
  colnames(user_df) <- c("c_team", "b_subteam", "x_topic_teams")

  # Use the most recent row of user inputs
  latest <- tail(user_df, 1)
  
  list(
    c_team        = latest$c_team,
    b_subteam     = latest$b_subteam,
    x_topic_teams = latest$x_topic_teams
  )
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
  group_id_cols <- grep("Student_ID", names(df), value = TRUE)
  
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
get_topics <- function(df, df_pref) {
  # Get unique topics from the preference dataframe
  topics <- df_pref %>%
    filter(!is.na(topic_value)) %>%
    distinct(topic_value) %>%
    pull(topic_value)
  
  message("Found topics: ", paste(topics, collapse = ", "))
  return(topics)
}

#--------------------------------------------------------------
# Extract Unique Subteam Names from Survey Data
#--------------------------------------------------------------
get_subteams <- function(df, df_pref) {
  # Get valid subteams from the preference dataframe
  valid_subteams <- df_pref %>%
    filter(tolower(subteam_value) != "no preference") %>%
    distinct(subteam_value) %>%
    pull(subteam_value)
  
  message("Found valid subteams: ", paste(valid_subteams, collapse = ", "))
  return(valid_subteams)
}

#--------------------------------------------------------------
# Extract topic preferences from survey data
#--------------------------------------------------------------
extract_topics_from_survey <- function(survey_data) {
  # Find topic columns
  topic_cols <- grep("Topic", names(survey_data), value = TRUE)
  
  if (length(topic_cols) == 0) {
    stop("No columns containing 'Topic' found in the survey data.")
  }
  
  message("Topic columns found: ", paste(topic_cols, collapse = ", "))
  
  # Create a dataframe with topic preferences
  df_topics <- survey_data %>%
    mutate(row_id = row_number()) %>%
    select(row_id, all_of(topic_cols)) %>%
    pivot_longer(
      cols = topic_cols,
      names_to = "topic_col",
      values_to = "topic_value"
    ) %>%
    filter(!is.na(topic_value) & topic_value != "")
  
  # Extract rank from column names
  df_topics <- df_topics %>%
    mutate(
      topic_rank = case_when(
        grepl("First", topic_col) ~ 1,
        grepl("Second", topic_col) ~ 2,
        grepl("Third", topic_col) ~ 3,
        grepl("Fourth", topic_col) ~ 4,
        TRUE ~ NA_integer_
      )
    )
  
  message("Extracted topic preferences for ", length(unique(df_topics$row_id)), " groups")
  return(df_topics)
}

#--------------------------------------------------------------
# Extract subteam preferences from survey data
#--------------------------------------------------------------
extract_subteams_from_survey <- function(survey_data) {
  # Find subteam columns
  subteam_cols <- grep("Subteam", names(survey_data), value = TRUE)
  
  if (length(subteam_cols) == 0) {
    stop("No subteam columns found in the survey data.")
  }
  
  message("Subteam columns found: ", paste(subteam_cols, collapse = ", "))
  
  # Create a dataframe with subteam preferences
  df_subteams <- survey_data %>%
    mutate(row_id = row_number()) %>%
    select(row_id, all_of(subteam_cols)) %>%
    pivot_longer(
      cols = subteam_cols,
      names_to = "subteam_col",
      values_to = "subteam_value"
    ) %>%
    filter(!is.na(subteam_value) & subteam_value != "")
  
  # Extract rank from column names
  df_subteams <- df_subteams %>%
    mutate(
      subteam_rank = case_when(
        grepl("First", subteam_col) ~ 1,
        grepl("Second", subteam_col) ~ 2,
        grepl("Third", subteam_col) ~ 3,
        grepl("Fourth", subteam_col) ~ 4,
        TRUE ~ NA_integer_
      )
    )
  
  message("Extracted subteam preferences for ", length(unique(df_subteams$row_id)), " groups")
  return(df_subteams)
}

#--------------------------------------------------------------
# Build a Preference Array
#--------------------------------------------------------------
build_pref_array <- function(df_pref, topics, valid_subteams, n_groups) {
  # Required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' needed for this function. Please install it.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' needed for this function. Please install it.")
  }
  
  # Load packages
  library(dplyr)
  library(tidyr)
  
  message("Building preference array with: ", n_groups, " groups, ", 
          length(topics), " topics, ", length(valid_subteams), " subteams")
  
  # Assign preference scores based on rank
  df_pref <- df_pref %>%
    mutate(
      no_pref = tolower(subteam_value) == "no preference",
      # Initial scoring: valid selections get a score based on rank
      preference_score = case_when(
        no_pref ~ 0,
        pref_rank == 1 ~ 100,
        pref_rank == 2 ~ 80,
        pref_rank == 3 ~ 60,
        pref_rank == 4 ~ 40,
        TRUE ~ 0
      )
    )
  
  # Expand "No preference" entries to one entry per valid subteam
  df_pref_expanded <- df_pref %>%
    mutate(subteam_value = if_else(no_pref, NA_character_, subteam_value)) %>%
    rowwise() %>%
    mutate(subteam_value = list(if (is.na(subteam_value)) valid_subteams else subteam_value)) %>%
    ungroup() %>%
    unnest(subteam_value) %>%
    # For rows originally with "No preference", use a different scoring scale
    mutate(
      preference_score = if_else(no_pref,
                             case_when(
                               pref_rank == 1 ~ 100,
                               pref_rank == 2 ~ 60,
                               pref_rank == 3 ~ 40,
                               TRUE ~ 20
                             ),
                             preference_score)
    ) %>%
    select(-no_pref)
  
  # Print some diagnostic info
  message("Original preference rows: ", nrow(df_pref))
  message("Expanded preference rows: ", nrow(df_pref_expanded))
  
  # Build the pref_array: a 3D array of dimensions (groups x topics x subteams)
  pref_array <- array(0,
                    dim = c(n_groups, length(topics), length(valid_subteams)),
                    dimnames = list(
                      group   = 1:n_groups,
                      topic   = topics,
                      subteam = valid_subteams
                    ))
  
  # Populate the pref_array using the scores from df_pref_expanded
  for (i in seq_len(nrow(df_pref_expanded))) {
    g <- df_pref_expanded$row_id[i]
    t_val <- df_pref_expanded$topic_value[i]
    s_val <- df_pref_expanded$subteam_value[i]
    score <- df_pref_expanded$preference_score[i]
    
    # Find index of topic and subteam in our arrays
    t <- match(t_val, topics)
    s <- match(s_val, valid_subteams)
    
    # Only assign if all indices are valid
    if (!is.na(g) && !is.na(t) && !is.na(s) && g <= n_groups) {
      pref_array[g, t, s] <- score
      
      # For debugging, log a few assignments
      if (i <= 5 || i > nrow(df_pref_expanded) - 5) {
        message("Assigning score ", score, " to group ", g, 
                ", topic '", t_val, "' (", t, "), subteam '", s_val, "' (", s, ")")
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
read_data <- function(student_data_path) {
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
  df_topics <- extract_topics_from_survey(survey_data)
  df_subteams <- extract_subteams_from_survey(survey_data)
  
  # 5) Join to create preference dataframe
  df_pref <- df_topics %>%
    inner_join(df_subteams, by = c("row_id", "topic_rank" = "subteam_rank")) %>%
    rename(pref_rank = topic_rank) %>%
    select(row_id, pref_rank, topic_value, subteam_value)
  
  # 6) Extract unique topics and valid subteams from this dataframe
  topics <- df_pref %>%
    filter(!is.na(topic_value)) %>%
    distinct(topic_value) %>%
    pull(topic_value)
  
  valid_subteams <- df_pref %>%
    filter(tolower(subteam_value) != "no preference") %>%
    distinct(subteam_value) %>%
    pull(subteam_value)
  
  # Update n_groups after filtering
  n_groups <- nrow(survey_data)
  
  # Safety checks
  if (length(topics) == 0) {
    stop("No topics found in the preference data.")
  }
  if (length(valid_subteams) == 0) {
    stop("No valid subteams found in the preference data.")
  }
  if (n_groups == 0) {
    stop("No valid groups found (all have zero group_size?).")
  }
  
  # Report key parameters for debugging
  message("Number of groups: ", n_groups)
  message("Group sizes: ", paste(group_size, collapse = ", "))
  message("Topics found: ", paste(topics, collapse = ", "))
  message("Valid subteams found: ", paste(valid_subteams, collapse = ", "))
  
  # 7) Build preference array
  pref_array <- build_pref_array(df_pref, topics, valid_subteams, n_groups)
  
  # 8) Verify dimensions of pref_array
  if (dim(pref_array)[1] != n_groups || 
      dim(pref_array)[2] != length(topics) || 
      dim(pref_array)[3] != length(valid_subteams)) {
    stop("Dimension mismatch in preference array. Check your data.")
  }
  
  # 9) Return a consolidated list of all parameters for the model
  list(
    c_team         = project_params$c_team,
    b_subteam      = project_params$b_subteam,
    x_topic_teams  = project_params$x_topic_teams,
    n_groups       = n_groups,
    n_topics       = length(topics),
    n_subteams     = length(valid_subteams),
    group_size     = group_size,
    topics         = topics,
    valid_subteams = valid_subteams,
    pref_array     = pref_array,
    survey_data    = survey_data
  )
}