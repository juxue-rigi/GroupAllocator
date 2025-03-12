#--------------------------------------------------------------
# Read Project Parameters from Project Set-Up Page
#--------------------------------------------------------------
read_project_data <- function() {
  user_inputs_path <- file.path("shiny_app", "user_inputs.csv")
  if (!file.exists(user_inputs_path)) {
    stop("user_inputs.csv not found in shiny_app folder.")
  }
  user_df <- read.csv(user_inputs_path, stringsAsFactors = FALSE)
  
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
  read.csv(student_data_path, stringsAsFactors = FALSE)
}

#--------------------------------------------------------------
# Compute Group Sizes from Survey Data
#--------------------------------------------------------------
get_group_size <- function(df) {
  # Identify columns that start with "Student_ID"
  group_id_cols <- grep("^Student_ID", names(df), value = TRUE)
  
  # Compute group size: count non-NA entries per row
  df$group_size <- rowSums(!is.na(df[group_id_cols]))
  
  # Remove rows with zero group size
  df <- df[df$group_size != 0, ]
  
  return(df$group_size)
}

#--------------------------------------------------------------
# Extract Unique Topic Names from Survey Data
#--------------------------------------------------------------
get_topics <- function(df) {
  topic_cols <- grep("Choice \\(Topic\\)", names(df), value = TRUE)
  topics <- unique(trimws(unlist(df[, topic_cols])))
  topics[topics != ""]
}

#--------------------------------------------------------------
# Extract Unique Subteam Names from Survey Data
#--------------------------------------------------------------
get_subteams <- function(df) {
  subteam_cols <- grep("Choice \\(Sub-Team\\)", names(df), value = TRUE)
  subteams <- unique(trimws(unlist(df[, subteam_cols])))
  subteams[subteams != ""]
}

#--------------------------------------------------------------
# Build a Preference Array (Dummy Implementation)
#--------------------------------------------------------------
build_pref_array <- function(df, topics, valid_subteams) {
  n_groups <- nrow(df)
  array(0, 
        dim = c(n_groups, length(topics), length(valid_subteams)),
        dimnames = list(
          group   = 1:n_groups,
          topic   = topics,
          subteam = valid_subteams
        ))
}

#--------------------------------------------------------------
# Consolidated Function to Read All Data for the Optimization Model
#--------------------------------------------------------------
read_data <- function(student_data_path) {
  # 1) Read project parameters
  project_params <- read_project_data()
  
  # 2) Read survey data (uploaded CSV)
  survey_data <- process_survey_data(student_data_path)
  
  # 3) Process survey data: compute group sizes, extract topics and subteams
  group_size     <- get_group_size(survey_data)
  topics         <- get_topics(survey_data)
  valid_subteams <- get_subteams(survey_data)
  
  # 4) Build a (dummy) preference array (update logic as needed)
  pref_array <- build_pref_array(survey_data, topics, valid_subteams)
  
  # 5) Return a consolidated list of all parameters for the model
  list(
    c_team        = project_params$c_team,
    b_subteam     = project_params$b_subteam,
    x_topic_teams = project_params$x_topic_teams,
    n_groups      = nrow(survey_data),
    n_topics      = length(topics),
    n_subteams    = length(valid_subteams),
    group_size    = group_size,
    topics        = topics,
    valid_subteams = valid_subteams,
    pref_array    = pref_array,
    survey_data   = survey_data
  )
}