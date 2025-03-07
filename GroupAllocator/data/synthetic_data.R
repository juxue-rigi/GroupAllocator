
# sample data for current code:
# list(Name = c("stu1", "stu2", "stu3", "stu4"), StudentID = c("e123456", "e123457", "e123458", "e123450")),"a | b | c | d","backend | frontend"

set.seed(123) 

num_responses <- 20
sample_responses <- vector("list", length = num_responses)

for (i in seq_len(num_responses)) {
  # Randomly pick a group size from 1..4
  group_size <- sample(1:4, 1)
  
  # Generate unique student names: e.g. "stu1_1" ..."stu1_4"
  member_names <- paste0("stu", i, "_", seq_len(group_size))
  
  # Generate random 6-digit IDs, e.g., "e328467"
  member_ids <- paste0("e", sample(100000:999999, group_size))
  
  # Randomize projects (4 unique letters: a,b,c,d)
  project_order <- sample(c("a", "b", "c", "d"), 4, replace = FALSE)
  projects_str  <- paste(project_order, collapse = " | ")
  
  # Randomize roles (2 unique: backend, frontend)
  role_order <- sample(c("backend", "frontend"), 2, replace = FALSE)
  roles_str  <- paste(role_order, collapse = " | ")
  
  # Build each survey response
  sample_responses[[i]] <- list(
    Name       = member_names,
    StudentID  = member_ids,
    Projects   = projects_str,
    Roles      = roles_str
  )
}

# Flatten each response into one row for CSV
responses_df <- data.frame(
  Name      = sapply(sample_responses, function(resp) paste(resp$Name, collapse = ", ")),
  StudentID = sapply(sample_responses, function(resp) paste(resp$StudentID, collapse = ", ")),
  Projects  = sapply(sample_responses, function(resp) resp$Projects),
  Roles     = sapply(sample_responses, function(resp) resp$Roles),
  stringsAsFactors = FALSE
)

# View the flattened data
print(responses_df)

# Finally, write to CSV
write.csv(responses_df, "sample_survey_responses.csv", row.names = FALSE)
