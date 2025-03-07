# loading packages
#install.packages("slam", repos = "https://cloud.r-project.org", type = "source")
#remotes::install_github("dirkschumacher/ompr")
#remotes::install_github("dirkschumacher/ompr.roi")

library(slam)               # test that slam loads
library(ompr)               # or library(ompr, lib.loc=.libPaths()[1])
library(ompr.roi)
library(ROI.plugin.glpk)

#-------------------------------------------
# Read the CSV data
#-----------------------------------------------
# since interaction with front-end is still in progress, we will hard code some inputs first

# read data from student survey response csv file
survey_data <- read.csv("../data/sample_survey_responses.csv", stringsAsFactors = FALSE)

# sample data for current code:
# list(Name = c("stu1", "stu2", "stu3", "stu4"), StudentID = c("e123456", "e123457", "e123458", "e123450")),"a | b | c | d","backend | frontend"

# We'll define:
#   G groups (one per row),
#   T = 4 projects (a, b, c, d),
#   R = 2 roles (backend, frontend).

T_names <- c("a", "b", "c", "d")
R_names <- c("backend", "frontend")
T <- length(T_names)  # 4
R <- length(R_names)  # 2
G <- nrow(df)         # 20

# Helper: parse a "projects" string like "b | a | d | c" into numeric preferences, e.g.:
# first in the string => highest preference (4), second => 3, third => 2, fourth => 1.
# We'll store them in a numeric vector P_proj[g, t_index].
parse_project_preferences <- function(pref_string, project_labels = T_names) {
  # Split on " | "
  items <- unlist(strsplit(pref_string, " *\\| *"))
  # items might look like c("b", "a", "d", "c")
  
  # We want to assign highest score to the first item in items
  # E.g. if there are 4 items, we do scores: 4,3,2,1
  n <- length(project_labels)  # 4
  scores <- numeric(n)
  # Example logic: if items = c("b","a","d","c"), we do:
  #   "b" -> 4, "a" -> 3, "d" -> 2, "c" -> 1
  # ignoring any duplicates or missing
  for (i in seq_along(items)) {
    which_t <- match(items[i], project_labels)
    if (!is.na(which_t)) {
      scores[which_t] <- n - (i - 1)
    }
  }
  return(scores)
}

# Similarly, parse a "roles" string like "frontend | backend" => highest = 2, second = 1
parse_role_preferences <- function(pref_string, role_labels = R_names) {
  items <- unlist(strsplit(pref_string, " *\\| *"))
  n <- length(role_labels)  # 2
  scores <- numeric(n)
  for (i in seq_along(items)) {
    which_r <- match(items[i], role_labels)
    if (!is.na(which_r)) {
      scores[which_r] <- n - (i - 1)
    }
  }
  return(scores)
}

# We'll build:
#   group_size[g]
#   P_proj[g, t]
#   P_role[g, r]
# Then we'll combine them into P[g, t, r] = alpha*P_proj[g,t] + beta*P_role[g,r]

# For a real scenario, you might also store the actual members (Name/StudentID),
# but for the MIP, we primarily need the group size and preference scores.

group_size <- integer(G)
P_proj <- matrix(0, nrow = G, ncol = T)
P_role <- matrix(0, nrow = G, ncol = R)

for (g in 1:G) {
  # The group might have multiple names/IDs separated by commas
  # e.g. "stu1_1, stu1_2, stu1_3"
  # We'll count them to get the group size
  name_vec <- unlist(strsplit(df$Name[g], " *, *"))
  group_size[g] <- length(name_vec)
  
  # parse Projects
  P_proj[g, ] <- parse_project_preferences(df$Projects[g], T_names)
  
  # parse Roles
  P_role[g, ] <- parse_role_preferences(df$Roles[g], R_names)
}

# Combine them into a single 3D array if we want: P[g,t,r]
alpha <- 1.0
beta  <- 1.0
P <- array(0, dim = c(G, T, R))  # P[g,t,r]
for (g in 1:G) {
  for (t in 1:T) {
    for (r in 1:R) {
      P[g, t, r] <- alpha * P_proj[g, t] + beta * P_role[g, r]
    }
  }
}

#-------------------------------------------
# 3) Build a Simple MIP to Assign Groups to (Project, Role)
#-------------------------------------------
# Suppose we have minimum/maximum capacity constraints on each project
# For demonstration, let's say each project must have at least 1 group member
# and at most 10 members total.
# You can adjust these or read them from an external source.
L_t <- c(1, 1, 1, 1)   # min 1 person in each project
U_t <- c(10,10,10,10)  # max 10 people in each project

# Decision variables: x[g,t,r] in {0,1}.
# Objective: maximize sum(P[g,t,r] * x[g,t,r]).
# Constraints:
#   1) Each group g is assigned exactly once => sum_{t,r} x[g,t,r] = 1
#   2) Project capacity: for each t, sum_{g,r} (group_size[g]* x[g,t,r]) in [L_t, U_t]
#   3) x[g,t,r] binary


model <- MIPModel() %>%
  # 3.1 Decision variables
  add_variable(x[g, t, r], g = 1:G, t = 1:T, r = 1:R, type = "binary") %>%
  
  # 3.2 Objective
  set_objective(
    sum_expr(P[g, t, r] * x[g, t, r], g = 1:G, t = 1:T, r = 1:R),
    sense = "max"
  ) %>%
  
  # 3.3 Constraints
  # (A) Each group assigned exactly once
  add_constraint(
    sum_expr(x[g, t, r], t = 1:T, r = 1:R) == 1,
    g = 1:G
  ) %>%
  
  # (B) Project capacity
  add_constraint(
    sum_expr(group_size[g] * x[g, t, r], g = 1:G, r = 1:R) >= L_t[t],
    t = 1:T
  ) %>%
  add_constraint(
    sum_expr(group_size[g] * x[g, t, r], g = 1:G, r = 1:R) <= U_t[t],
    t = 1:T
  )

# Solve model with GLPK (feel free to switch to another solver)
solution <- solve_model(model, with_ROI("glpk"))
cat("Solver status:", solution$status, "\n")

#-------------------------------------------
# 4) Extract & Interpret the Solution
#-------------------------------------------
assignments <- get_solution(solution, x[g, t, r])

# Keep only rows where x[g,t,r] = 1
final_assignments <- subset(assignments, value > 0.5)

cat("Optimal assignment:\n")
for (i in seq_len(nrow(final_assignments))) {
  g_ <- final_assignments$g[i]
  t_ <- final_assignments$t[i]
  r_ <- final_assignments$r[i]
  # Identify the group members, the project name, and role name
  group_members <- unlist(strsplit(df$Name[g_], " *, *"))
  project_name  <- T_names[t_]
  role_name     <- R_names[r_]
  
  cat(sprintf(
    " Group %d (size=%d: %s) => Project: %s, Role: %s\n",
    g_, group_size[g_], paste(group_members, collapse=" / "),
    project_name, role_name
  ))
}

# Finally, print objective value
obj_value <- solution$objval
cat("\nObjective (Total Preference) =", obj_value, "\n")