# loading packages
library(slam)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)


# Read in Data 
"
Read in:
1. a set of groups or individuals:  G = {1, 2, …, n_groups}.
2. a set of topics: T = {1, 2, …, m_topics}.
3. a preference score matrix pref[g, t], where a higher score means group g prefers topic t more
4. each group's size, group_size[g].
5. the ideal number of students per project group, b.
6. a penalty constant p we apply if a project is underfilled by 1 student.
"

# Sample Data
n_groups <- 5
m_topics <- 3
b <- 6
p <- 50

# each group has a certain number of members
group_size <- c(3,2,1,2,3) # size of each group g

# each group has a preference score for each topic, higher = more preferred
pref <- matrix(
  c(80, 60, 20,
    70, 90, 10,
    50, 20, 80,
    30, 40, 70,
    90, 10, 50),
  nrow = n_groups, ncol = m_topics, byrow = TRUE
)



# Create the MIP model
model <- MIPModel() %>%

  # Decision Variables:
  # A[g,t] = 1 if group g is assigned to topic t, else 0
  add_variable(A[g,t], g = 1:n_groups, t = 1:m_topics, type = "binary") %>%

  # y[t]= 1 if topic t is underfilled by 1 student, else 0
  add_variable(y[t], t = 1:m_topics, type = 'binary') %>%



  # Objective Function:
  # Maximizing sum of preference minus penalty
  set_objective(
    sum_expr(pref[g,t] * A[g,t], g = 1: n_groups, t = 1:m_topics) 
    - 
    p * sum_expr(y[t], t = 1:m_topics),
    sense = 'max'
  ) %>%

  # Constraints:
  # 1) each group is assigned exactly once:
  add_constraint(
    sum_expr(A[g,t], t = 1:m_topics) == 1,
    g = 1:n_groups
  ) %>%

  # 2) enforce that the sum of group sizes for a topic = b - y[t]
  # i.e. either b or (b-1) if y[t]=1
  add_constraint(
    sum_expr(group_size[g] * A[g,t], g = 1:n_groups) == (b - y[t]),
    t = 1:m_topics
  )

# solve the model using GLPK
result <- solve_model(
  model,
  with_ROI(solver = "glpk", verbose = TRUE)
)

cat("Solver status:", result$status, "\n")


# extract assignment decisions:
solution_A <- get_solution(result, A[g,t])
assigned <- solution_A[solution_A$value > 0.5, ]
solution_y <- get_solution(result, y[t])

