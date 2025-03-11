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
3. each topic has 2 sub-teams, or more generally a set s = 1,....,s
4. each group's size, group_size[g].
5. the ideal number of students per sub-team, b (e.g. 4), with an optional shortfall variable y[t,s]
6. ach group g has a preference for each (t,s) combination, denoted pref[g,t,s].
6. a penalty constant p we apply if a project is underfilled by 1 student.
"

# Sample Data
n_groups <- 6
m_topics <- 3
s_subteams <- 2
b_subteams <- 4
p_penalty <- 50

# each group has a certain number of members
# size of each group 
# number of students per survey response
group_size <- c(2, 1, 3, 2, 4, 1)


# define an array for preferences: pref[g, t, s].
set.seed(123)
pref_array <- array(sample(10:100, n_groups * m_topics * s_subteams, replace = TRUE),
                    dim = c(n_groups, m_topics, s_subteams))
pref_array

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

