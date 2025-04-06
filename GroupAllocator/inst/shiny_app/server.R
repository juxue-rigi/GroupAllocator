#===========================================================================
# TABLE OF CONTENTS
#===========================================================================
# 0. Initial Source Files
#
# 1. INITIALIZATION AND SETUP
#    - Helper Functions
#      - %||% (null-coalescing operator)
#      - get_cached_data()
#      - calculate_preference_score()
#    - Reactive Values Initialization
#
# 2. UI RENDERING
#    - Main UI
#    - Text Outputs
#    - Solution Comparison UI
#    - Data Tables
#
# 3. PAGE NAVIGATION HANDLERS
#    - Navigation Between Pages
#    - UI State Updates
#
# 4. CSV UPLOAD AND DATA PROCESSING
#    - File Management
#
# 5. OPTIMIZATION AND SOLUTION MANAGEMENT
#    - Run Optimization
#    - Solution Navigation
#    - Parameter Changes
#
# 6. MANUAL EDIT MODE
#    - Toggle Edit Mode
#    - Handle User Edits
#    - Score Updates
#===========================================================================


server <- function(input, output, session) {
  #===========================================================================
  # INITIALIZATION AND SETUP
  #===========================================================================
  
  # Define the null-coalescing operator (missing in base R)
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # Load required libraries
  library(DT)  # For interactive tables

  #---------------------------------------------------------------------------
  # Reactive Values for State Management
  #---------------------------------------------------------------------------
  
  # Track which page is displayed
  current_page <- reactiveVal("login")
  
  # Store user information
  user_name <- reactiveVal("")
  course_name <- reactiveVal("")

  # Store the selected model type
  model_type <- reactiveVal("1")  # Default to model 1
  
  # Main reactive values for application state
  params <- reactiveValues(
    # Optimization parameters
    c_team = NULL,              # Team size
    b_subteam = NULL,           # Subteam size
    x_topic_teams = NULL,       # Max teams per topic
    k_solutions = 3,            # Number of solutions to find
    
    # Data tracking
    survey_uploaded = FALSE,    # Track if user has uploaded CSV
    data_cache = NULL,          # Cache for survey data
    
    # Solution tracking
    final_assignments = NULL,   # Store current selected solution
    all_assignments = NULL,     # List of all solutions
    objective_values = NULL,    # Scores for all solutions
    score_diffs = NULL,         # Score differences from best solution
    current_solution_idx = NULL,# Index of current solution
    preference_score = NULL,    # Score of current solution
    previous_score = NULL,      # Previous score for comparison
    error_message = NULL,       # Store any error messages
    
    # Manual edit mode
    edit_mode = FALSE,          # Manual edit mode flag
    edited_assignments = NULL,  # Store edited assignments 
    original_preference_score = NULL, # Original score before edits
    edited_preference_score = NULL,   # Current score after edits
    last_score_calc_time = NULL,      # Timestamp of last score calculation
    pending_change = NULL  # Store pending changes that need confirmation
  )


  # Dynamic source files based on model type
  observeEvent(model_type(), {
    req(model_type())
    model_num <- as.numeric(model_type())
    
    # Clear the global environment of any previous model-specific functions
    rm(list = ls(pattern = "^read_data$|^run_multi_solution_optimization$|^process_survey_data$", envir = .GlobalEnv), envir = .GlobalEnv)
    
    # Source files based on selected model
    if (model_num == 1) {
      # Just source the files without redefining the functions
      source("../../R/import_data.R")
      source("../../R/optimization_model.R")
      
    } else if (model_num == 2) {
      source("../../R/import_data_diversity.R")
      source("../../R/optimization_model_diversity.R")
      
    } else if (model_num == 3) {
      source("../../R/import_data_subteam_skills.R")
      source("../../R/optimization_model_subteam_skills.R")
    }
    
    # Always source common adjustments
    source("../../R/manual_adjustments.R")
  }, ignoreNULL = TRUE)
  #---------------------------------------------------------------------------
  # Helper Functions
  #---------------------------------------------------------------------------
  
  # Create a helper function to get data with caching
  get_cached_data <- function() {
    if (is.null(params$data_cache)) {
      params$data_cache <- read_data("survey_data.csv")
      message("Survey data loaded into cache")
    }
    return(params$data_cache)
  }
  
  # Function to calculate preference score based on assignments
  calculate_preference_score <- function(assignments) {
    req(assignments)
    
    # Get cached survey data instead of reading every time
    data_list <- get_cached_data()
    survey_data <- data_list$survey_data
    pref_array <- data_list$pref_array
    topics <- data_list$topics
    valid_subteams <- data_list$valid_subteams
    
    # Initialize score - IMPORTANT: Start from zero each time
    total_score <- 0
    
    # Add debug output
    message("Calculating preference score for ", nrow(assignments), " assignments")
    
    # Process each student assignment
    for (i in 1:nrow(assignments)) {
      student_id <- assignments$student_id[i]
      project_team <- assignments$project_team[i]
      subteam <- assignments$subteam[i]
      
      # Find the topic from the project_team - add error handling
      topic <- tryCatch({
        parts <- strsplit(project_team, "_team")[[1]]
        if (length(parts) > 0) parts[1] else project_team
      }, error = function(e) {
        warning("Invalid project_team format: ", project_team)
        return(NA)  # Return NA if we can't parse the project_team
      })
      
      # Skip if we couldn't parse the topic
      if (is.na(topic)) next
      
      # Find the group in survey data that contains this student_id
      student_cols <- grep("Student_ID", names(survey_data), value = TRUE)
      group_indices <- apply(survey_data[, student_cols, drop = FALSE], 1, function(row) {
        any(row == student_id, na.rm = TRUE)
      })
      
      if (any(group_indices)) {
        group_idx <- which(group_indices)[1]
        
        # Find indices in topics and valid_subteams arrays
        topic_idx <- match(topic, topics)
        subteam_idx <- match(subteam, valid_subteams)
        
        if (!is.na(topic_idx) && !is.na(subteam_idx)) {
          # Get preference score from the array
          student_score <- pref_array[group_idx, topic_idx, subteam_idx]
          total_score <- total_score + student_score
          message("Added score ", student_score, " for student ", student_id, ", new total: ", total_score)
        }
      }
    }
    
    # Add team formation bonus (simplified)
    unique_teams <- unique(assignments$project_team)
    team_bonus <- length(unique_teams) * 50
    total_score <- total_score + team_bonus
    
    message("Final score: ", total_score, " (including team bonus of ", team_bonus, ")")
    
    return(total_score)
  }

  # Helper function to calculate individual scores for assignments
  calculate_individual_scores <- function(assignments, survey_data, topics, valid_subteams, pref_array) {
    # Initialize individual scores
    assignments$individual_score <- 0
    
    # Process each student assignment
    for (i in 1:nrow(assignments)) {
      student_id <- assignments$student_id[i]
      project_team <- assignments$project_team[i]
      subteam <- assignments$subteam[i]
      
      # Extract topic from project_team
      topic <- tryCatch({
        parts <- strsplit(project_team, "_team")[[1]]
        if (length(parts) > 0) parts[1] else project_team
      }, error = function(e) {
        warning("Invalid project_team format: ", project_team)
        return(NA)
      })
      
      # Skip if topic is NA
      if (is.na(topic)) next
      
      # Find the group in survey data
      student_cols <- grep("Student_ID", names(survey_data), value = TRUE)
      group_indices <- apply(survey_data[, student_cols, drop = FALSE], 1, function(row) {
        any(row == student_id, na.rm = TRUE)
      })
      
      if (any(group_indices)) {
        group_idx <- which(group_indices)[1]
        topic_idx <- match(topic, topics)
        subteam_idx <- match(subteam, valid_subteams)
        
        if (!is.na(topic_idx) && !is.na(subteam_idx)) {
          # Get preference score from the array
          assignments$individual_score[i] <- pref_array[group_idx, topic_idx, subteam_idx]
        }
      }
    }
    
    return(assignments)
  }
  
  #===========================================================================
  # UI RENDERING
  #===========================================================================
  
  # Render dynamic UI based on current page
  output$main_ui <- renderUI({
    current <- current_page()
    
    if (current == "login") {
      return(login_ui)
    } else if (current == "project_setup") {
      # Return the appropriate setup page based on selected model
      model_num <- as.numeric(model_type())
      
      return(switch(
        model_num,
        "1" = project_setup_model1_ui,
        "2" = project_setup_model2_ui,  # Use model 2 UI for diversity model
        "3" = project_setup_model3_ui
      ))
    } else if (current == "csv_upload") {
      return(csv_upload_ui)
    } else if (current == "result") {
      return(result_ui)
    }
  })


  # Update model display text on pages after login
  output$model_display <- renderText({
    model_num <- as.numeric(model_type())
    
    switch(
      model_num,
      "1" = "Model: Subteam Considerations",
      "2" = "Model: Diversity Optimization",  
      "3" = "Model: Subteam + Skills",
      "4" = "Model: No Subteam + Skills"
    )
  })

  # Solution metrics outputs
  output$teams_formed <- renderText({
    req(params$final_assignments)
    paste(length(unique(params$final_assignments$project_team)), "teams formed")
  })

  output$pref_satisfaction <- renderText({
    req(params$final_assignments)
    
    # Calculate percentage of students who got their first choice
    first_choice_count <- sum(params$final_assignments$individual_score >= 90)
    total_students <- nrow(params$final_assignments)
    percentage <- round(first_choice_count / total_students * 100)
    
    paste(percentage, "% first choice assignments")
  })

  output$topic_coverage <- renderText({
    req(params$final_assignments)
    
    # Check if project_team is a character column
    if ("project_team" %in% names(params$final_assignments) && 
        is.character(params$final_assignments$project_team)) {
      # Extract topics from project teams
      topics <- unique(sapply(
        strsplit(params$final_assignments$project_team, "_team"), 
        function(x) if(length(x) > 0) x[1] else NA
      ))
      topics <- topics[!is.na(topics)]
      paste(length(topics), "topics covered")
    } else {
      "Topics data not available"
    }
  })
  
  #---------------------------------------------------------------------------
  # Text Outputs for User Feedback
  #---------------------------------------------------------------------------
  
  # Welcome messages and user info
  output$welcome_message <- renderText({
    req(user_name(), course_name())
    paste("Hello", user_name(), ", Welcome to the Project Set-up Page for Course", course_name())
  })
  
  # User name
  output$profile_name <- renderText({
    req(user_name())
    paste("Hello,", user_name())
  })

  # Course name
  output$course_display <- renderText({
    req(course_name())
    paste("Course:", course_name())
  })
  
  # Preference score display
  output$preference_score_text <- renderText({
    req(params$preference_score)
    paste("Total Preference Score:", round(params$preference_score, 2))
  })
  
  # Score comparison with icons
  output$score_comparison_text <- renderUI({
    # Only show if we have both a current and previous score
    req(params$preference_score, params$previous_score)
    
    score_diff <- params$preference_score - params$previous_score
    
    if(score_diff > 0) {
      # Positive change - upward arrow with green text
      HTML(paste(
        "<span style='color: var(--success); font-weight: 500;'>",
        "<i class='fa fa-arrow-up' style='margin-right: 5px;'></i>",
        "Increased by", round(score_diff, 2), "points",
        "</span>"
      ))
    } else if(score_diff < 0) {
      # Negative change - downward arrow with red text
      HTML(paste(
        "<span style='color: var(--danger); font-weight: 500;'>",
        "<i class='fa fa-arrow-down' style='margin-right: 5px;'></i>",
        "Decreased by", round(abs(score_diff), 2), "points",
        "</span>"
      ))
    } else {
      # No change - horizontal arrow with neutral color
      HTML(paste(
        "<span style='color: var(--dark); font-weight: 500;'>",
        "<i class='fa fa-arrow-right' style='margin-right: 5px;'></i>",
        "No change in score",
        "</span>"
      ))
    }
  })
  
  # Solution navigation info
  output$solution_nav_info <- renderText({
    req(params$all_assignments, params$current_solution_idx)
    paste("Solution", params$current_solution_idx, "of", length(params$all_assignments))
  })
  
  # Optimization parameters display 
  output$optimization_params <- renderText({
    req(params$c_team, params$b_subteam, params$x_topic_teams)
    paste("Current Settings: Team Size =", params$c_team, 
          ", Subteam Size =", params$b_subteam, 
          ", Max Teams per Topic =", params$x_topic_teams)
  })
  
  # Manual edit score impact
  output$manual_edit_score_impact <- renderText({
    req(params$original_preference_score, params$edited_preference_score)
    
    score_diff <- params$edited_preference_score - params$original_preference_score
    
    if (score_diff > 0) {
      return(paste0("Score Impact: +", round(score_diff, 2), " points (Improved!)"))
    } else if (score_diff < 0) {
      return(paste0("Score Impact: ", round(score_diff, 2), " points (Reduced)"))
    } else {
      return("Score Impact: No change")
    }
  })

  output$model_specific_params_ui <- renderUI({
    model_num <- as.numeric(model_type())
    
    if(model_num == 1) {
      # Model 1: Basic Subteam Model
      tagList(
        numericInput("new_c_team", "Team Size", value = params$c_team, min = 1),
        numericInput("new_b_subteam", "Sub-team Size", value = params$b_subteam, min = 1),
        numericInput("new_x_topic_teams", "Max Teams per Topic", value = params$x_topic_teams, min = 1),
        numericInput("new_k_solutions", "Number of Solutions", value = params$k_solutions, min = 1, max = 10)
      )
    } else if(model_num == 2) {
      # Model 2: Diversity Optimization
      tagList(
        numericInput("new_c_team", "Team Size", value = params$c_team, min = 2),
        numericInput("new_k_solutions", "Number of Solutions", value = params$k_solutions, min = 1, max = 10)
      )
    } else if(model_num == 3) {
      # Model 3: Subteam + Skills
      tagList(
        numericInput("new_c_team", "Team Size", value = params$c_team, min = 1),
        numericInput("new_b_subteam", "Sub-team Size", value = params$b_subteam, min = 1),
        numericInput("new_x_topic_teams", "Max Teams per Topic", value = params$x_topic_teams, min = 1),
        numericInput("new_skills_weight", "Skills Matching Weight", value = params$skills_weight, min = 1, max = 10),
        numericInput("new_k_solutions", "Number of Solutions", value = params$k_solutions, min = 1, max = 10)
      )
    } 
  })

  # CSV format instructions output based on selected model
  output$csv_format_instructions <- renderUI({
    model_num <- as.numeric(model_type())
    
    if(model_num == 1) {
      # Model 1: Basic Subteam Model
      HTML('
        <p>Your CSV should include:</p>
        <ul>
          <li><strong>Student_ID columns</strong> (one for each student in a group)</li>
          <li><strong>Topic preference columns</strong> (First Choice, Second Choice, etc.)</li>
          <li><strong>Subteam role preference columns</strong></li>
        </ul>
        <div style="overflow-x: auto; margin-top: 10px;">
          <table style="width:100%; border-collapse: collapse; text-align: center; font-size: 0.9em;">
            <tr style="background-color: #2c3e50; color: white;">
              <th style="padding: 6px; border: 1px solid #ddd;">Student_ID #1</th>
              <th style="padding: 6px; border: 1px solid #ddd;">Student_ID #2</th>
              <th style="padding: 6px; border: 1px solid #ddd;">Student_ID #3</th>
              <th style="padding: 6px; border: 1px solid #ddd;">Student_ID #4</th>
              <th style="padding: 6px; border: 1px solid #ddd;">First Choice (Topic)</th>
              <th style="padding: 6px; border: 1px solid #ddd;">Second Choice (Topic)</th>
              <th style="padding: 6px; border: 1px solid #ddd;">First Choice (Subteam)</th>
              <th style="padding: 6px; border: 1px solid #ddd;">Second Choice (Subteam)</th>
            </tr>
          </table>
        </div>
      ')
    } else if(model_num == 2) {
      # Model 2: Diversity Optimization
      HTML('
        <p>Your CSV should include:</p>
        <ul>
          <li><strong>student_id column</strong> (one for each individual student)</li>
          <li><strong>diversity_category columns</strong> (e.g., major, nationality, year, gender)</li>
        </ul>
        <div style="overflow-x: auto; margin-top: 10px;">
          <table style="width:100%; border-collapse: collapse; text-align: center; font-size: 0.9em;">
            <tr style="background-color: #2c3e50; color: white;">
              <th style="padding: 6px; border: 1px solid #ddd;">student_id</th>
              <th style="padding: 6px; border: 1px solid #ddd;">diversity_category_major</th>
              <th style="padding: 6px; border: 1px solid #ddd;">diversity_category_nationality</th>
              <th style="padding: 6px; border: 1px solid #ddd;">diversity_category_year</th>
              <th style="padding: 6px; border: 1px solid #ddd;">diversity_category_gender</th>
            </tr>
          </table>
        </div>
      ')
    } else if(model_num == 3) {
      # Model 3: Subteam + Skills
      HTML('
        <p>Your CSV should include:</p>
        <ul>
          <li><strong>Student_ID columns</strong> (one for each student in a group)</li>
          <li><strong>Topic preference columns</strong></li>
          <li><strong>Subteam preference columns</strong></li>
          <li><strong>Skill level columns</strong> (numeric ratings)</li>
        </ul>
        <div style="overflow-x: auto; margin-top: 10px;">
          <table style="width:100%; border-collapse: collapse; text-align: center; font-size: 0.9em;">
            <tr style="background-color: #2c3e50; color: white;">
              <th style="padding: 6px; border: 1px solid #ddd;">Student_ID #1</th>
              <th style="padding: 6px; border: 1px solid #ddd;">Student_ID #2</th>
              <th style="padding: 6px; border: 1px solid #ddd;">First Choice (Topic)</th>
              <th style="padding: 6px; border: 1px solid #ddd;">First Choice (Subteam)</th>
              <th style="padding: 6px; border: 1px solid #ddd;">Skill 1 Level</th>
              <th style="padding: 6px; border: 1px solid #ddd;">Skill 2 Level</th>
            </tr>
          </table>
        </div>
      ')
    } 
  })

  # Output for model-specific metrics
  output$model_specific_metric <- renderUI({
    req(params$final_assignments)
    
    model_num <- as.numeric(model_type())
    
    if(model_num == 2) {
      # DIVERSITY MODEL
      # Get diversity metrics from solution
      diversity_value <- NULL
      
      if(!is.null(params$all_diversity_scores) && !is.null(params$current_solution_idx)) {
        diversity_scores <- params$all_diversity_scores[[params$current_solution_idx]]
        if(!is.null(diversity_scores$overall)) {
          # Average of all diversity metrics
          diversity_value <- mean(unlist(diversity_scores$overall), na.rm = TRUE)
        }
      }
      
      if(is.null(diversity_value)) {
        # Fallback if no diversity value is available - count unique categories per team
        teams <- unique(params$final_assignments$project_team)
        diversity_cols <- grep("diversity_category", names(params$final_assignments), value = TRUE, ignore.case = TRUE)
        
        if(length(diversity_cols) > 0) {
          # Calculate average diversity across teams
          team_diversities <- sapply(teams, function(team) {
            team_members <- params$final_assignments[params$final_assignments$project_team == team, ]
            mean(sapply(diversity_cols, function(col) {
              length(unique(team_members[[col]]))
            }))
          })
          diversity_value <- mean(team_diversities, na.rm = TRUE)
        } else {
          # If no diversity columns found
          diversity_value <- NA
        }
      }
      
      # Return UI element for diversity metric
      div(
        h5("Team Diversity", style = "margin: 0; font-weight: 500;"),
        if(!is.na(diversity_value)) {
          div(
            style = paste0("color: ", ifelse(diversity_value > 0.6, "var(--success)", 
                                          ifelse(diversity_value > 0.3, "var(--warning)", "var(--danger)")), 
                          "; font-weight: 500;"),
            paste0(round(diversity_value * 100), "% diverse")
          )
        } else {
          div("Diversity metrics not available")
        }
      )
      
    } else if(model_num == 3) {
      # SKILLS MODEL
      # Calculate skills balance
      skill_cols <- grep("skill", names(params$final_assignments), value = TRUE, ignore.case = TRUE)
      
      if(length(skill_cols) > 0) {
        # Get team skill balance
        teams <- unique(params$final_assignments$project_team)
        
        # Calculate skill coverage (percent of teams with all required skills)
        skill_coverage <- sapply(teams, function(team) {
          team_members <- params$final_assignments[params$final_assignments$project_team == team, ]
          all_skills_covered <- all(sapply(skill_cols, function(skill_col) {
            any(team_members[[skill_col]] >= params$min_skill_level %||% 3, na.rm = TRUE)
          }))
          return(all_skills_covered)
        })
        
        coverage_pct <- sum(skill_coverage) / length(teams) * 100
        
        # Return UI element for skills metric
        div(
          h5("Skills Coverage", style = "margin: 0; font-weight: 500;"),
          div(
            style = paste0("color: ", ifelse(coverage_pct > 80, "var(--success)", 
                                          ifelse(coverage_pct > 50, "var(--warning)", "var(--danger)")), 
                        "; font-weight: 500;"),
            paste0(round(coverage_pct), "% teams with all skills")
          )
        )
      } else {
        # If no skill columns found
        div(
          h5("Skills Coverage", style = "margin: 0; font-weight: 500;"),
          div("Skills data not available")
        )
      }
    } else {
      # Model 1 or other models
      div(
        h5("Team Balance", style = "margin: 0; font-weight: 500;"),
        textOutput("team_balance")
      )
    }
  })
  
  #---------------------------------------------------------------------------
  # Solution Comparison UI
  #---------------------------------------------------------------------------
  
  # Output for solution comparison cards
  output$solution_comparison <- renderUI({
    req(params$all_assignments, params$objective_values, params$score_diffs)
    
    # Get the number of solutions
    n_solutions <- length(params$all_assignments)
    best_score <- max(params$objective_values)
    best_idx <- which.max(params$objective_values)
    
    # Create a card for each solution
    solution_cards <- lapply(1:n_solutions, function(idx) {
      score <- params$objective_values[idx]
      diff <- params$score_diffs[idx]
      is_current <- idx == params$current_solution_idx
      is_best <- idx == best_idx
      
      # Determine card styling based on current selection and whether it's the best solution
      card_style <- if (is_current) {
        "border: 2px solid var(--secondary); box-shadow: 0 6px 16px rgba(0, 0, 0, 0.15);"
      } else if (is_best) {
        "border: 2px solid var(--success);"
      } else {
        ""
      }
      
      # Calculate differences for tooltip
      diff_student_count <- if(idx != best_idx) {
        current_sol <- params$all_assignments[[idx]]
        best_sol <- params$all_assignments[[best_idx]]
        sum(current_sol$project_team != best_sol$project_team | current_sol$subteam != best_sol$subteam)
      } else {
        0
      }

      # Create the card
      div(
        id = paste0("solution_card_", idx),
        class = "solution-card",
        style = paste(
          "background-color: white;",
          "border-radius: 8px;",
          "padding: 15px;",
          "margin-bottom: 15px;",
          "cursor: pointer;",
          "position: relative;", # Added for tooltip positioning
          card_style
        ),
        onclick = paste0("Shiny.setInputValue('select_solution', ", idx, ")"),
        
        # Adding solution specifics
        div(style = "border-bottom: 1px solid #eee; padding-bottom: 10px; margin-bottom: 10px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              div(
                h4(paste("Solution", idx), style = if(is_best) "color: var(--success); margin: 0;" else "margin: 0;"),
                if (is_best) {
                  tags$span(class = "badge", style = "background-color: var(--success); color: white; padding: 3px 8px; border-radius: 4px;", "Best")
                } else {
                  if (is_current) {
                    tags$span(class = "badge", style = "background-color: var(--secondary); color: white; padding: 3px 8px; border-radius: 4px;", "Current")
                  }
                }
              ),
              div(
                style = "text-align: right;",
                div(style = "font-weight: 600;", paste0("Score: ", round(score, 2))),
                if (diff > 0) {
                  div(style = "color: var(--danger); font-size: 0.9em;", 
                      paste0("-", round(diff, 2), " points from best"))
                } else {
                  div(style = "color: var(--success); font-size: 0.9em;", "Best score")
                }
              )
          )
        ),
        
        # Add solution metrics
        div(style = "display: grid; grid-template-columns: 1fr 1fr; font-size: 0.85em; color: var(--dark);",
            div(
              tags$i(class = "fa fa-users", style = "margin-right: 5px; color: var(--primary);"),
              paste(length(unique(params$all_assignments[[idx]]$project_team)), "teams")
            ),
            div(
              tags$i(class = "fa fa-exchange-alt", style = "margin-right: 5px; color: var(--primary);"),
              if(diff_student_count > 0) paste(diff_student_count, "changes") else "No changes"
            )
        ),
        
        # Add tooltip that shows on hover
        tags$div(
          class = "solution-tooltip",
          style = "display: none; position: absolute; background: white; border: 1px solid #ddd; padding: 10px; border-radius: 5px; z-index: 1000; width: 250px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); right: 0; top: 100%;",
          if(idx != best_idx) {
            HTML(paste0(
              "<strong>Key Differences:</strong>",
              "<ul style='margin-bottom: 0; padding-left: 20px;'>",
              "<li>", diff_student_count, " students assigned differently</li>",
              "<li>Teams formed: ", length(unique(params$all_assignments[[idx]]$project_team)), "</li>",
              "<li>Preference score is ", round(diff), " points lower</li>",
              "</ul>"
            ))
          } else {
            tagList(
              tags$strong("Best Solution"), 
              tags$br(),
              "Optimal assignment based on preferences"
    )
          }
        ),
        
        # Add mouse events to show/hide tooltip
        onmouseover = paste0("document.getElementById('solution_card_", idx, "').querySelector('.solution-tooltip').style.display='block';"),
        onmouseout = paste0("document.getElementById('solution_card_", idx, "').querySelector('.solution-tooltip').style.display='none';")
      )
    })
    
    # Wrap cards in a container
    div(
      style = "margin-top: 15px;",
      solution_cards
    )
  })
  
  # Solution differences output
  output$solution_differences <- renderUI({
    req(params$all_assignments, params$current_solution_idx)
    
    # Only show differences if we're not viewing the best solution
    if (params$current_solution_idx == which.max(params$objective_values)) {
      return(div(
        style = "color: var(--success); font-style: italic;",
        "This is the best solution."
      ))
    }
    
    # Get current and best solution data
    current_sol <- params$all_assignments[[params$current_solution_idx]]
    best_sol <- params$all_assignments[[which.max(params$objective_values)]]
    
    # Find differences
    diff_students <- current_sol$student_id[current_sol$project_team != best_sol$project_team | 
                                          current_sol$subteam != best_sol$subteam]
    
    # Return the complete UI - simplified version to avoid errors
    div(
      p(paste0(length(diff_students), " students assigned differently from the best solution:")),
      
      if(length(diff_students) > 0) {
        tags$ul(
          style = "margin-bottom: 0;",
          lapply(1:min(length(diff_students), 5), function(i) {
            # Be very careful with variable scoping here
            student_id <- diff_students[i]
            # Find the corresponding rows safely
            current_idx <- which(current_sol$student_id == student_id)[1]
            best_idx <- which(best_sol$student_id == student_id)[1]
            
            if(!is.na(current_idx) && !is.na(best_idx)) {
              tags$li(
                tags$strong(student_id), ": ",
                tags$span(
                  style = "color: var(--danger);",
                  paste0(best_sol$project_team[best_idx], "/", best_sol$subteam[best_idx])
                ),
                " → ",
                tags$span(
                  style = "color: var(--primary);",
                  paste0(current_sol$project_team[current_idx], "/", current_sol$subteam[current_idx])
                )
              )
            } else {
              # Skip this student if we can't find matching rows
              NULL
            }
          })
        )
      } else {
        p("No specific differences to display.")
      },
      
      if(length(diff_students) > 5) {
        p(paste0("... and ", length(diff_students) - 5, " more"))
      }
    )
  })

  
  #---------------------------------------------------------------------------
  # Data Tables for Results
  #---------------------------------------------------------------------------
  
  # Standard allocation table (view mode)
  output$allocation_table <- DT::renderDataTable({
    req(params$final_assignments)
    
    # Debug info
    message("Rendering allocation table. Row count: ", nrow(params$final_assignments))
    message("Columns: ", paste(names(params$final_assignments), collapse=", "))
    
    # Ensure individual scores are calculated
    if (!"individual_score" %in% names(params$final_assignments)) {
      message("Adding missing individual scores to final_assignments")
      # Calculate individual scores
      tryCatch({
        data_list <- read_data("survey_data.csv")
        params$final_assignments <- calculate_individual_scores(
          params$final_assignments,
          data_list$survey_data,
          data_list$topics,
          data_list$valid_subteams,
          data_list$pref_array
        )
      }, error = function(e) {
        message("Error calculating individual scores: ", e$message)
        # Fallback: Use solution score for all
        if ("solution_score" %in% names(params$final_assignments)) {
          params$final_assignments$individual_score <- params$final_assignments$solution_score
        } else {
          params$final_assignments$individual_score <- 0
        }
      })
    }
    
    # Ensure all required columns exist
    required_cols <- c("student_id", "project_team", "subteam", "solution_number", "individual_score")
    for (col in required_cols) {
      if (!col %in% names(params$final_assignments)) {
        params$final_assignments[[col]] <- NA
      }
    }
    
    # Add group_id if missing
    if (!"group_id" %in% names(params$final_assignments)) {
      params$final_assignments$group_id <- 1:nrow(params$final_assignments)
    }
    
    # Prepare data for display
    display_data <- params$final_assignments %>%
      select(student_id, project_team, subteam, solution_number, individual_score, group_id)
    
    # Create a formatted DT table
    DT::datatable(
      display_data,
      rownames = FALSE,
      colnames = c(
        "Student ID", 
        "Project Team", 
        "Subteam", 
        "Solution Number", 
        "Preference Score",
        "Group ID"  # Add column name for group identification
      ),
      options = list(
        pageLength = 25,
        dom = 'ftip',
        order = list(list(1, 'asc'), list(2, 'asc')), # Sort by group_id first, then project team and subteam
        rowCallback = JS("
          function(row, data, index) {
            // Create a group identifier from project_team and subteam
            var groupId = data[1] + '_' + data[2]; // Combine project_team and subteam
            
            // Store this as a data attribute for styling
            $(row).attr('data-group', groupId);
            
            // Apply styling based on project_team
            var projectTeam = data[1];
            if(projectTeam.includes('Topic 1')) {
              $(row).addClass('group-even');
            } else if(projectTeam.includes('Topic 2')) {
              $(row).addClass('group-odd');
            } else if(projectTeam.includes('Topic 3')) {
              $(row).addClass('group-even');
            } else if(projectTeam.includes('Topic 4')) {
              $(row).addClass('group-odd');
            } else if(projectTeam.includes('Topic 5')) {
              $(row).addClass('group-even');
            } else {
              // Alternate for any other topics
              if (index % 2 === 0) {
                $(row).addClass('group-even');
              } else {
                $(row).addClass('group-odd');
              }
            }
            
            // Additional styling for subteams
            var subteam = data[2];
            if(subteam === 'A') {
              $(row).find('td:eq(2)').css('border-left', '3px solid rgba(26, 188, 156, 0.7)');
            } else if(subteam === 'B') {
              $(row).find('td:eq(2)').css('border-left', '3px solid rgba(52, 152, 219, 0.7)');
            }
          }
        "),
        columnDefs = list(
          list(
            targets = 4, # preference score column
            render = JS("
              function(data, type, row, meta) {
                if(type === 'display'){
                  var score = parseFloat(data);
                  var color = score > 7 ? '#2ecc71' : (score > 3 ? '#f39c12' : '#e74c3c');
                  return '<span style=\"color: ' + color + '; font-weight: 500;\">' + score.toFixed(1) + '</span>';
                }
                return data;
              }
            ")
          ),
          list(
            targets = 5, # Hide the group_id column
            visible = FALSE
          )
        )
      ),
      selection = 'none'
    )
  })
  
  # Editable allocation table (edit mode)
  output$editable_allocation_table <- DT::renderDataTable({
    req(params$edited_assignments)
    
    # Debug information
    message("Rendering editable allocation table")
    message("Row count in edited_assignments: ", nrow(params$edited_assignments))
    message("Columns: ", paste(names(params$edited_assignments), collapse=", "))
    
    # Make sure we have valid data structure
    if (nrow(params$edited_assignments) == 0) {
      message("Warning: edited_assignments has 0 rows")
      # If edited_assignments is empty, try to initialize it from final_assignments
      if (!is.null(params$final_assignments) && nrow(params$final_assignments) > 0) {
        params$edited_assignments <- params$final_assignments
        message("Initialized edited_assignments from final_assignments with ", nrow(params$final_assignments), " rows")
      } else {
        # Return empty table if we can't get valid data
        return(DT::datatable(data.frame(
          student_id = character(0),
          project_team = character(0),
          subteam = character(0),
          solution_number = integer(0),
          individual_score = numeric(0),
          group_id = integer(0)
        )))
      }
    }
    
    # Get unique project teams and subteams for dropdowns
    unique_teams <- unique(params$edited_assignments$project_team)
    unique_subteams <- unique(params$edited_assignments$subteam)
    
    message("Unique teams: ", length(unique_teams))
    message("Unique subteams: ", length(unique_subteams))
    
    # Create a display data frame with individual scores instead of solution score
    display_data <- params$edited_assignments
    
    # If individual_score column doesn't exist yet, try to calculate it
    if (!"individual_score" %in% names(display_data)) {
      message("Adding missing individual scores to edited_assignments")
      # Use the helper function to add individual scores
      display_data <- tryCatch({
        data_list <- read_data("survey_data.csv")
        calculate_individual_scores(
          display_data,
          data_list$survey_data,
          data_list$topics,
          data_list$valid_subteams,
          data_list$pref_array
        )
      }, error = function(e) {
        message("Error calculating individual scores: ", e$message)
        # If there's an error, just use the solution score for all
        if ("solution_score" %in% names(display_data)) {
          display_data$individual_score <- display_data$solution_score
        } else {
          display_data$individual_score <- 0
        }
        return(display_data)
      })
    }
    
    # Ensure all required columns exist
    required_cols <- c("student_id", "project_team", "subteam", "solution_number", "individual_score")
    for (col in required_cols) {
      if (!col %in% names(display_data)) {
        display_data[[col]] <- NA
        message("Added missing column: ", col)
      }
    }
    
    # Add group_id if missing
    if (!"group_id" %in% names(display_data)) {
      display_data$group_id <- 1:nrow(display_data)
      message("Added missing group_id column")
    }
    
    # Update edited_assignments with any fixes we made
    params$edited_assignments <- display_data
    
    # Create a datatable with editable cells for project_team and subteam
    DT::datatable(
      display_data %>% select(student_id, project_team, subteam, solution_number, individual_score, group_id),
      rownames = FALSE,
      colnames = c(
        "Student ID", 
        "Project Team", 
        "Subteam", 
        "Solution Number", 
        "Preference Score",
        "Group ID"  # Add column name
      ),
      editable = list(
        target = "cell", 
        disable = list(columns = c(0, 3, 4, 5)) # Disable editing for student_id, solution_number, preference_score, group_id
      ),
      options = list(
        pageLength = 25,
        dom = 'frtip',
        rowCallback = JS("
          function(row, data, index) {
            // Use the group_id (column 5) to add a specific class
            var groupId = data[5];
            $(row).addClass('group-' + groupId);
            
            // Add alternating color based on group_id
            if (groupId % 2 == 0) {
              $(row).addClass('group-even');
            } else {
              $(row).addClass('group-odd');
            }
          }
        "),
        columnDefs = list(
          list(
            targets = 1, # project_team column
            render = JS(
              paste0(
                "function(data, type, row, meta) {",
                "  if(type === 'display'){",
                "    var select = '<select class=\"project-team-select\" data-student=\"' + row[0] + '\">';",
                "    var options = ", jsonlite::toJSON(unique_teams), ";",
                "    for(var i=0; i<options.length; i++){",
                "      var selected = options[i] === data ? 'selected' : '';",
                "      select += '<option value=\"' + options[i] + '\" ' + selected + '>' + options[i] + '</option>';",
                "    }",
                "    select += '</select>';",
                "    return select;",
                "  }",
                "  return data;",
                "}"
              )
            )
          ),
          list(
            targets = 2, # subteam column
            render = JS(
              paste0(
                "function(data, type, row, meta) {",
                "  if(type === 'display'){",
                "    var select = '<select class=\"subteam-select\" data-student=\"' + row[0] + '\">';",
                "    var options = ", jsonlite::toJSON(unique_subteams), ";",
                "    for(var i=0; i<options.length; i++){",
                "      var selected = options[i] === data ? 'selected' : '';",
                "      select += '<option value=\"' + options[i] + '\" ' + selected + '>' + options[i] + '</option>';",
                "    }",
                "    select += '</select>';",
                "    return select;",
                "  }",
                "  return data;",
                "}"
              )
            )
          ),
          list(
            targets = 4, # preference score column
            render = JS("
              function(data, type, row, meta) {
                if(type === 'display'){
                  var score = parseFloat(data);
                  if(isNaN(score)) score = 0;
                  var color = score > 7 ? '#2ecc71' : (score > 3 ? '#f39c12' : '#e74c3c');
                  return '<span style=\"color: ' + color + '; font-weight: 500;\">' + score.toFixed(1) + '</span>';
                }
                return data;
              }
            ")
          ),
          list(
            targets = 5, # Hide the group_id column
            visible = FALSE
          )
        )
      ),
      callback = JS("
        table.on('change', 'select.project-team-select', function() {
          var cell = table.cell($(this).closest('td'));
          var row = table.row($(this).closest('tr')).index();
          var student = $(this).closest('tr').find('td:first').text();
          var newValue = $(this).val();
          
          // Update the cell value
          cell.data(newValue).draw();
          
          // Send the update to Shiny
          Shiny.setInputValue('project_team_change', {
            student_id: student,
            new_value: newValue,
            row: row
          });
        });
        
        table.on('change', 'select.subteam-select', function() {
          var cell = table.cell($(this).closest('td'));
          var row = table.row($(this).closest('tr')).index();
          var student = $(this).closest('tr').find('td:first').text();
          var newValue = $(this).val();
          
          // Update the cell value
          cell.data(newValue).draw();
          
          // Send the update to Shiny
          Shiny.setInputValue('subteam_change', {
            student_id: student,
            new_value: newValue,
            row: row
          });
        });
      ")
    )
  })
  
  # Download handler for results
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("student_assignments_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(params$final_assignments)
      write.csv(params$final_assignments, file, row.names = FALSE)
    }
  )
  
  #===========================================================================
  # PAGE NAVIGATION HANDLERS
  #===========================================================================
  
  # 1) Login -> Project Setup
  observeEvent(input$go, {
    if (input$username != "" && input$course != "") {
      user_name(input$username)
      course_name(input$course)
      model_type(input$selected_model)  # Store selected model
      current_page("project_setup")
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please enter both your username and course name.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # 2) "Back" from Project Setup -> Login
  observeEvent(input$back_to_login, {
    current_page("login")
  })
  
  # 3) "Back" from CSV Upload -> Project Setup
  observeEvent(input$back_to_setup, {
    current_page("project_setup")
  })
  
  # 4) "Back" from Result -> CSV Upload
  observeEvent(input$back_to_upload, {
    current_page("csv_upload")
  })
  
  # 5) Project Setup -> CSV Upload
  observeEvent(input$next_step, {
    # Get model number
    model_num <- as.numeric(model_type())
    
    # Check which parameters need validation based on model
    valid_inputs <- TRUE
    error_msg <- ""
    
    if(model_num == 1) {
      # Model 1: Check subteam-related parameters
      if (is.na(input$c_team) || is.na(input$b_subteam) || is.na(input$x_topic_teams) || is.na(input$k_solutions)) {
        valid_inputs <- FALSE
        error_msg <- "Please enter valid values for all fields."
      }
    } else if(model_num == 2) {
      # Model 2: Check only team size and k_solutions
      if (is.na(input$c_team) || is.na(input$k_solutions)) {
        valid_inputs <- FALSE
        error_msg <- "Please enter valid values for team size and number of solutions."
      }
    } else if(model_num == 3) {
      # Models 3 & 4: Check all skill-related parameters
      if (is.na(input$c_team) || is.na(input$b_subteam) || is.na(input$x_topic_teams) || 
          is.na(input$skills_weight) || is.na(input$k_solutions)) {
        valid_inputs <- FALSE
        error_msg <- "Please enter valid values for all fields."
      }
    }
    
    # Show error if invalid inputs
    if(!valid_inputs) {
      showModal(modalDialog(
        title = "Error",
        error_msg,
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # Save parameters based on model type
    if(model_num == 1) {
      data_to_save <- data.frame(
        c_team        = input$c_team,
        b_subteam     = input$b_subteam,
        x_topic_teams = input$x_topic_teams
      )
      
      # Store values in reactive values
      params$c_team <- as.numeric(input$c_team)
      params$b_subteam <- as.numeric(input$b_subteam)
      params$x_topic_teams <- as.numeric(input$x_topic_teams)
      params$k_solutions <- as.numeric(input$k_solutions)
      
    } else if(model_num == 2) {
      # For Model 2 (Diversity), we only need c_team
      data_to_save <- data.frame(
        c_team          = input$c_team,
        x_topic_teams   = 1,  # Default value not needed for diversity model
        diversity_weight = 0.7  # Default diversity weight
      )
      
      # Store values in reactive values
      params$c_team <- as.numeric(input$c_team)
      params$k_solutions <- as.numeric(input$k_solutions)
      
    } else {
      # Models 3 & 4
      data_to_save <- data.frame(
        c_team        = input$c_team,
        b_subteam     = input$b_subteam,
        x_topic_teams = input$x_topic_teams,
        skills_weight = input$skills_weight
      )
      
      # Store values in reactive values
      params$c_team <- as.numeric(input$c_team)
      params$b_subteam <- as.numeric(input$b_subteam)
      params$x_topic_teams <- as.numeric(input$x_topic_teams)
      params$skills_weight <- as.numeric(input$skills_weight)
      params$k_solutions <- as.numeric(input$k_solutions)
    }
    
    # Save to CSV
    tryCatch({
      write.table(
        data_to_save,
        file = "user_inputs.csv",
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,  # As expected by read_project_data
        append = FALSE
      )
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to save settings:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    })
    
    current_page("csv_upload")
  })
  
  # 6) Open Microsoft Form in new tab
  observeEvent(input$go_survey, {
    # Optionally also save numeric inputs here if needed
    form_url <- "https://forms.office.com/Pages/ResponsePage.aspx?id=DQSIkWdsW0yxEjajBLZtrQAAAAAAAAAAAANAATn8PsFUMjI0Q0JLOEgwRkwxVTdaNjlYNzJTVk1RUy4u"
    js <- sprintf("window.open('%s', '_blank');", form_url)
    session$sendCustomMessage(type = "jsCode", js)
  })
  
  # Update input values when returning to the project setup page
  observe({
    if (current_page() == "project_setup" && !is.null(params$c_team)) {
      updateNumericInput(session, "c_team", value = params$c_team)
      updateNumericInput(session, "b_subteam", value = params$b_subteam)
      updateNumericInput(session, "x_topic_teams", value = params$x_topic_teams)
    }
  })

  # Update model type when selection changes
  observeEvent(input$selected_model, {
    model_type(input$selected_model)
  })
  
  #===========================================================================
  # CSV UPLOAD AND DATA PROCESSING
  #===========================================================================
  
  # Upload CSV -> Save to "survey_data.csv"
  observeEvent(input$upload_csv, {
    req(input$survey_csv)
    
    # Copy the uploaded file to a known location
    dest_path <- "survey_data.csv"
    
    tryCatch({
      file.copy(input$survey_csv$datapath, dest_path, overwrite = TRUE)
      params$survey_uploaded <- TRUE
      
      # Enable the generate allocation button
      shinyjs::enable("generate_allocation")
      
      showModal(modalDialog(
        title = "Upload Successful",
        paste("CSV has been saved to", dest_path),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Upload Error",
        paste("Failed to save the CSV file:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Enable/disable the Generate Allocation button based on whether CSV is uploaded
  observe({
    shinyjs::toggleState("generate_allocation", condition = params$survey_uploaded)
  })
  

  #===========================================================================
  # OPTIMIZATION AND SOLUTION MANAGEMENT
  #===========================================================================
  
  #---------------------------------------------------------------------------
  # Run Optimization
  #---------------------------------------------------------------------------
  
  # Generate Allocation -> Run Optimization -> Go to Result Page
  observeEvent(input$generate_allocation, {
  req(params$survey_uploaded)  # Ensure user uploaded CSV
  
  # Show progress indicator
  withProgress(message = "Running optimization...", {
    
    # Get the model type
    model_num <- as.numeric(model_type())

    if(model_num == 1) {
      # Ensure parameters are numeric before calling optimization
      c_team <- as.numeric(params$c_team)
      b_subteam <- as.numeric(params$b_subteam)
      x_topic_teams <- as.numeric(params$x_topic_teams)
      k_solutions <- as.numeric(params$k_solutions)
      
      # Debug info
      message("Original Optimization parameters:")
      message("c_team = ", c_team, " (", class(c_team), ")")
      message("b_subteam = ", b_subteam, " (", class(b_subteam), ")")
      message("x_topic_teams = ", x_topic_teams, " (", class(x_topic_teams), ")")
      message("k_solutions = ", k_solutions, " (", class(k_solutions), ")")
      
      # Run the original optimization
      tryCatch({
        # Pass the path to the uploaded CSV file
        res <- run_multi_solution_optimization(
          "survey_data.csv",
          params$k_solutions,
          5.0  # Default score gap of 5%
        )
        
        # Check if we have valid results
        if (length(res$assignments_list) > 0 && !is.null(res$assignments_list[[res$best_solution_index]])) {
          # Store the results
          params$all_assignments <- res$assignments_list
          params$objective_values <- res$objective_values
          params$score_diffs <- res$score_diffs
          params$current_solution_idx <- res$best_solution_index
          params$final_assignments <- res$assignments_list[[res$best_solution_index]]
          params$preference_score <- res$objective_values[res$best_solution_index]
          params$previous_score <- NULL  # No previous score for first run
          params$error_message <- NULL
          
          # Debug output
          message("Best solution index: ", res$best_solution_index)
          message("Row count in final_assignments: ", nrow(params$final_assignments))
          message("Columns in final_assignments: ", paste(names(params$final_assignments), collapse=", "))
          
          if (!"individual_score" %in% names(params$final_assignments)) {
            # This is a fallback in case the process_solution function didn't add individual scores
            data_list <- read_data("survey_data.csv")
            params$final_assignments <- calculate_individual_scores(
              params$final_assignments,
              data_list$survey_data,
              data_list$topics,
              data_list$valid_subteams,
              data_list$pref_array
            )
          }
          
          # Go to results page
          current_page("result")
        } else {
          showModal(modalDialog(
            title = "Optimization Error",
            "The optimization completed but didn't produce valid assignments. Please check your data and parameters.",
            easyClose = TRUE
          ))
        }
      }, error = function(e) {
        showModal(modalDialog(
          title = "Optimization Error", 
          paste("Failed to generate allocation:", e$message),
          easyClose = TRUE
        ))
      })
    } else if(model_num == 2) {
      # MODEL 2: DIVERSITY OPTIMIZATION
      # Ensure parameters are numeric
      c_team <- as.numeric(params$c_team)
      k_solutions <- as.numeric(params$k_solutions)
      
      # Print debug info to console
      message("Diversity Optimization parameters:")
      message("c_team = ", c_team, " (", class(c_team), ")")
      message("k_solutions = ", k_solutions, " (", class(k_solutions), ")")
      
      # Run the diversity optimization with error handling
      tryCatch({
        # Use the diversity optimization function
        res <- run_diversity_optimization(
          "survey_data.csv",
          team_size = c_team,
          k_solutions = k_solutions,
          score_gap_percent = 5.0
        )
        
        # Check if we have valid results
        if (length(res$assignments_list) > 0 && !is.null(res$assignments_list[[res$best_solution_index]])) {
          # Store the results
          params$all_assignments <- res$assignments_list
          params$objective_values <- res$objective_values
          params$score_diffs <- res$objective_values[res$best_solution_index] - res$objective_values
          params$current_solution_idx <- res$best_solution_index
          params$final_assignments <- res$assignments_list[[res$best_solution_index]]
          params$preference_score <- res$objective_values[res$best_solution_index]
          params$previous_score <- NULL  # No previous score for first run
          params$error_message <- NULL
          
          # Debug output
          message("Best solution index: ", res$best_solution_index)
          message("Row count in final_assignments: ", nrow(params$final_assignments))
          message("Columns in final_assignments: ", paste(names(params$final_assignments), collapse=", "))
          
          # Go to results page
          current_page("result")
        } else {
          showModal(modalDialog(
            title = "Optimization Error",
            "The diversity optimization completed but didn't produce valid assignments. Please check your data and parameters.",
            easyClose = TRUE
          ))
        }
      }, error = function(e) {
        showModal(modalDialog(
          title = "Optimization Error",
          paste("Failed to run diversity optimization:", e$message),
          easyClose = TRUE
        ))
      })
      
    } else if(model_num == 3) {
      # MODEL 3: SUBTEAM + SKILLS
      
      # Ensure parameters are numeric
      c_team <- as.numeric(params$c_team)
      b_subteam <- as.numeric(params$b_subteam)
      x_topic_teams <- as.numeric(params$x_topic_teams)
      skills_weight <- as.numeric(params$skills_weight)
      k_solutions <- as.numeric(params$k_solutions)
      
      # Debug info
      message("Subteam + Skills Optimization parameters:")
      message("c_team = ", c_team, " (", class(c_team), ")")
      message("b_subteam = ", b_subteam, " (", class(b_subteam), ")")
      message("x_topic_teams = ", x_topic_teams, " (", class(x_topic_teams), ")")
      message("skills_weight = ", skills_weight, " (", class(skills_weight), ")")
      message("k_solutions = ", k_solutions, " (", class(k_solutions), ")")
      
      # Run the subteam + skills optimization
      tryCatch({
        # Use the specific function for Model 3
        res <- run_multi_solution_optimization_subteam_skills(
          "survey_data.csv",
          k_solutions,
          5.0  # Default score gap of 5%
        )
        
        # Check if we have valid results
        if (length(res$assignments_list) > 0 && !is.null(res$assignments_list[[res$best_solution_index]])) {
          # Store the results
          params$all_assignments <- res$assignments_list
          params$objective_values <- res$objective_values
          params$score_diffs <- res$score_diffs
          params$current_solution_idx <- res$best_solution_index
          params$final_assignments <- res$assignments_list[[res$best_solution_index]]
          params$preference_score <- res$objective_values[res$best_solution_index]
          params$previous_score <- NULL  # No previous score for first run
          params$error_message <- NULL
          
          # Debug output
          message("Best solution index: ", res$best_solution_index)
          message("Row count in final_assignments: ", nrow(params$final_assignments))
          message("Columns in final_assignments: ", paste(names(params$final_assignments), collapse=", "))
          
          if (!"individual_score" %in% names(params$final_assignments)) {
            # This is a fallback in case the process_solution function didn't add individual scores
            data_list <- read_data_subteam_skills("survey_data.csv")
            params$final_assignments <- calculate_individual_scores(
              params$final_assignments,
              data_list$survey_data,
              data_list$topics,
              data_list$valid_subteams,
              data_list$pref_array
            )
          }
          
          # Go to results page
          current_page("result")
        } else {
          showModal(modalDialog(
            title = "Optimization Error",
            "The optimization completed but didn't produce valid assignments. Please check your data and parameters.",
            easyClose = TRUE
          ))
        }
      }, error = function(e) {
        showModal(modalDialog(
          title = "Optimization Error", 
          paste("Failed to generate allocation:", e$message),
          easyClose = TRUE
        ))
      })
    }
  })
})
  
  #---------------------------------------------------------------------------
  # Solution Navigation
  #---------------------------------------------------------------------------
  
  # Next solution button
  observeEvent(input$next_solution, {
    req(params$all_assignments)
    
    # Calculate next index (with wrap-around)
    next_idx <- params$current_solution_idx %% length(params$all_assignments) + 1
    
    # Update displayed solution
    params$current_solution_idx <- next_idx
    params$final_assignments <- params$all_assignments[[next_idx]]
    params$preference_score <- params$objective_values[next_idx]
    
    # Make sure individual scores are present
    if (!"individual_score" %in% names(params$final_assignments)) {
      data_list <- read_data("survey_data.csv")
      params$final_assignments <- calculate_individual_scores(
        params$final_assignments,
        data_list$survey_data,
        data_list$topics,
        data_list$valid_subteams,
        data_list$pref_array
      )
    }
  })
  
  # Solution card click handler
  observeEvent(input$select_solution, {
    req(params$all_assignments, input$select_solution)
  
    model_num <- as.numeric(model_type())
  
    # Use the appropriate read_data function based on model
    data_list <- if(model_num == 2) {
      tryCatch({
        read_data_diversity("survey_data.csv")
      }, error = function(e) {
        message("Error with diversity model: ", e$message)
        # Fall back to standard data reader
        read_data("survey_data.csv")
      })
    } else if(model_num == 3) {
      read_data_subteam_skills("survey_data.csv")
    } else {
      read_data("survey_data.csv")
    }
    sol_idx <- input$select_solution
    if (sol_idx >= 1 && sol_idx <= length(params$all_assignments)) {
      params$current_solution_idx <- sol_idx
      params$final_assignments <- params$all_assignments[[sol_idx]]
      params$preference_score <- params$objective_values[sol_idx]
      
      # Make sure individual scores are present
      if (!"individual_score" %in% names(params$final_assignments)) {
        data_list <- read_data("survey_data.csv")
        params$final_assignments <- calculate_individual_scores(
          params$final_assignments,
          data_list$survey_data,
          data_list$topics,
          data_list$valid_subteams,
          data_list$pref_array
        )
      }
    }
  })
  
  #---------------------------------------------------------------------------
  # Parameter Changes on Results Page
  #---------------------------------------------------------------------------
  
  # Toggle parameter panel on result page
  observeEvent(input$change_params, {
    shinyjs::toggle("params_panel")
    
    # Initialize new parameter inputs with current values
    if (!is.null(params$c_team)) {
      updateNumericInput(session, "new_c_team", value = params$c_team)
      updateNumericInput(session, "new_b_subteam", value = params$b_subteam)
      updateNumericInput(session, "new_x_topic_teams", value = params$x_topic_teams)
    }
  })
  
  # Run model again with new parameters
  observeEvent(input$run_again, {
    # Get the model type
    model_num <- as.numeric(model_type())
    
    # Store the current score as previous before running again
    params$previous_score <- params$preference_score
    
    # Different parameter handling based on model type
    if(model_num == 1) {
      # MODEL 1: BASIC SUBTEAM MODEL
      # Validate inputs
      if (is.na(input$new_c_team) || is.na(input$new_b_subteam) || is.na(input$new_x_topic_teams)) {
        showModal(modalDialog(
          title = "Error",
          "Please enter valid values for all fields.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      # Update reactive values
      params$c_team <- as.numeric(input$new_c_team)
      params$b_subteam <- as.numeric(input$new_b_subteam)
      params$x_topic_teams <- as.numeric(input$new_x_topic_teams)
      
      # Save to CSV
      data_to_save <- data.frame(
        c_team        = input$new_c_team,
        b_subteam     = input$new_b_subteam,
        x_topic_teams = input$new_x_topic_teams
      )
      
      # Log for debugging
      message("Updated parameters for model 1:")
      message("c_team = ", params$c_team)
      message("b_subteam = ", params$b_subteam)
      message("x_topic_teams = ", params$x_topic_teams)
      
    } else if(model_num == 2) {
      # MODEL 2: DIVERSITY OPTIMIZATION
      # Validate inputs
      if (is.na(input$new_c_team) || is.na(input$new_k_solutions)) {
        showModal(modalDialog(
          title = "Error",
          "Please enter valid values for team size and number of solutions.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      # Update reactive values
      params$c_team <- as.numeric(input$new_c_team)
      params$k_solutions <- as.numeric(input$new_k_solutions)
      
      # Save to CSV for compatibility with read_data function
      data_to_save <- data.frame(
        c_team          = input$new_c_team,
        x_topic_teams   = 1,  # Default for diversity model
        diversity_weight = 0.7  # Default diversity weight
      )
      
      # Log for debugging
      message("Updated parameters for model 2 (Diversity):")
      message("c_team = ", params$c_team)
      message("k_solutions = ", params$k_solutions)
      
    } else if(model_num == 3) {
      # MODEL 3: SUBTEAM + SKILLS
      # Validate inputs
      if (is.na(input$new_c_team) || is.na(input$new_b_subteam) || 
          is.na(input$new_x_topic_teams) || is.na(input$new_skills_weight) || 
          is.na(input$new_k_solutions)) {
        showModal(modalDialog(
          title = "Error",
          "Please enter valid values for all fields.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      # Update reactive values
      params$c_team <- as.numeric(input$new_c_team)
      params$b_subteam <- as.numeric(input$new_b_subteam)
      params$x_topic_teams <- as.numeric(input$new_x_topic_teams)
      params$skills_weight <- as.numeric(input$new_skills_weight)
      params$k_solutions <- as.numeric(input$new_k_solutions)
      
      # Save to CSV
      data_to_save <- data.frame(
        c_team        = input$new_c_team,
        b_subteam     = input$new_b_subteam,
        x_topic_teams = input$new_x_topic_teams,
        skills_weight = input$new_skills_weight
      )
      
      # Log for debugging
      message("Updated parameters for model 3 (Subteam + Skills):")
      message("c_team = ", params$c_team)
      message("b_subteam = ", params$b_subteam)
      message("x_topic_teams = ", params$x_topic_teams)
      message("skills_weight = ", params$skills_weight)
      message("k_solutions = ", params$k_solutions)
      
    }
    
    # Save parameters to CSV (common for all models)
    tryCatch({
      write.table(
        data_to_save,
        file = "user_inputs.csv",
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        append = FALSE
      )
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to save settings:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    })
    
    # Show progress while running optimization
    withProgress(message = "Running optimization with new parameters...", {
      # Run different optimization based on model type
      if(model_num == 2) {
        # MODEL 2: DIVERSITY OPTIMIZATION
        tryCatch({
          # Use the diversity optimization function with updated parameters
          res <- run_diversity_optimization(
            "survey_data.csv", 
            team_size = params$c_team,
            k_solutions = params$k_solutions,
            score_gap_percent = 5.0
          )
          
          # Check if we have valid results
          if (length(res$assignments_list) > 0 && !is.null(res$assignments_list[[res$best_solution_index]])) {
            # Store the results
            params$all_assignments <- res$assignments_list
            params$objective_values <- res$objective_values
            params$score_diffs <- res$score_diffs
            params$current_solution_idx <- res$best_solution_index
            params$final_assignments <- res$assignments_list[[res$best_solution_index]]
            params$preference_score <- res$objective_values[res$best_solution_index]
            params$all_diversity_scores <- res$diversity_scores
            params$error_message <- NULL
            
            # Debug output
            message("Best solution index: ", res$best_solution_index)
            message("Row count in final_assignments: ", nrow(params$final_assignments))
            
            # Hide the parameter panel
            shinyjs::hide("params_panel")
            
            # Show success message with score comparison
            score_diff <- params$preference_score - params$previous_score
            score_change_msg <- if(score_diff > 0) {
              paste("Diversity score increased by", round(score_diff, 2), "points!")
            } else if(score_diff < 0) {
              paste("Diversity score decreased by", round(abs(score_diff), 2), "points.")
            } else {
              "Diversity score remained the same."
            }
            
            showModal(modalDialog(
              title = "Success",
              HTML(paste(
                "Diversity optimization ran successfully with new parameters!<br><br>",
                "<strong>", score_change_msg, "</strong>"
              )),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          } else {
            showModal(modalDialog(
              title = "Optimization Error",
              "The diversity optimization completed but didn't produce valid assignments. Please check your data and parameters.",
              easyClose = TRUE
            ))
          }
        }, error = function(e) {
          params$error_message <- e$message
          showModal(modalDialog(
            title = "Diversity Optimization Error",
            paste("Failed to generate allocation with new parameters:", e$message),
            easyClose = TRUE
          ))
        })
      } else if(model_num == 3) {
        # MODEL 3: SUBTEAM + SKILLS
        tryCatch({
          # Use the specific function for Model 3
          res <- run_multi_solution_optimization_subteam_skills(
            "survey_data.csv", 
            params$k_solutions,
            5.0  # Default score gap of 5%
          )
          
          # Check if we have valid results
          if (length(res$assignments_list) > 0 && !is.null(res$assignments_list[[res$best_solution_index]])) {
            # Store the results
            params$all_assignments <- res$assignments_list
            params$objective_values <- res$objective_values
            params$score_diffs <- res$score_diffs
            params$current_solution_idx <- res$best_solution_index
            params$final_assignments <- res$assignments_list[[res$best_solution_index]]
            params$preference_score <- res$objective_values[res$best_solution_index]
            params$error_message <- NULL
            
            if (!"individual_score" %in% names(params$final_assignments)) {
              # Add individual scores if needed
              data_list <- read_data_subteam_skills("survey_data.csv")
              params$final_assignments <- calculate_individual_scores(
                params$final_assignments,
                data_list$survey_data,
                data_list$topics,
                data_list$valid_subteams,
                data_list$pref_array
              )
            }
            
            # Hide the parameter panel
            shinyjs::hide("params_panel")
            
            # Show success message with score comparison
            score_diff <- params$preference_score - params$previous_score
            score_change_msg <- if(score_diff > 0) {
              paste("Preference score increased by", round(score_diff, 2), "points!")
            } else if(score_diff < 0) {
              paste("Preference score decreased by", round(abs(score_diff), 2), "points.")
            } else {
              "Preference score remained the same."
            }
            
            showModal(modalDialog(
              title = "Success",
              HTML(paste(
                "Subteam + Skills model ran successfully with new parameters!<br><br>",
                "<strong>", score_change_msg, "</strong>"
              )),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          } else {
            showModal(modalDialog(
              title = "Optimization Error",
              "The optimization completed but didn't produce valid assignments. Please check your data and parameters.",
              easyClose = TRUE
            ))
          }
        }, error = function(e) {
          params$error_message <- e$message
          showModal(modalDialog(
            title = "Optimization Error",
            paste("Failed to generate allocation with new parameters:", e$message),
            easyClose = TRUE
          ))
        })
      } else {
        # OTHER MODELS (1, 4)
        tryCatch({
          # Run the original optimization for other models
          res <- run_multi_solution_optimization(
            "survey_data.csv", 
            params$k_solutions,
            5.0  # Default score gap of 5%
          )
          
          # Check if we have valid results
          if (length(res$assignments_list) > 0 && !is.null(res$assignments_list[[res$best_solution_index]])) {
            # Store the results
            params$all_assignments <- res$assignments_list
            params$objective_values <- res$objective_values
            params$score_diffs <- res$score_diffs
            params$current_solution_idx <- res$best_solution_index
            params$final_assignments <- res$assignments_list[[res$best_solution_index]]
            params$preference_score <- res$objective_values[res$best_solution_index]
            params$error_message <- NULL
            
            if (!"individual_score" %in% names(params$final_assignments)) {
              # Add individual scores if needed
              data_list <- read_data("survey_data.csv")
              params$final_assignments <- calculate_individual_scores(
                params$final_assignments,
                data_list$survey_data,
                data_list$topics,
                data_list$valid_subteams,
                data_list$pref_array
              )
            }
            
            # Hide the parameter panel
            shinyjs::hide("params_panel")
            
            # Show success message with score comparison
            score_diff <- params$preference_score - params$previous_score
            score_change_msg <- if(score_diff > 0) {
              paste("Preference score increased by", round(score_diff, 2), "points!")
            } else if(score_diff < 0) {
              paste("Preference score decreased by", round(abs(score_diff), 2), "points.")
            } else {
              "Preference score remained the same."
            }
            
            showModal(modalDialog(
              title = "Success",
              HTML(paste(
                "Model ran successfully with new parameters!<br><br>",
                "<strong>", score_change_msg, "</strong>"
              )),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          } else {
            showModal(modalDialog(
              title = "Optimization Error",
              "The optimization completed but didn't produce valid assignments. Please check your data and parameters.",
              easyClose = TRUE
            ))
          }
        }, error = function(e) {
          params$error_message <- e$message
          showModal(modalDialog(
            title = "Optimization Error",
            paste("Failed to generate allocation with new parameters:", e$message),
            easyClose = TRUE
          ))
        })
      }
    })
  })
  
  #===========================================================================
  # MANUAL EDIT MODE
  #===========================================================================
  
  # Toggle edit mode
  observeEvent(input$toggle_edit_mode, {
  # Toggle the edit mode flag
  params$edit_mode <- !params$edit_mode
  
  if (params$edit_mode) {
    # ENTERING EDIT MODE
    # Store a copy of the original assignments
    params$edited_assignments <- params$final_assignments
    
    # Store the current score AS IS - don't recalculate it
    params$original_preference_score <- params$preference_score
    params$edited_preference_score <- params$preference_score
    
    message("ENTERING EDIT MODE - Original score: ", params$preference_score)
  } else {
    # EXITING EDIT MODE
    if (!is.null(params$edited_assignments)) {
      # Only recalculate if changes were made
      if (params$edited_preference_score != params$original_preference_score) {
        # Use the edited score directly - don't recalculate
        params$final_assignments <- params$edited_assignments
        params$preference_score <- params$edited_preference_score
        
        # Update the objective values array
        current_idx <- params$current_solution_idx
        if (!is.null(current_idx) && current_idx <= length(params$objective_values)) {
          params$objective_values[current_idx] <- params$edited_preference_score
          
          # Recalculate score differences
          best_score <- max(params$objective_values)
          params$score_diffs <- best_score - params$objective_values
        }
        
        message("Exiting edit mode with modified score: ", params$edited_preference_score)
      } else {
        message("Exiting edit mode without changes - keeping original score: ", params$original_preference_score)
      }
    }
  }
}) 

  # ------------------------------------------------------------------------------------------------
  # Handle project team change
  # ------------------------------------------------------------------------------------------------
  observeEvent(input$project_team_change, {
    req(params$edited_assignments, input$project_team_change)
    
    student_id <- input$project_team_change$student_id
    new_value <- input$project_team_change$new_value
    
    # Find index in the edited assignments
    row_idx <- which(params$edited_assignments$student_id == student_id)
    
    if (length(row_idx) > 0) {
      # Store old project team value for checking topic disappearance
      old_project_team <- params$edited_assignments$project_team[row_idx]
      
      # Extract topics from old and new project teams
      old_topic <- strsplit(old_project_team, "_team")[[1]][1]
      new_topic <- strsplit(new_value, "_team")[[1]][1]
      
      # Get the group_id for this student
      group_id <- params$edited_assignments$group_id[row_idx]
      
      # 1. Check if this change would separate student from their group
      # Find other students from the same group
      group_members <- which(params$edited_assignments$group_id == group_id)
      
      # If any members will be on a different team after this change
      will_separate_group <- FALSE
      if (length(group_members) > 1) {
        # If any other members will be on a different team/subteam
        other_members <- group_members[group_members != row_idx]
        current_teams <- unique(params$edited_assignments$project_team[other_members])
        current_subteams <- unique(params$edited_assignments$subteam[other_members])
        
        if (length(current_teams) == 1 && current_teams != new_value) {
          will_separate_group <- TRUE
          # Show warning
          showModal(modalDialog(
            title = "Group Separation Warning",
            HTML(paste0(
              "<div style='color: #f39c12;'><i class='fa fa-exclamation-triangle'></i> Warning:</div>",
              "<p>This change will separate student <strong>", student_id, "</strong> from their original group members.</p>",
              "<p>Other members are currently assigned to: <strong>", current_teams, "</strong></p>"
            )),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("confirm_team_change", "Proceed Anyway", 
                          class = "btn-warning")
            ),
            easyClose = TRUE
          ))
          # Store the pending change to apply after confirmation
          params$pending_change <- list(
            type = "project_team",
            student_id = student_id,
            new_value = new_value,
            row_idx = row_idx,
            old_score = params$edited_assignments$individual_score[row_idx]
          )
          return()  # Exit without making the change yet
        }
      }
      
      # 2. Check if this change would cause a topic to disappear
      # Count occurrences of the old topic
      old_topic_count <- sum(grepl(paste0("^", old_topic), params$edited_assignments$project_team))
      
      if (old_topic_count == 1 && old_topic != new_topic) {
        # Show warning - this is the last instance of this topic
        showModal(modalDialog(
          title = "Topic Removal Warning",
          HTML(paste0(
            "<div style='color: #e74c3c;'><i class='fa fa-exclamation-triangle'></i> Warning:</div>",
            "<p>This change will completely remove the topic <strong>", old_topic, "</strong> from the allocation.</p>",
            "<p>This is the last student/group assigned to this topic.</p>"
          )),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_topic_removal", "Proceed Anyway", 
                        class = "btn-danger")
          ),
          easyClose = TRUE
        ))
        # Store the pending change to apply after confirmation
        params$pending_change <- list(
          type = "project_team",
          student_id = student_id,
          new_value = new_value,
          row_idx = row_idx,
          old_score = params$edited_assignments$individual_score[row_idx]
        )
        return()  # Exit without making the change yet
      }
      
      # If no warnings triggered or user confirmed, proceed with the change
      # Get the old individual score before updating
      old_score <- params$edited_assignments$individual_score[row_idx]
      
      # Update the assignment
      params$edited_assignments$project_team[row_idx] <- new_value
      
      # Recalculate individual score for just this student
      data_list <- get_cached_data()
      
      # Extract topic from project_team
      topic <- tryCatch({
        if (!is.character(project_team) || is.na(project_team) || project_team == "") {
          return(NA)
        }
        parts <- strsplit(project_team, "_team")[[1]]
        if (length(parts) > 0) parts[1] else NA
      }, error = function(e) {
        warning("Invalid project_team format: ", project_team)
        return(NA)
      })
      
      if (!is.na(topic)) {
        subteam <- params$edited_assignments$subteam[row_idx]
        
        # Find group in survey data
        student_cols <- grep("Student_ID", names(data_list$survey_data), value = TRUE)
        group_indices <- sapply(1:nrow(data_list$survey_data), function(row_idx) {
          row_data <- data_list$survey_data[row_idx, student_cols, drop = FALSE]
          any(row_data == student_id, na.rm = TRUE)
        })
        
        if (any(group_indices)) {
          group_idx <- which(group_indices)[1]
          topic_idx <- match(topic, data_list$topics)
          subteam_idx <- match(subteam, data_list$valid_subteams)
          
          if (!is.na(topic_idx) && !is.na(subteam_idx)) {
            # Get preference score from the array
            new_score <- data_list$pref_array[group_idx, topic_idx, subteam_idx]
            
            # Update individual score
            params$edited_assignments$individual_score[row_idx] <- new_score
            
            # Update the edited total score (subtract old, add new)
            params$edited_preference_score <- params$edited_preference_score - old_score + new_score
            message("Updated score for student ", student_id, ": ", old_score, " -> ", new_score, ", new total: ", params$edited_preference_score)
          }
        }
      }
      
      params$last_score_calc_time <- Sys.time()  # Record when we calculated the score
    }
  })

  # Handle confirmation for team change despite group separation
  observeEvent(input$confirm_team_change, {
    req(params$pending_change, params$pending_change$type == "project_team")
    
    # Apply the pending change
    student_id <- params$pending_change$student_id
    new_value <- params$pending_change$new_value
    row_idx <- params$pending_change$row_idx
    old_score <- params$pending_change$old_score
    
    # Update the assignment
    params$edited_assignments$project_team[row_idx] <- new_value
    
    # Recalculate individual score - similar to your existing code
    data_list <- get_cached_data()
    
    # Extract topic from project_team
    topic <- tryCatch({
      parts <- strsplit(new_value, "_team")[[1]]
      if (length(parts) > 0) parts[1] else new_value
    }, error = function(e) {
      warning("Invalid project_team format: ", new_value)
      return(NA)
    })
    
    if (!is.na(topic)) {
      subteam <- params$edited_assignments$subteam[row_idx]
      
      # Find group in survey data
      student_cols <- grep("Student_ID", names(data_list$survey_data), value = TRUE)
      group_indices <- sapply(1:nrow(data_list$survey_data), function(row_idx) {
        row_data <- data_list$survey_data[row_idx, student_cols, drop = FALSE]
        any(row_data == student_id, na.rm = TRUE)
      })
      
      if (any(group_indices)) {
        group_idx <- which(group_indices)[1]
        topic_idx <- match(topic, data_list$topics)
        subteam_idx <- match(subteam, data_list$valid_subteams)
        
        if (!is.na(topic_idx) && !is.na(subteam_idx)) {
          # Get preference score from the array
          new_score <- data_list$pref_array[group_idx, topic_idx, subteam_idx]
          
          # Update individual score
          params$edited_assignments$individual_score[row_idx] <- new_score
          
          # Update the edited total score (subtract old, add new)
          params$edited_preference_score <- params$edited_preference_score - old_score + new_score
          message("Updated score for student ", student_id, ": ", old_score, " -> ", new_score, ", new total: ", params$edited_preference_score)
        }
      }
    }
    
    params$last_score_calc_time <- Sys.time()
    params$pending_change <- NULL  # Clear the pending change
    removeModal()
  })

  # Handle confirmation for team change despite topic removal
  observeEvent(input$confirm_topic_removal, {
    req(params$pending_change, params$pending_change$type == "project_team")
    
    # Apply the pending change - same code as above
    student_id <- params$pending_change$student_id
    new_value <- params$pending_change$new_value
    row_idx <- params$pending_change$row_idx
    old_score <- params$pending_change$old_score
    
    # Update the assignment
    params$edited_assignments$project_team[row_idx] <- new_value
    
    # Recalculate individual score - similar to your existing code
    data_list <- get_cached_data()
    
    # Extract topic from project_team
    topic <- tryCatch({
      parts <- strsplit(new_value, "_team")[[1]]
      if (length(parts) > 0) parts[1] else new_value
    }, error = function(e) {
      warning("Invalid project_team format: ", new_value)
      return(NA)
    })
    
    if (!is.na(topic)) {
      subteam <- params$edited_assignments$subteam[row_idx]
      
      # Find group in survey data
      student_cols <- grep("Student_ID", names(data_list$survey_data), value = TRUE)
      group_indices <- sapply(1:nrow(data_list$survey_data), function(row_idx) {
        row_data <- data_list$survey_data[row_idx, student_cols, drop = FALSE]
        any(row_data == student_id, na.rm = TRUE)
      })
      
      if (any(group_indices)) {
        group_idx <- which(group_indices)[1]
        topic_idx <- match(topic, data_list$topics)
        subteam_idx <- match(subteam, data_list$valid_subteams)
        
        if (!is.na(topic_idx) && !is.na(subteam_idx)) {
          # Get preference score from the array
          new_score <- data_list$pref_array[group_idx, topic_idx, subteam_idx]
          
          # Update individual score
          params$edited_assignments$individual_score[row_idx] <- new_score
          
          # Update the edited total score (subtract old, add new)
          params$edited_preference_score <- params$edited_preference_score - old_score + new_score
          message("Updated score for student ", student_id, ": ", old_score, " -> ", new_score, ", new total: ", params$edited_preference_score)
        }
      }
    }
    
    params$last_score_calc_time <- Sys.time()
    params$pending_change <- NULL  # Clear the pending change
    removeModal()
  })


  # ----------------------------------------------------------------------------------
  # Handle subteam change
  # ----------------------------------------------------------------------------------
  observeEvent(input$subteam_change, {
    req(params$edited_assignments, input$subteam_change)
    
    student_id <- input$subteam_change$student_id
    new_value <- input$subteam_change$new_value
    
    # Find index in the edited assignments
    row_idx <- which(params$edited_assignments$student_id == student_id)
    
    if (length(row_idx) > 0) {
      # Get the current subteam and project team
      current_subteam <- params$edited_assignments$subteam[row_idx]
      current_project_team <- params$edited_assignments$project_team[row_idx]
      
      # Get the group_id for this student
      group_id <- params$edited_assignments$group_id[row_idx]
      
      # 1. Check if this change would separate student from their group
      # Find other students from the same group
      group_members <- which(params$edited_assignments$group_id == group_id)
      
      # If any members will be on a different subteam after this change
      will_separate_group <- FALSE
      if (length(group_members) > 1) {
        # If any other members will be on a different team/subteam
        other_members <- group_members[group_members != row_idx]
        same_team_members <- other_members[params$edited_assignments$project_team[other_members] == current_project_team]
        
        if (length(same_team_members) > 0) {
          current_subteams <- unique(params$edited_assignments$subteam[same_team_members])
          
          if (length(current_subteams) == 1 && current_subteams != new_value) {
            will_separate_group <- TRUE
            # Show warning
            showModal(modalDialog(
              title = "Group Separation Warning",
              HTML(paste0(
                "<div style='color: #f39c12;'><i class='fa fa-exclamation-triangle'></i> Warning:</div>",
                "<p>This change will separate student <strong>", student_id, "</strong> from their original group members.</p>",
                "<p>Other members are currently assigned to subteam: <strong>", current_subteams, "</strong></p>"
              )),
              footer = tagList(
                modalButton("Cancel"),
                actionButton("confirm_subteam_change", "Proceed Anyway", 
                            class = "btn-warning")
              ),
              easyClose = TRUE
            ))
            # Store the pending change to apply after confirmation
            params$pending_change <- list(
              type = "subteam",
              student_id = student_id,
              new_value = new_value,
              row_idx = row_idx,
              old_score = params$edited_assignments$individual_score[row_idx]
            )
            return()  # Exit without making the change yet
          }
        }
      }
      
      # No warnings needed for subteam changes regarding topic disappearance
      # (changing subteam won't remove a topic)
      
      # Get the old individual score before updating
      old_score <- params$edited_assignments$individual_score[row_idx]
      
      # Update the assignment
      params$edited_assignments$subteam[row_idx] <- new_value
      
      # Recalculate individual score for just this student
      data_list <- get_cached_data()
      
      # Get project team and extract topic
      project_team <- params$edited_assignments$project_team[row_idx]
      topic <- tryCatch({
        parts <- strsplit(project_team, "_team")[[1]]
        if (length(parts) > 0) parts[1] else project_team
      }, error = function(e) {
        warning("Invalid project_team format: ", project_team)
        return(NA)
      })
      
      if (!is.na(topic)) {
        # Find group in survey data
        student_cols <- grep("Student_ID", names(data_list$survey_data), value = TRUE)
        group_indices <- sapply(1:nrow(data_list$survey_data), function(row_idx) {
          row_data <- data_list$survey_data[row_idx, student_cols, drop = FALSE]
          any(row_data == student_id, na.rm = TRUE)
        })
        
        if (any(group_indices)) {
          group_idx <- which(group_indices)[1]
          topic_idx <- match(topic, data_list$topics)
          subteam_idx <- match(new_value, data_list$valid_subteams)
          
          if (!is.na(topic_idx) && !is.na(subteam_idx)) {
            # Get preference score from the array
            new_score <- data_list$pref_array[group_idx, topic_idx, subteam_idx]
            
            # Update individual score
            params$edited_assignments$individual_score[row_idx] <- new_score
            
            # Update the edited total score (subtract old, add new)
            params$edited_preference_score <- params$edited_preference_score - old_score + new_score
            message("Updated score for student ", student_id, ": ", old_score, " -> ", new_score, ", new total: ", params$edited_preference_score)
          }
        }
      }
      
      params$last_score_calc_time <- Sys.time()  # Record when we calculated the score
    }
  })

  # Handle confirmation for subteam change despite group separation
  observeEvent(input$confirm_subteam_change, {
    req(params$pending_change, params$pending_change$type == "subteam")
    
    # Apply the pending change
    student_id <- params$pending_change$student_id
    new_value <- params$pending_change$new_value
    row_idx <- params$pending_change$row_idx
    old_score <- params$pending_change$old_score
    
    # Update the assignment
    params$edited_assignments$subteam[row_idx] <- new_value
    
    # Recalculate individual score for just this student
    data_list <- get_cached_data()
    
    # Get project team and extract topic
    project_team <- params$edited_assignments$project_team[row_idx]
    topic <- tryCatch({
      parts <- strsplit(project_team, "_team")[[1]]
      if (length(parts) > 0) parts[1] else project_team
    }, error = function(e) {
      warning("Invalid project_team format: ", project_team)
      return(NA)
    })
    
    if (!is.na(topic)) {
      # Find group in survey data
      student_cols <- grep("Student_ID", names(data_list$survey_data), value = TRUE)
      group_indices <- sapply(1:nrow(data_list$survey_data), function(row_idx) {
        row_data <- data_list$survey_data[row_idx, student_cols, drop = FALSE]
        any(row_data == student_id, na.rm = TRUE)
      })
      
      if (any(group_indices)) {
        group_idx <- which(group_indices)[1]
        topic_idx <- match(topic, data_list$topics)
        subteam_idx <- match(new_value, data_list$valid_subteams)
        
        if (!is.na(topic_idx) && !is.na(subteam_idx)) {
          # Get preference score from the array
          new_score <- data_list$pref_array[group_idx, topic_idx, subteam_idx]
          
          # Update individual score
          params$edited_assignments$individual_score[row_idx] <- new_score
          
          # Update the edited total score (subtract old, add new)
          params$edited_preference_score <- params$edited_preference_score - old_score + new_score
          message("Updated score for student ", student_id, ": ", old_score, " -> ", new_score, ", new total: ", params$edited_preference_score)
        }
      }
    }
    
    params$last_score_calc_time <- Sys.time()
    params$pending_change <- NULL  # Clear the pending change
    removeModal()
  })




# ------------------------------------------------------------------------------------------------
# END
# ------------------------------------------------------------------------------------------------

}