#===========================================================================
# TABLE OF CONTENTS
#===========================================================================
# 1. INITIALIZATION AND SETUP
#    - Initial Source Files
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




# Access Initial Source Files
source("../../R/optimization_model.R")
source("../../R/import_data.R")
source("../../R/manual_adjustments.R")

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
    last_score_calc_time = NULL      # Timestamp of last score calculation
  )

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
  
  #===========================================================================
  # UI RENDERING
  #===========================================================================
  
  # Render dynamic UI based on current page
  output$main_ui <- renderUI({
    switch(
      current_page(),
      "login"         = login_ui,
      "project_setup" = project_setup_ui,
      "csv_upload"    = csv_upload_ui,
      "result"        = result_ui
    )
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
          card_style
        ),
        onclick = paste0("Shiny.setInputValue('select_solution', ", idx, ")"),
        
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
      )
    })
    
    # Wrap cards in a container
    div(
      style = "margin-top: 15px;",
      solution_cards
    )
  })
  
  #---------------------------------------------------------------------------
  # Data Tables for Results
  #---------------------------------------------------------------------------
  
  # Standard allocation table (view mode)
  output$allocation_table <- DT::renderDataTable({
    req(params$final_assignments)
    
    # Ensure individual scores are calculated
    if (!"individual_score" %in% names(params$final_assignments)) {
      # Calculate individual scores
      data_list <- read_data("survey_data.csv")
      params$final_assignments <- calculate_individual_scores(
        params$final_assignments,
        data_list$survey_data,
        data_list$topics,
        data_list$valid_subteams,
        data_list$pref_array
      )
    }
    
    # Prepare data for display
    display_data <- params$final_assignments %>%
      select(student_id, project_team, subteam, solution_number, individual_score)
    
    # Create a formatted DT table
    DT::datatable(
      display_data,
      rownames = FALSE,
      colnames = c(
        "Student ID", 
        "Project Team", 
        "Subteam", 
        "Solution Number", 
        "Preference Score"
      ),
      options = list(
        pageLength = 25,
        dom = 'ftip',
        order = list(list(1, 'asc'), list(2, 'asc')), # Sort by project team then subteam
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
          )
        )
      ),
      selection = 'none'
    )
  })
  
  # Editable allocation table (edit mode)
  output$editable_allocation_table <- DT::renderDataTable({
    req(params$edited_assignments)
    
    # Get unique project teams and subteams for dropdowns
    unique_teams <- unique(params$edited_assignments$project_team)
    unique_subteams <- unique(params$edited_assignments$subteam)
    
    # Create a display data frame with individual scores instead of solution score
    display_data <- params$edited_assignments
    
    # If individual_score column doesn't exist yet, try to calculate it
    if (!"individual_score" %in% names(display_data)) {
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
        # If there's an error, just use the solution score for all
        display_data$individual_score <- display_data$solution_score
        return(display_data)
      })
    }
    
    # Create a datatable with editable cells for project_team and subteam
    DT::datatable(
      display_data %>% select(student_id, project_team, subteam, solution_number, individual_score),
      rownames = FALSE,
      colnames = c(
        "Student ID", 
        "Project Team", 
        "Subteam", 
        "Solution Number", 
        "Preference Score"
      ),
      editable = list(
        target = "cell", 
        disable = list(columns = c(0, 3, 4)) # Disable editing for student_id, solution_number, preference_score
      ),
      options = list(
        pageLength = 25,
        dom = 'frtip',
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
                  var color = score > 7 ? '#2ecc71' : (score > 3 ? '#f39c12' : '#e74c3c');
                  return '<span style=\"color: ' + color + '; font-weight: 500;\">' + score.toFixed(1) + '</span>';
                }
                return data;
              }
            ")
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
    # Validate inputs first
    if (is.na(input$c_team) || is.na(input$b_subteam) || is.na(input$x_topic_teams) || is.na(input$k_solutions)) {
      showModal(modalDialog(
        title = "Error",
        "Please enter valid values for all fields.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # Save numeric inputs to user_inputs.csv
    # so read_project_data() can read them
    data_to_save <- data.frame(
      c_team        = input$c_team,
      b_subteam     = input$b_subteam,
      x_topic_teams = input$x_topic_teams
    )
    
    # Store values in reactive values for later use
    params$c_team <- as.numeric(input$c_team)
    params$b_subteam <- as.numeric(input$b_subteam)
    params$x_topic_teams <- as.numeric(input$x_topic_teams)
    params$k_solutions <- as.numeric(input$k_solutions)
    
    # Save to CSV
    tryCatch({
      write.table(
        data_to_save,
        file = "user_inputs.csv",
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,  # Changed to FALSE as expected by read_project_data
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
      
      # Ensure parameters are numeric before calling optimization
      c_team <- as.numeric(params$c_team)
      b_subteam <- as.numeric(params$b_subteam)
      x_topic_teams <- as.numeric(params$x_topic_teams)
      k_solutions <- as.numeric(params$k_solutions)
      
      # Print debug info to console
      message("Optimization parameters:")
      message("c_team = ", c_team, " (", class(c_team), ")")
      message("b_subteam = ", b_subteam, " (", class(b_subteam), ")")
      message("x_topic_teams = ", x_topic_teams, " (", class(x_topic_teams), ")")
      message("k_solutions = ", k_solutions, " (", class(k_solutions), ")")
      
      # Run the optimization with error handling
      tryCatch({
        # Pass the path to the uploaded CSV file
        res <- run_multi_solution_optimization("survey_data.csv",
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
        params$error_message <- e$message
        message("Optimization error: ", e$message)  # Log for debugging
        showModal(modalDialog(
          title = "Optimization Error",
          paste("Failed to generate allocation:", e$message),
          easyClose = TRUE
        ))
      })
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
    # Validate inputs
    params$c_team <- as.numeric(input$new_c_team)
    params$b_subteam <- as.numeric(input$new_b_subteam)
    params$x_topic_teams <- as.numeric(input$new_x_topic_teams)
    
    # Log for debugging
    message("New parameters for re-run:")
    message("c_team = ", params$c_team, " (", class(params$c_team), ")")
    message("b_subteam = ", params$b_subteam, " (", class(params$b_subteam), ")")
    message("x_topic_teams = ", params$x_topic_teams, " (", class(params$x_topic_teams), ")")
    
    if (is.na(input$new_c_team) || is.na(input$new_b_subteam) || is.na(input$new_x_topic_teams)) {
      showModal(modalDialog(
        title = "Error",
        "Please enter valid values for all fields.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # Save new parameters
    data_to_save <- data.frame(
      c_team        = input$new_c_team,
      b_subteam     = input$new_b_subteam,
      x_topic_teams = input$new_x_topic_teams
    )
    
    # Update reactive values
    params$c_team <- as.numeric(input$new_c_team)
    params$b_subteam <- as.numeric(input$new_b_subteam)
    params$x_topic_teams <- as.numeric(input$new_x_topic_teams)
    
    # Save to CSV
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
    
    # Store the current score as previous before running again
    params$previous_score <- params$preference_score
    
    # Show progress while running optimization
    withProgress(message = "Running optimization with new parameters...", {
      # Run the optimization with error handling
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
          params$error_message <- NULL
          
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
    })
  })
  
  #===========================================================================
  # MANUAL EDIT MODE
  #===========================================================================
  
  # Toggle edit mode
  observeEvent(input$toggle_edit_mode, {
    params$edit_mode <- !params$edit_mode
    
    if (params$edit_mode) {
      # When entering edit mode, store the original assignments
      params$edited_assignments <- params$final_assignments
      # Calculate the original score using the SAME function
      params$original_preference_score <- calculate_preference_score(params$final_assignments)
      params$edited_preference_score <- params$original_preference_score
      message("ENTERING EDIT MODE - Original score: ", params$original_preference_score)
    } else {
      # When exiting edit mode, update the final assignments with edited version
      if (!is.null(params$edited_assignments)) {
        # Recalculate the final score one more time to ensure accuracy
        final_score <- calculate_preference_score(params$edited_assignments)
        
        params$final_assignments <- params$edited_assignments
        params$preference_score <- final_score
        params$edited_preference_score <- final_score
        
        message("Exiting edit mode with final score: ", final_score)
      }
    }
  })
  
  # Handle project team change
  observeEvent(input$project_team_change, {
    req(params$edited_assignments, input$project_team_change)
    
    student_id <- input$project_team_change$student_id
    new_value <- input$project_team_change$new_value
    row_idx <- which(params$edited_assignments$student_id == student_id)
    
    if (length(row_idx) > 0) {
      # Update the assignment
      params$edited_assignments$project_team[row_idx] <- new_value
      
      # Recalculate individual score for just this student
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
            student_score <- data_list$pref_array[group_idx, topic_idx, subteam_idx]
            params$edited_assignments$individual_score[row_idx] <- student_score
          }
        }
      }
      
      # Recalculate overall preference score
      params$edited_preference_score <- calculate_preference_score(params$edited_assignments)
      params$last_score_calc_time <- Sys.time()  # Record when we calculated the score
    }
  })
  
  # Handle subteam change
  observeEvent(input$subteam_change, {
    req(params$edited_assignments, input$subteam_change)
    
    student_id <- input$subteam_change$student_id
    new_value <- input$subteam_change$new_value
    row_idx <- which(params$edited_assignments$student_id == student_id)
    
    if (length(row_idx) > 0) {
      # Update the assignment
      params$edited_assignments$subteam[row_idx] <- new_value
      
      # Recalculate individual score for just this student
      data_list <- read_data("survey_data.csv")
      
      # Get project team and extract topic
      project_team <- params$edited_assignments$project_team[row_idx]
      topic <- strsplit(project_team, "_team")[[1]][1]
      
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
          student_score <- data_list$pref_array[group_idx, topic_idx, subteam_idx]
          params$edited_assignments$individual_score[row_idx] <- student_score
        }
      }
      
      # Recalculate overall preference score
      params$edited_preference_score <- calculate_preference_score(params$edited_assignments)
    }
  })
  
  # Update preference score for manual edits
  observe({
    # Only update when explicitly in edit mode and after changes
    if (params$edit_mode && !is.null(params$edited_assignments)) {
      # Add isolation to prevent too many recalculations
      isolate({
        # Only recalculate if we haven't just done so from another event
        current_time <- Sys.time()
        last_calc_time <- params$last_score_calc_time %||% as.POSIXct("1970-01-01")
        
        if (difftime(current_time, last_calc_time, units="secs") > 1) {
          params$edited_preference_score <- calculate_preference_score(params$edited_assignments)
          params$last_score_calc_time <- current_time
          message("Score recalculated at: ", format(current_time))
        }
      })
    }
  })

}
